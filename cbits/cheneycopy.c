#include "Rts.h"
#include "RtsAPI.h"

#include "stg/Regs.h"

#include <string.h>

extern void *stgMallocBytes(int, char *);
extern void stgFree(void*);

typedef struct bdescr_list_ {
    bdescr *bd;
    struct bdescr_list_ *link;
} bdescr_list;

static bdescr *allocate_in_region(CapabilityPublic *cap) {
    // allocate() version

    bdescr *bd = cap->r.rCurrentNursery->link;

    if (bd == NULL) {
        bd = allocBlock_lock();
        cap->r.rNursery->n_blocks++;
        initBdescr(bd, g0, g0);
        bd->flags = 0;
    } else {
        bd->free = bd->start;
        cap->r.rCurrentNursery->link = bd->link;
        if (bd->link != NULL) {
            bd->link->u.back = cap->r.rCurrentNursery;
        }
    }
    dbl_link_onto(bd, &cap->r.rNursery->blocks);
    return bd;
}

        /*
static bdescr *allocate_in_region(CapabilityPublic *cap) {
    // pinned version, HOWEVER this one needs more work
    // to turn it into bytestrings...

    StgPtr given = allocatePinned((Capability*)cap, stg_max(sizeW, 8000));
    bd = Bdescr(given);
    ASSERT(bd->start == given);
    bd->free = bd->start;
    return bd;
}
        */

static StgPtr
// bd + tail == more register pressure but avoid a deref in the fast loop
allocate_loop (CapabilityPublic *cap, bdescr **dest, bdescr_list **tail, StgWord sizeW)
{
    StgWord next_size;
    StgPtr at;
    bdescr *bd = *dest;

    ASSERT((*tail)->bd == bd);
    ASSERT((*tail)->link == NULL);

    if (bd->free + sizeW > bd->start + BLOCK_SIZE_W * bd->blocks) {
        // 64k chunk size
        bdescr_list *old_tail = *tail;
        bd = allocate_in_region(cap);
        bdescr_list *new_tail = stgMallocBytes(sizeof(bdescr_list), "bdescr_list");
        *tail = new_tail;
        old_tail->link = new_tail;
        new_tail->bd = bd;
        new_tail->link = NULL;
        *dest = new_tail->bd;
    }

    at = bd->free;
    bd->free += sizeW;
    return at;
}

static void
unroll_memcpy(StgPtr to, StgPtr from, StgWord size)
{
    for (; size > 0; size--)
        *(to++) = *(from++);
}

static void
copy_tag (CapabilityPublic *cap, bdescr **dest, bdescr_list **tail, StgClosure **p, StgClosure *from, StgWord tag)
{
    StgPtr to;
    StgWord sizeW;

    sizeW = closure_sizeW(from);

    to = allocate_loop(cap, dest, tail, sizeW);

    // unroll memcpy for small sizes because we can
    // benefit of known alignment
    // (32 extracted from my magic hat)
    if (sizeW < 32)
        unroll_memcpy(to, (StgPtr)from, sizeW);
    else
        memcpy(to, from, sizeW * sizeof(StgWord));

    *p = TAG_CLOSURE(tag, (StgClosure*)to);
}

static void
simple_evacuate (CapabilityPublic *cap, bdescr **dest, bdescr_list **tail, StgClosure **p)
{
    StgWord tag;
    StgClosure *from;
    void *already;

    from = *p;
    tag = GET_CLOSURE_TAG(from);
    from = UNTAG_CLOSURE(from);

    switch (get_itbl(from)->type) {
    case BLACKHOLE:
        // If tag == 0, the indirectee is the TSO that claimed the tag
        //
        // Not useful and not NFData
        from = ((StgInd*)from)->indirectee;
        if (GET_CLOSURE_TAG(from) == 0) {
            barf("Claimed but not updated BLACKHOLE, not normal form");
        }

        *p = from;
        return simple_evacuate(cap, dest, tail, p);

    case IND:
    case IND_STATIC:
        // follow chains of indirections, don't evacuate them
        from = ((StgInd*)from)->indirectee;
        *p = from;
        // Evac.c uses a goto, but let's rely on a smart compiler
        // and get readable code instead
        return simple_evacuate(cap, dest, tail, p);

    default:
        copy_tag(cap, dest, tail, p, from, tag);
    }
}

static void
simple_scavenge_mut_arr_ptrs (CapabilityPublic       *cap,
                              bdescr **dest,
                              bdescr_list **tail,
                              StgMutArrPtrs    *a)
{
    StgPtr p, q;

    p = (StgPtr)&a->payload[0];
    q = (StgPtr)&a->payload[a->ptrs];
    for (; p < q; p++) {
        simple_evacuate(cap, dest, tail, (StgClosure**)p);
    }
}

#ifdef DEBUG
extern void        printClosure    ( StgClosure *obj );
#endif

static void
simple_scavenge_block (CapabilityPublic *cap, bdescr *bd, bdescr **dest, bdescr_list **tail, StgPtr p)
{
    StgInfoTable *info;

    while (p < bd->free) {
        ASSERT (LOOKS_LIKE_CLOSURE_PTR(p));
        info = get_itbl((StgClosure*)p);

        switch (info->type) {
        case CONSTR_1_0:
            simple_evacuate(cap, dest, tail, &((StgClosure*)p)->payload[0]);
        case CONSTR_0_1:
            p += sizeofW(StgClosure) + 1;
            break;

        case CONSTR_2_0:
            simple_evacuate(cap, dest, tail, &((StgClosure*)p)->payload[1]);
        case CONSTR_1_1:
            simple_evacuate(cap, dest, tail, &((StgClosure*)p)->payload[0]);
        case CONSTR_0_2:
            p += sizeofW(StgClosure) + 2;
            break;

        case CONSTR:
        case PRIM:
        case CONSTR_NOCAF_STATIC:
        case CONSTR_STATIC:
        {
            StgPtr end;

            end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
            for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
                simple_evacuate(cap, dest, tail, (StgClosure **)p);
            }
            p += info->layout.payload.nptrs;
            break;
        }

        case ARR_WORDS:
            p += arr_words_sizeW((StgArrWords*)p);
            break;

        case MUT_ARR_PTRS_FROZEN:
        case MUT_ARR_PTRS_FROZEN0:
            simple_scavenge_mut_arr_ptrs(cap, dest, tail, (StgMutArrPtrs*)p);
            p += mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
            break;

        case SMALL_MUT_ARR_PTRS_FROZEN:
        case SMALL_MUT_ARR_PTRS_FROZEN0:
        {
            nat i;
            StgSmallMutArrPtrs *arr = (StgSmallMutArrPtrs*)p;

            for (i = 0; i < arr->ptrs; i++)
                simple_evacuate(cap, dest, tail, &arr->payload[i]);

            p += sizeofW(StgSmallMutArrPtrs) + arr->ptrs;
            break;
        }

        case IND:
        case BLACKHOLE:
        case IND_STATIC:
            // They get shortcircuited by simple_evaluate()
            barf("IND/BLACKHOLE in Compact");

        default:
#ifdef DEBUG
            printClosure((StgClosure *)p);
#endif
            barf("Invalid non-NFData closure");
        }
    }
}

static W_
scavenge_loop (CapabilityPublic *cap, bdescr_list *cur, bdescr **dest, bdescr_list **tail)
{
    W_ chunks = 0;
    while (cur != NULL) {
        chunks++;
        bdescr *bd = cur->bd;
        simple_scavenge_block(cap, bd, dest, tail, bd->start);
        cur = cur->link;
    }
    return chunks;
}

StgPtr
cheneycopy (CapabilityPublic *cap, StgClosure *p)
{
    StgClosure *tagged_root;
    tagged_root = p;

    // TODO pessimal behavior if first object doesn't fit
    bdescr *bd = allocate_in_region(cap);
    bdescr_list *cur = (bdescr_list*)stgMallocBytes(sizeof(bdescr_list), "bdescr_list");
    cur->bd = bd;
    cur->link = NULL;
    bdescr_list *tail = cur;
    bdescr *dest = bd;

    simple_evacuate(cap, &dest, &tail, &tagged_root);
    W_ chunks = scavenge_loop(cap, cur, &dest, &tail);

    return (StgPtr)tagged_root;
}
