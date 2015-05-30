#include "Rts.h"
#include "RtsAPI.h"

#include "stg/Regs.h"

#include <string.h>

static StgPtr
allocate_loop (CapabilityPublic *cap, bdescr *bd, StgWord sizeW)
{
    StgWord next_size;
    StgPtr at;
    ASSERT(!(bd->flags & BF_PINNED));

    // TODO: loop
    if (bd->free + sizeW > bd->start + BLOCK_SIZE_W * bd->blocks) {
        barf("You tried to copy something bigger than a megablock!  (not implemented yet)");
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
copy_tag (CapabilityPublic *cap, bdescr *bd, StgClosure **p, StgClosure *from, StgWord tag)
{
    StgPtr to;
    StgWord sizeW;

    sizeW = closure_sizeW(from);

    to = allocate_loop(cap, bd, sizeW);
    ASSERT(!(Bdescr(to)->flags & BF_PINNED));

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
simple_evacuate (CapabilityPublic *cap, bdescr *bd, StgClosure **p)
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
        return simple_evacuate(cap, bd, p);

    case IND:
    case IND_STATIC:
        // follow chains of indirections, don't evacuate them
        from = ((StgInd*)from)->indirectee;
        *p = from;
        // Evac.c uses a goto, but let's rely on a smart compiler
        // and get readable code instead
        return simple_evacuate(cap, bd, p);

    default:
        copy_tag(cap, bd, p, from, tag);
    }
}

static void
simple_scavenge_mut_arr_ptrs (CapabilityPublic       *cap,
                              bdescr *bd,
                              StgMutArrPtrs    *a)
{
    StgPtr p, q;

    p = (StgPtr)&a->payload[0];
    q = (StgPtr)&a->payload[a->ptrs];
    for (; p < q; p++) {
        simple_evacuate(cap, bd, (StgClosure**)p);
    }
}

static void
simple_scavenge_block (CapabilityPublic *cap, bdescr *bd, StgPtr p)
{
    StgInfoTable *info;

    while (p < bd->free) {
        ASSERT (LOOKS_LIKE_CLOSURE_PTR(p));
        info = get_itbl((StgClosure*)p);

        switch (info->type) {
        case CONSTR_1_0:
            simple_evacuate(cap, bd, &((StgClosure*)p)->payload[0]);
        case CONSTR_0_1:
            p += sizeofW(StgClosure) + 1;
            break;

        case CONSTR_2_0:
            simple_evacuate(cap, bd, &((StgClosure*)p)->payload[1]);
        case CONSTR_1_1:
            simple_evacuate(cap, bd, &((StgClosure*)p)->payload[0]);
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
                simple_evacuate(cap, bd, (StgClosure **)p);
            }
            p += info->layout.payload.nptrs;
            break;
        }

        case ARR_WORDS:
            p += arr_words_sizeW((StgArrWords*)p);
            break;

        case MUT_ARR_PTRS_FROZEN:
        case MUT_ARR_PTRS_FROZEN0:
            simple_scavenge_mut_arr_ptrs(cap, bd, (StgMutArrPtrs*)p);
            p += mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
            break;

#ifdef SMALL_MUT_ARR_PTRS_FROZEN
        case SMALL_MUT_ARR_PTRS_FROZEN:
        case SMALL_MUT_ARR_PTRS_FROZEN0:
        {
            nat i;
            StgSmallMutArrPtrs *arr = (StgSmallMutArrPtrs*)p;

            for (i = 0; i < arr->ptrs; i++)
                simple_evacuate(cap, bd, &arr->payload[i]);

            p += sizeofW(StgSmallMutArrPtrs) + arr->ptrs;
            break;
        }
#endif

        case IND:
        case BLACKHOLE:
        case IND_STATIC:
            // They get shortcircuited by simple_evaluate()
            barf("IND/BLACKHOLE in Compact");

        default:
            barf("Invalid non-NFData closure in Compact\n");
        }
    }
}


static void
scavenge_loop (CapabilityPublic *cap, bdescr *bd, StgPtr p)
{
    // Scavenge the first block
    simple_scavenge_block(cap, bd, p);

    // TODO: loop
}

StgPtr
cheneycopy (CapabilityPublic *cap, StgClosure *p)
{
    StgClosure *tagged_root;
    tagged_root = p;
    bdescr *bd;

    // pessimal case!
    W_ blocks = BLOCKS_PER_MBLOCK-1;
    bd = allocGroup_lock(blocks);
    cap->r.rNursery->n_blocks += blocks;
    initBdescr(bd, g0, g0);
    bd->flags = 0;
    dbl_link_onto(bd, &cap->r.rNursery->blocks);

    bd->free = bd->start;

    simple_evacuate(cap, bd, &tagged_root);
    scavenge_loop(cap, bd, (P_)UNTAG_CLOSURE(tagged_root));
    return (StgPtr)tagged_root;
}
