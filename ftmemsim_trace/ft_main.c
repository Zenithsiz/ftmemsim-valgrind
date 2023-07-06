
/*--------------------------------------------------------------------*/
/*--- FtMemSim-Trace: Program tracer for `ftmemsim`.     ft_main.c ---*/
/*--------------------------------------------------------------------*/

#include "pub_tool_basics.h"
#include "pub_tool_clientstate.h"
#include "pub_tool_debuginfo.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcbase.h"
#include "pub_tool_libcfile.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_libcproc.h"
#include "pub_tool_machine.h" // VG_(fnptr_to_fnentry)
#include "pub_tool_mallocfree.h"
#include "pub_tool_options.h"
#include "pub_tool_oset.h"
#include "pub_tool_tooliface.h"
#include "pub_tool_xarray.h"

#include "ft_arch.c"
#include "ft_arch.h"
#include "ft_sim.c"

static Int min_line_size = 0; /* min of L1 and LL cache line sizes */

static cache_t clo_I1_cache = UNDEFINED_CACHE;
static cache_t clo_D1_cache = UNDEFINED_CACHE;
static cache_t clo_LL_cache = UNDEFINED_CACHE;

static void ft_post_clo_init(void)
{
   cache_t I1c, D1c, LLc;

   VG_(post_clo_init_configure_caches)(&I1c, &D1c, &LLc, &clo_I1_cache,
                                       &clo_D1_cache, &clo_LL_cache);

   // min_line_size is used to make sure that we never feed
   // accesses to the simulator straddling more than two
   // cache lines at any cache level
   min_line_size =
      (I1c.line_size < D1c.line_size) ? I1c.line_size : D1c.line_size;
   min_line_size =
      (LLc.line_size < min_line_size) ? LLc.line_size : min_line_size;

   Int largest_load_or_store_size =
      VG_(machine_get_size_of_largest_guest_register)();
   if (min_line_size < largest_load_or_store_size) {
      /* We can't continue, because the cache simulation might
         straddle more than 2 lines, and it will assert.  So let's
         just stop before we start. */
      VG_(umsg)("cannot continue: the minimum line size (%d)\n",
                (Int)min_line_size);
      VG_(umsg)(
         "  must be equal to or larger than the maximum register size (%d)\n",
         largest_load_or_store_size);
      VG_(umsg)("  but it is not.  Exiting now.\n");
      VG_(exit)(1);
   }

   cachesim_initcaches(I1c, D1c, LLc);
}

typedef enum { Event_Ir, Event_Dr, Event_Dw, Event_Dm } EventKind;

typedef struct {
   IRExpr*   addr;
   EventKind ekind;
   Int       size;
   IRExpr*   guard; /* :: Ity_I1, or NULL=="always True" */
} Event;

/* Up to this many unnotified events are allowed.  Must be at least two,
   so that reads and writes to the same address can be merged into a modify.
   Beyond that, larger numbers just potentially induce more spilling due to
   extending live ranges of address temporaries. */
#define N_EVENTS 4

/* Maintain an ordered list of memory events which are outstanding, in
   the sense that no IR has yet been generated to do the relevant
   helper calls.  The SB is scanned top to bottom and memory events
   are added to the end of the list, merging with the most recent
   notified event where possible (Dw immediately following Dr and
   having the same size and EA can be merged).

   This merging is done so that for architectures which have
   load-op-store instructions (x86, amd64), the instr is treated as if
   it makes just one memory reference (a modify), rather than two (a
   read followed by a write at the same address).

   At various points the list will need to be flushed, that is, IR
   generated from it.  That must happen before any possible exit from
   the block (the end, or an IRStmt_Exit).  Flushing also takes place
   when there is no space to add a new event, and before entering a
   RMW (read-modify-write) section on processors supporting LL/SC.

   If we require the simulation statistics to be up to date with
   respect to possible memory exceptions, then the list would have to
   be flushed before each memory reference.  That's a pain so we don't
   bother.

   Flushing the list consists of walking it start to end and emitting
   instrumentation IR for each event, in the order in which they
   appear. */

static Event events[N_EVENTS];
static Int   events_used = 0;

// Skips the cache for all traces
static Bool skip_cache = False;

// Tracing instructions
static void trace_instr(Addr addr, SizeT size)
{

   if (skip_cache || cachesim_ref_is_miss(&I1, addr, size)) {
      if (skip_cache || cachesim_ref_is_miss(&LL, addr, size)) {
         VG_(printf)("I %08lx\n", addr);
      }
   }
}
static void trace_store(Addr addr, SizeT size)
{
   if (skip_cache || cachesim_ref_is_miss(&D1, addr, size)) {
      if (skip_cache || cachesim_ref_is_miss(&LL, addr, size)) {
         VG_(printf)("S %08lx\n", addr);
      }
   }
}
static void trace_load(Addr addr, SizeT size)
{
   if (skip_cache || cachesim_ref_is_miss(&D1, addr, size)) {
      if (skip_cache || cachesim_ref_is_miss(&LL, addr, size)) {
         VG_(printf)("L %08lx\n", addr);
      }
   }
}
static void trace_modify(Addr addr, SizeT size)
{
   if (skip_cache || cachesim_ref_is_miss(&D1, addr, size)) {
      if (skip_cache || cachesim_ref_is_miss(&LL, addr, size)) {
         VG_(printf)("M %08lx\n", addr);
      }
   }
}

static void flushEvents(IRSB* sb)
{
   Int          i;
   const HChar* helperName;
   void*        helperAddr;
   IRExpr**     argv;
   IRDirty*     di;
   Event*       ev;

   for (i = 0; i < events_used; i++) {

      ev = &events[i];

      // Decide on helper fn to call and args to pass it.
      switch (ev->ekind) {
      case Event_Ir:
         helperName = "trace_instr";
         helperAddr = trace_instr;
         break;

      case Event_Dr:
         helperName = "trace_load";
         helperAddr = trace_load;
         break;

      case Event_Dw:
         helperName = "trace_store";
         helperAddr = trace_store;
         break;

      case Event_Dm:
         helperName = "trace_modify";
         helperAddr = trace_modify;
         break;
      default:
         tl_assert(0);
      }

      // Add the helper.
      argv = mkIRExprVec_2(ev->addr, mkIRExpr_HWord(ev->size));
      di   = unsafeIRDirty_0_N(/*regparms*/ 2, helperName,
                             VG_(fnptr_to_fnentry)(helperAddr), argv);
      if (ev->guard) {
         di->guard = ev->guard;
      }
      addStmtToIRSB(sb, IRStmt_Dirty(di));
   }

   events_used = 0;
}

// WARNING:  If you aren't interested in instruction reads, you can omit the
// code that adds calls to trace_instr() in flushEvents().  However, you
// must still call this function, addEvent_Ir() -- it is necessary to add
// the Ir events to the events list so that merging of paired load/store
// events into modify events works correctly.
static void addEvent_Ir(IRSB* sb, IRExpr* iaddr, UInt isize)
{
   Event* evt;
   tl_assert((VG_MIN_INSTR_SZB <= isize && isize <= VG_MAX_INSTR_SZB) ||
             VG_CLREQ_SZB == isize);
   if (events_used == N_EVENTS)
      flushEvents(sb);
   tl_assert(events_used >= 0 && events_used < N_EVENTS);
   evt        = &events[events_used];
   evt->ekind = Event_Ir;
   evt->addr  = iaddr;
   evt->size  = isize;
   evt->guard = NULL;
   events_used++;
}

/* Add a guarded read event. */
static void
addEvent_Dr_guarded(IRSB* sb, IRExpr* daddr, Int dsize, IRExpr* guard)
{
   Event* evt;
   tl_assert(isIRAtom(daddr));
   if (events_used == N_EVENTS)
      flushEvents(sb);
   tl_assert(events_used >= 0 && events_used < N_EVENTS);
   evt        = &events[events_used];
   evt->ekind = Event_Dr;
   evt->addr  = daddr;
   evt->size  = dsize;
   evt->guard = guard;
   events_used++;
}

/* Add an ordinary read event, by adding a guarded read event with an
   always-true guard. */
static void addEvent_Dr(IRSB* sb, IRExpr* daddr, Int dsize)
{
   addEvent_Dr_guarded(sb, daddr, dsize, NULL);
}

/* Add a guarded write event. */
static void
addEvent_Dw_guarded(IRSB* sb, IRExpr* daddr, Int dsize, IRExpr* guard)
{
   Event* evt;
   tl_assert(isIRAtom(daddr));
   if (events_used == N_EVENTS)
      flushEvents(sb);
   tl_assert(events_used >= 0 && events_used < N_EVENTS);
   evt        = &events[events_used];
   evt->ekind = Event_Dw;
   evt->addr  = daddr;
   evt->size  = dsize;
   evt->guard = guard;
   events_used++;
}

/* Add an ordinary write event.  Try to merge it with an immediately
   preceding ordinary read event of the same size to the same
   address. */
static void addEvent_Dw(IRSB* sb, IRExpr* daddr, Int dsize)
{
   Event* lastEvt;
   Event* evt;
   tl_assert(isIRAtom(daddr));

   // Is it possible to merge this write with the preceding read?
   lastEvt = &events[events_used - 1];
   if (events_used > 0 && lastEvt->ekind == Event_Dr &&
       lastEvt->size == dsize && lastEvt->guard == NULL &&
       eqIRAtom(lastEvt->addr, daddr)) {
      lastEvt->ekind = Event_Dm;
      return;
   }

   // No.  Add as normal.
   if (events_used == N_EVENTS)
      flushEvents(sb);
   tl_assert(events_used >= 0 && events_used < N_EVENTS);
   evt        = &events[events_used];
   evt->ekind = Event_Dw;
   evt->size  = dsize;
   evt->addr  = daddr;
   evt->guard = NULL;
   events_used++;
}

static IRSB* ft_instrument(VgCallbackClosure*     closure,
                           IRSB*                  sbIn,
                           const VexGuestLayout*  layout,
                           const VexGuestExtents* vge,
                           const VexArchInfo*     archinfo_host,
                           IRType                 gWordTy,
                           IRType                 hWordTy)
{
   Int        i;
   IRSB*      sbOut;
   IRTypeEnv* tyenv = sbIn->tyenv;

   if (gWordTy != hWordTy) {
      /* We don't currently support this case. */
      VG_(tool_panic)("host/guest word size mismatch");
   }

   /* Set up SB */
   sbOut = deepCopyIRSBExceptStmts(sbIn);

   // Copy verbatim any IR preamble preceding the first IMark
   i = 0;
   while (i < sbIn->stmts_used && sbIn->stmts[i]->tag != Ist_IMark) {
      addStmtToIRSB(sbOut, sbIn->stmts[i]);
      i++;
   }

   events_used = 0;

   for (/*use current i*/; i < sbIn->stmts_used; i++) {
      IRStmt* st = sbIn->stmts[i];
      if (!st || st->tag == Ist_NoOp)
         continue;

      switch (st->tag) {
      case Ist_NoOp:
      case Ist_AbiHint:
      case Ist_Put:
      case Ist_PutI:
      case Ist_MBE:
         addStmtToIRSB(sbOut, st);
         break;

      case Ist_IMark:
         // WARNING: do not remove this function call, even if you
         // aren't interested in instruction reads.  See the comment
         // above the function itself for more detail.
         addEvent_Ir(sbOut, mkIRExpr_HWord((HWord)st->Ist.IMark.addr),
                     st->Ist.IMark.len);

         addStmtToIRSB(sbOut, st);
         break;

      case Ist_WrTmp: {
         // Add a call to trace_load() if --trace-mem=yes.

         IRExpr* data = st->Ist.WrTmp.data;
         if (data->tag == Iex_Load) {
            addEvent_Dr(sbOut, data->Iex.Load.addr,
                        sizeofIRType(data->Iex.Load.ty));
         }

         addStmtToIRSB(sbOut, st);
         break;
      }

      case Ist_Store: {
         IRExpr* data = st->Ist.Store.data;
         IRType  type = typeOfIRExpr(tyenv, data);
         tl_assert(type != Ity_INVALID);
         addEvent_Dw(sbOut, st->Ist.Store.addr, sizeofIRType(type));

         addStmtToIRSB(sbOut, st);
         break;
      }

      case Ist_StoreG: {
         IRStoreG* sg   = st->Ist.StoreG.details;
         IRExpr*   data = sg->data;
         IRType    type = typeOfIRExpr(tyenv, data);
         tl_assert(type != Ity_INVALID);
         addEvent_Dw_guarded(sbOut, sg->addr, sizeofIRType(type), sg->guard);

         addStmtToIRSB(sbOut, st);
         break;
      }

      case Ist_LoadG: {
         IRLoadG* lg       = st->Ist.LoadG.details;
         IRType   type     = Ity_INVALID; /* loaded type */
         IRType   typeWide = Ity_INVALID; /* after implicit widening */
         typeOfIRLoadGOp(lg->cvt, &typeWide, &type);
         tl_assert(type != Ity_INVALID);
         addEvent_Dr_guarded(sbOut, lg->addr, sizeofIRType(type), lg->guard);

         addStmtToIRSB(sbOut, st);
         break;
      }

      case Ist_Dirty: {
         Int      dsize;
         IRDirty* d = st->Ist.Dirty.details;
         if (d->mFx != Ifx_None) {
            // This dirty helper accesses memory.  Collect the details.
            tl_assert(d->mAddr != NULL);
            tl_assert(d->mSize != 0);
            dsize = d->mSize;
            if (d->mFx == Ifx_Read || d->mFx == Ifx_Modify)
               addEvent_Dr(sbOut, d->mAddr, dsize);
            if (d->mFx == Ifx_Write || d->mFx == Ifx_Modify)
               addEvent_Dw(sbOut, d->mAddr, dsize);
         } else {
            tl_assert(d->mAddr == NULL);
            tl_assert(d->mSize == 0);
         }

         addStmtToIRSB(sbOut, st);
         break;
      }

      case Ist_CAS: {
         /* We treat it as a read and a write of the location.  I
            think that is the same behaviour as it was before IRCAS
            was introduced, since prior to that point, the Vex
            front ends would translate a lock-prefixed instruction
            into a (normal) read followed by a (normal) write. */
         Int    dataSize;
         IRType dataTy;
         IRCAS* cas = st->Ist.CAS.details;
         tl_assert(cas->addr != NULL);
         tl_assert(cas->dataLo != NULL);
         dataTy   = typeOfIRExpr(tyenv, cas->dataLo);
         dataSize = sizeofIRType(dataTy);
         if (cas->dataHi != NULL)
            dataSize *= 2; /* since it's a doubleword-CAS */
         addEvent_Dr(sbOut, cas->addr, dataSize);
         addEvent_Dw(sbOut, cas->addr, dataSize);

         addStmtToIRSB(sbOut, st);
         break;
      }

      case Ist_LLSC: {
         IRType dataTy;
         if (st->Ist.LLSC.storedata == NULL) {
            /* LL */
            dataTy = typeOfIRTemp(tyenv, st->Ist.LLSC.result);
            addEvent_Dr(sbOut, st->Ist.LLSC.addr, sizeofIRType(dataTy));
            /* flush events before LL, helps SC to succeed */
            flushEvents(sbOut);

         } else {
            /* SC */
            dataTy = typeOfIRExpr(tyenv, st->Ist.LLSC.storedata);
            addEvent_Dw(sbOut, st->Ist.LLSC.addr, sizeofIRType(dataTy));
         }
         addStmtToIRSB(sbOut, st);
         break;
      }

      case Ist_Exit:
         flushEvents(sbOut);

         addStmtToIRSB(sbOut, st); // Original statement
         break;

      default:
         ppIRStmt(st);
         tl_assert(0);
      }
   }

   /* At the end of the sbIn.  Flush outstandings. */
   flushEvents(sbOut);

   return sbOut;
}

static void ft_fini(Int exitcode) {}

static void ft_pre_clo_init(void)
{
   VG_(details_name)("FtMemSim-Trace");
   VG_(details_version)(NULL);
   VG_(details_description)("Tracer for `ftmemsim`");
   VG_(details_copyright_author)("");
   VG_(details_bug_reports_to)(VG_BUGS_TO);

   VG_(details_avg_translation_sizeB)(275);

   VG_(basic_tool_funcs)(ft_post_clo_init, ft_instrument, ft_fini);

   /* No needs, no core events to track */
}

VG_DETERMINE_INTERFACE_VERSION(ft_pre_clo_init)
