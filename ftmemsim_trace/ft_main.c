
/*--------------------------------------------------------------------*/
/*--- FtMemSim-Trace: Program tracer for `ftmemsim`.     ft_main.c ---*/
/*--------------------------------------------------------------------*/

#include "pub_tool_basics.h"
#include "pub_tool_tooliface.h"

static void ft_post_clo_init(void) {}

static IRSB* ft_instrument(VgCallbackClosure*     closure,
                           IRSB*                  bb,
                           const VexGuestLayout*  layout,
                           const VexGuestExtents* vge,
                           const VexArchInfo*     archinfo_host,
                           IRType                 gWordTy,
                           IRType                 hWordTy)
{
   return bb;
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
