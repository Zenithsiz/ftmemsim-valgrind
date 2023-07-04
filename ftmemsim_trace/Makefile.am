include $(top_srcdir)/Makefile.tool.am

#----------------------------------------------------------------------------
# ftmemsim_trace-<platform>
#----------------------------------------------------------------------------

noinst_PROGRAMS  = ftmemsim_trace-@VGCONF_ARCH_PRI@-@VGCONF_OS@
if VGCONF_HAVE_PLATFORM_SEC
noinst_PROGRAMS += ftmemsim_trace-@VGCONF_ARCH_SEC@-@VGCONF_OS@
endif

ftmemsim_trace_SOURCES_COMMON = ft_main.c

ftmemsim_trace_@VGCONF_ARCH_PRI@_@VGCONF_OS@_SOURCES      = \
	$(ftmemsim_trace_SOURCES_COMMON)
ftmemsim_trace_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
ftmemsim_trace_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CFLAGS       = $(LTO_CFLAGS) \
	$(AM_CFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
ftmemsim_trace_@VGCONF_ARCH_PRI@_@VGCONF_OS@_DEPENDENCIES = \
	$(TOOL_DEPENDENCIES_@VGCONF_PLATFORM_PRI_CAPS@)
ftmemsim_trace_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDADD        = \
	$(TOOL_LDADD_@VGCONF_PLATFORM_PRI_CAPS@)
ftmemsim_trace_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDFLAGS      = \
	$(TOOL_LDFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
ftmemsim_trace_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LINK = \
	$(top_builddir)/coregrind/link_tool_exe_@VGCONF_OS@ \
	@VALT_LOAD_ADDRESS_PRI@ \
	$(LINK) \
	$(ftmemsim_trace_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CFLAGS) \
	$(ftmemsim_trace_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDFLAGS)

if VGCONF_HAVE_PLATFORM_SEC
ftmemsim_trace_@VGCONF_ARCH_SEC@_@VGCONF_OS@_SOURCES      = \
	$(ftmemsim_trace_SOURCES_COMMON)
ftmemsim_trace_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
ftmemsim_trace_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CFLAGS       = $(LTO_CFLAGS) \
	$(AM_CFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
ftmemsim_trace_@VGCONF_ARCH_SEC@_@VGCONF_OS@_DEPENDENCIES = \
	$(TOOL_DEPENDENCIES_@VGCONF_PLATFORM_SEC_CAPS@)
ftmemsim_trace_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDADD        = \
	$(TOOL_LDADD_@VGCONF_PLATFORM_SEC_CAPS@)
ftmemsim_trace_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDFLAGS      = \
	$(TOOL_LDFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
ftmemsim_trace_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LINK = \
	$(top_builddir)/coregrind/link_tool_exe_@VGCONF_OS@ \
	@VALT_LOAD_ADDRESS_SEC@ \
	$(LINK) \
	$(ftmemsim_trace_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CFLAGS) \
	$(ftmemsim_trace_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDFLAGS)
endif
