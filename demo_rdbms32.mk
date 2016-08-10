# This makefile is similar to demo_rdbms.mk, but this one builds
# 32-bit client executables.  (It uses demo_rdbms.mk to build any
# shared-libraries that are loaded into the database server, e.g. for
# external procedure call.)
#
# Example for building demo OCI programs:
#
# 1. All OCI demos (including extdemo2, extdemo4 and extdemo5):
#
#    make -f demo_rdbms32.mk demos
#
# 2. A single OCI demo:
#
#    make -f demo_rdbms32.mk build EXE=demo OBJS="demo.o ..."
#    e.g. make -f demo_rdbms32.mk build EXE=oci02 OBJS=oci02.o
#
# 3. A single OCI demo with static libraries:
#
#    make -f demo_rdbms32.mk build_static EXE=demo OBJS="demo.o ..."
#    e.g. make -f demo_rdbms32.mk build_static EXE=oci02 OBJS=oci02.o
#
# 4. To re-generate shared library:
#
#    make -f demo_rdbms32.mk generate_sharedlib
#
# 5. All OCCI demos
#
#    make -f demo_rdbms32.mk occidemos
#
# 6. A single OCCI demo:
#
#    make -f demo_rdbms32.mk <demoname>
#    e.g. make -f demo_rdbms32.mk occidml
#    OR
#    make -f demo_rdbms32.mk buildocci EXE=demoname OBJS="demoname.o ..."
#    e.g. make -f demo_rdbms32.mk buildocci EXE=occidml OBJS=occidml.o
#
# 7. A single OCCI demo with static libraries:
#
#    make -f demo_rdbms32.mk buildocci_static EXE=demoname OBJS="demoname.o ..."
#    e.g. make -f demo_rdbms32.mk buildocci_static EXE=occiblob OBJS=occiblob.o
#
# 8. All OCI Connection Pooling, Session Pooling and Statement Cache demos
#
#    make -f demo_rdbms32.mk cpdemos
#
# 9. A single OCI Connection Pooling demo:
#
#    make -f demo_rdbms32.mk <demoname>
#    e.g. make -f demo_rdbms32.mk cdemocp
#    OR
#    make -f demo_rdbms32.mk buildcp EXE=demoname OBJS="demoname.o ..."
#    e.g. make -f demo_rdbms32.mk buildcp EXE=cdemocp OBJS=cdemocp.o
#
# 10. A single OCI Connection Pooling demo with static libraries:
#
#    make -f demo_rdbms32.mk buildcp_static EXE=demoname OBJS="demoname.o ..."
#    e.g. make -f demo_rdbms32.mk buildcp_static EXE=cdemocp OBJS=cdemocp.o
#
# 11. A single OCI Session Pooling demo:
#
#    make -f demo_rdbms32.mk <demoname>
#    e.g. make -f demo_rdbms32.mk cdemosp
#    OR
#    make -f demo_rdbms32.mk buildcp EXE=demoname OBJS="demoname.o ..."
#    e.g. make -f demo_rdbms32.mk buildcp EXE=cdemosp OBJS=cdemosp.o
#
# 12. A single OCI Session Pooling demo with static libraries:
#
#    make -f demo_rdbms32.mk buildcp_static EXE=demoname OBJS="demoname.o ..."
#    e.g. make -f demo_rdbms32.mk buildcp_static EXE=cdemosp OBJS=cdemosp.o
#
#
# 13. A single OCI Statement Cache demo:
#
#    make -f demo_rdbms32.mk <demoname>
#    e.g. make -f demo_rdbms32.mk cdemostc
#    OR
#    make -f demo_rdbms32.mk buildcp EXE=demoname OBJS="demoname.o ..."
#    e.g. make -f demo_rdbms32.mk buildcp EXE=cdemostc OBJS=cdemostc.o
#
# 14. A single OCI Statement Cache demo with static libraries:
#
#    make -f demo_rdbms32.mk buildcp_static EXE=demoname OBJS="demoname.o ..."
#    e.g. make -f demo_rdbms32.mk buildcp_static EXE=cdemostc OBJS=cdemostc.o
#
#
# Example for building demo DIRECT PATH API programs:
#
# 1. All DIRECT PATH API demos:
#
#    make -f demo_rdbms32.mk demos_dp
#
# 2. A single DIRECT PATH API demo:
#
#    make -f demo_rdbms32.mk build_dp EXE=demo OBJS="demo.o ..."
#    e.g. make -f demo_rdbms32.mk build_dp EXE=cdemdplp OBJS=cdemdplp.o
#
#
# Example for building external procedures demo programs:
#
# 1. All external procedure demos:
#
# 2. A single external procedure demo whose 3GL routines do not use the 
#    "with context" argument:
#
#    make -f demo_rdbms32.mk extproc_no_context SHARED_LIBNAME=libname 
#                                             OBJS="demo.o ..."
#    e.g. make -f demo_rdbms32.mk extproc_no_context SHARED_LIBNAME=epdemo.so
#                                             OBJS="epdemo1.o epdemo2.o"
#
# 3. A single external procedure demo where one or more 3GL routines use the 
#    "with context" argument:
#
#    make -f demo_rdbms32.mk extproc_with_context SHARED_LIBNAME=libname 
#                                             OBJS="demo.o ..."
#    e.g. make -f demo_rdbms32.mk extproc_with_context SHARED_LIBNAME=epdemo.so
#                                             OBJS="epdemo1.o epdemo2.o"
#    e.g. make -f demo_rdbms32.mk extproc_with_context 
#                       SHARED_LIBNAME=extdemo2.so OBJS="extdemo2.o"
#    e.g. or For EXTDEMO2 DEMO ONLY: make -f demo_rdbms32.mk demos
#
# 4. To link C++ demos:
#
#    make -f demo_rdbms32.mk cppdemos
#
# Example for building OCI programs with libclntsh.so
#
# 1. A single OCI demo:
#
#    make -f demo_rdbms32.mk build_clntshared EXE=demo OBJS="demo.o ..."
#    e.g. make -f demo_rdbms32.mk build_clntshared EXE=oci02 OBJS=oci02.o
#
# Example for building OCI programs with no pthread dependency
#
# 1. A single OCI demo:
#
#    make -f demo_rdbms32.mk build_nopthread EXE=demo OBJS="demo.o ..."
#    e.g. make -f demo_rdbms32.mk build_nopthread EXE=oci02 OBJS=oci02.o
#
#
#
# NOTE: 1. ORACLE_HOME must be either:
#                  . set in the user's environment
#                  . passed in on the command line
#                  . defined in a modified version of this makefile
#
#       2. If the target platform support shared libraries (e.g. Solaris)
#          look in the platform specific documentation for information
#          about environment variables that need to be properly
#          defined (e.g. LD_LIBRARY_PATH in Solaris).
#

include $(ORACLE_HOME)/rdbms/lib/env_rdbms.mk

# flag for linking with non-deferred option (default is deferred mode)
NONDEFER=false

DEMO_DIR=$(ORACLE_HOME)/rdbms/demo
DEMO_MAKEFILE = $(DEMO_DIR)/demo_rdbms.mk
DEMO32_MAKEFILE = $(DEMO_DIR)/demo_rdbms32.mk

DEMOS = cdemo1 cdemo2 cdemo3 cdemo4 cdemo5 cdemo81 cdemo82 \
        cdemobj cdemolb cdemodsc cdemocor cdemolb2 cdemolbs \
        cdemodr1 cdemodr2 cdemodr3 cdemodsa obndra \
        cdemoext cdemothr cdemofil cdemofor \
        oci02 oci03 oci04 oci05 oci06 oci07 oci08 oci09 oci10 \
        oci11 oci12 oci13 oci14 oci15 oci16 oci17 oci18 oci19 oci20 \
        oci21 oci22 oci23 oci24 oci25 readpipe cdemosyev \
        ociaqdemo00 ociaqdemo01 ociaqdemo02 cdemoucb nchdemo1 \
        ociaqarraydeq ociaqarrayenq strmmon

DEMOS_DP = cdemdpco cdemdpin cdemdpit cdemdplp cdemdpno cdemdpro cdemdpss 

CPPDEMOS = cdemo6
# OCCI Demos
OCCIDEMOS = occiblob occiclob occicoll occidesc occidml occipool occiproc \
            occistre occiaqlis occiscp occixa occiuni1 occimb1 occilbar
OCCIOTTDEMOS = occiobj occiinh occipobj occiaqop
OCCIOTTUSR = hr
OCCIOTTPWD = hr
OCCI_UNICODE_OPT = none
# OTT Markers Support
OCCIOTTDEMOSWITHMARKER = mdemo1
OTTUSR = scott
OTTPWD = tiger
CPDEMOS = cdemocp cdemocpproxy cdemosp cdemostc

.SUFFIXES: .o .cob .for .c .pc .cc .cpp

demos: $(DEMOS) extdemo2 extdemo4 extdemo5 extdemo6

demos_dp: $(DEMOS_DP) 

generate_sharedlib:
	$(SILENT)$(ECHO) "Building client shared library ..."
	$(SILENT)$(ECHO) "Calling script $$ORACLE_HOME/bin/genclntsh ..."
	$(GENCLNTSH32)
	$(SILENT)$(ECHO) "The library is $$ORACLE_HOME/lib/libclntsh.so... DONE"

BUILD=build
$(DEMOS):
	$(MAKE) -f $(DEMO32_MAKEFILE) $(BUILD) EXE=$@ OBJS=$@.o

$(DEMOS_DP): cdemodp.c cdemodp0.h cdemodp.h
	$(MAKE) -f $(DEMO32_MAKEFILE) build_dp EXE=$@ OBJS=$@.o

cppdemos: $(CPPDEMOS)

$(CPPDEMOS):
	$(MAKE) -f $(DEMO32_MAKEFILE) buildcpp EXE=$@ OBJS=$@.o

buildcpp: $(OBJS)
	$(MAKECPLPLDEMO32)

occidemos:	$(OCCIDEMOS) $(OCCIOTTDEMOS) $(OCCIOTTDEMOSWITHMARKER)

$(OCCIDEMOS):
	$(MAKE) -f $(DEMO32_MAKEFILE) buildocci EXE=$@ OBJS=$@.o

$(OCCIOTTDEMOS):
	$(MAKE) -f $(DEMO32_MAKEFILE) occiott OCCIOTTFILE=$@
	$(MAKE) -f $(DEMO32_MAKEFILE) buildocci EXE=$@ OBJS="$@.o $@o.o $@m.o"

# OTT Markers Support
$(OCCIOTTDEMOSWITHMARKER):
	$(MAKE) -f $(DEMO32_MAKEFILE) ott_mrkr OTTFILE=$@
	$(MAKE) -f $(DEMO32_MAKEFILE) buildocci EXE=$@ OBJS="$@.o $@o.o $@m.o"


buildocci: $(OBJS)
	$(MAKEOCCISHAREDDEMO32)

buildocci_static: $(OBJS)
	$(MAKEOCCISTATICDEMO32)

occiott:
	$(ORACLE_HOME)/bin/ott \
                userid=$(OCCIOTTUSR)/$(OCCIOTTPWD) \
                intype=$(OCCIOTTFILE).typ \
                outtype=$(OCCIOTTFILE)out.type \
                code=cpp \
                hfile=$(OCCIOTTFILE).h \
                cppfile=$(OCCIOTTFILE)o.cpp \
                attraccess=private \
                unicode=$(OCCI_UNICODE_OPT)

# OTT Markers Suppport
ott_mrkr:
	$(ORACLE_HOME)/bin/ott \
                userid=$(OTTUSR)/$(OTTPWD) \
                intype=$(OTTFILE).typ \
                outtype=$(OTTFILE)out.type \
                code=cpp \
                hfile=$(OTTFILE).h \
                cppfile=$(OTTFILE)o.cpp \
                use_marker=true

cpdemos:        $(CPDEMOS)
$(CPDEMOS):
	$(MAKE) -f $(DEMO32_MAKEFILE) buildcp EXE=$@ OBJS=$@.o
buildcp: $(OBJS)
	$(MAKECPSHAREDDEMO32)
buildcp_static: $(OBJS)
	$(MAKECPSTATICDEMO32)

# Pro*C rules
# SQL Precompiler macros

pc1:
	$(PCC2C)

.pc.c:
	$(MAKE) -f $(DEMO32_MAKEFILE) PCCSRC=$* I_SYM=include= pc1

.pc.o:
	$(MAKE) -f $(DEMO32_MAKEFILE) PCCSRC=$* I_SYM=include= pc1
	$(PCCC2O32)

.cc.o:
	$(CCC2O32)

.cpp.o:
	$(CCC2O32)

build: $(LIBCLNTSH) $(OBJS)
	$(BUILDEXE32)

extdemo2:
	$(MAKE) -f $(DEMO32_MAKEFILE) extproc_with_context SHARED_LIBNAME=extdemo2.so OBJS="extdemo2.o"

extdemo4:
	$(MAKE) -f $(DEMO32_MAKEFILE) extproc_with_context SHARED_LIBNAME=extdemo4.so OBJS="extdemo4.o"

extdemo5:
	$(MAKE) -f $(DEMO32_MAKEFILE) extproc_with_context SHARED_LIBNAME=extdemo5.so OBJS="extdemo5.o"

extdemo6:
	$(MAKE) -f $(DEMO32_MAKEFILE) extproc_with_context SHARED_LIBNAME=extdemo6.so OBJS="extdemo6.o"

.c.o:
	$(C2O32)

build_dp: $(LIBCLNTSH) $(OBJS) cdemodp.o
	$(DPTARGET32)

build_static: $(OBJS)
	$(O2STATIC32)

# These 3 should be supplied by platform specific files
#LLIBCLNTSH_NPT=$(LDLIBFLAG)$(LIBCLNTSHNAME)_nopthread
#LIBCLNTSH_NPT=$(LIBHOME32)$(LIB_PREFIX)$(LIBCLNTSHNAME)_nopthread.$(SO_EXT)
#BUILDEXE32_NPT=$(CC) $(LDFLAGS32) -o $(EXE) $(OBJS) $(LLIBCLNTSH_NPT) $(MATHLIB)

build_clntshared :
	$(MAKECLNTSHAREDDEMO32)

# You can build OCI programs with no pthread dependency using
# the following target.
build_nopthread: $(LIBCLNTSH_NPT) $(OBJS)
	$(MAKENPTDEMO32)

# extproc_no_context and extproc_with_context are the current names of these
# targets.  The old names, extproc_nocallback and extproc_callback are
# preserved for backward compatibility.

extproc_no_context extproc_nocallback: $(OBJS)
	$(BUILDLIB32_NO_CONTEXT)

extproc_with_context extproc_callback: $(OBJS) $(LIBCLNTSH)
	$(BUILDLIB32_WITH_CONTEXT)

clean:
	$(RM) -f $(DEMOS) $(CPDEMOS) extdemo2 extdemo4 extdemo5 extdemo6 *.o *.so
	$(RM) -f $(OCCIDEMOS)  $(OCCIOTTDEMOS) $(OCCIOTTDEMOSWITHMARKER) occi*m.cpp occi*o.cpp \
            occi*.type occiobj*.h occiinh*.h occipobj*.h
	$(RM) -f $(DEMOS_DP)
