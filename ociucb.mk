#
# Makefile for building callback shared library
#
#    To link user callback DLL:
#
#    make -f ociucb.mk user_callback SHARED_LIBNAME=libname 
#                                             OBJS="user.o ..."
#    e.g. make -f ociucb.mk user_callback SHARED_LIBNAME=ociucb.so.1.0 \
#                                                   OBJS=ociucb.o
#
#
#
#
# NOTE: 1. ORACLE_HOME must be either:
#		   . set in the user's environment
#		   . passed in on the command line
#		   . defined in a modified version of this makefile
#
#       2. Look in the platform specific documentation for information
#          about environment variables that need to be properly
#          defined (e.g. LD_LIBRARY_PATH in Solaris).
#

include $(ORACLE_HOME)/rdbms/lib/env_rdbms.mk

.SUFFIXES: .o

.c.o:
	$(C2O)

user_callback: $(OBJS)
	$(BUILD_USERCALLBACK)
