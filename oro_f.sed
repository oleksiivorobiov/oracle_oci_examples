# NAME
#   oro_f.sed 
# DESCRIPTION
# This sed script changes the ORO function and type to long  names. 
# MODIFIED
#    10/03/96 - Peter Vasterd - Creation
#    10/10/96 - Peter Vasterd - Merge Calvins' stuff
s/oroind/OCIInd/g
s/oroodt/OCIDuration/g
s/oroolm/OCILockOpt/g
s/oroomo/OCIMarkOpt/g
s/oroopo/OCIPinOpt/g
s/ororef/OCIRef/g
s/orooro/OCICoherency/g
s/orotc/OCITypeCode/g
s/orotec/OCITypeEncap/g
s/orotmf/OCITypeMethodFlag/g
s/orotpm/OCITypeParamMode/g
s/orotgo/OCITypeGetOpt/g
