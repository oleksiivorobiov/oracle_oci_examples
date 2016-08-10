# NAME
#   oci_f.sed 
# DESCRIPTION
# This sed script changes the OCI function and  type names to long
# names
# MODIFIED
#    10/03/96 - Peter Vasterd - Creation
#    10/04/96 - Sreenivas Gollapudi - Added ComplexObjectComp
#
s/ocianyd/ocianyd/g
s/ociatch/OCIServerAttach/g
s/ociauth/OCISessionBegin/g
s/ocibda/OCIBindDynamic/g
s/ocibicfp/OCICallbackInBind/g
s/ociblobl/OCIBlobLocator/g
s/ociclobl/OCIClobLocator/g
s/ocibocfp/OCICallbackOutBind/g
s/ocidcfp/OCICallbackDefine/g
s/ocibndh/OCIBind/g
s/ocibndt/OCIBindObject/g
s/ocibreak/OCIBreak/g
s/ocibsa/OCIBindArrayOfStruct/g
s/ocicorh/OCIComplexObject/g
s/ocicord/OCIComplexObjectComp/g
s/ocicpw/OCIPasswordChange/g
s/ocidarr/OCIDefineArrayOfStruct/g
s/ociddf/OCIDefineDynamic/g
s/ocidfnh/OCIDefine/g
s/ocidndt/OCIDefineObject/g
s/ocidsca/OCIDescribeAny/g
s/ocidsch/OCIDescribe/g
s/ocidtch/OCIServerDetach/g
s/ocienvh/OCIEnv/g
s/ocierrh/OCIError/g
s/ociexec/OCIStmtExecute/g
s/ocifch/OCIStmtFetch/g
s/ocifcls/OCILobFileClose/g
s/ocifcrt/OCILobFileCreate/g
s/ocifdel/OCILobFileDelete/g
s/ocifdesc/OCIDescriptorFree/g
s/ocifhndl/OCIHandleFree/g
s/ocifopn/OCILobFileOpen/g
s/ocifreem/OCIMemoryFree/g
s/ocigattr/OCIAttrGet/g
s/ocigbp/OCIStmtGetBindInfo/g
s/ocigdesc/OCIDescriptorAlloc/g
s/ocigdr/OCIErrorGet/g
s/ocighndl/OCIHandleAlloc/g
s/ocigparm/OCIParamGet/g
s/ocisparm/OCIParamSet/g
s/ocigpi/OCIStmtGetPieceInfo/g
s/ociinit/OCIEnvInit/g
s/ocild2sv/OCILdaToSvcCtx/g
s/ocilfap/OCILobAppend/g
s/ocilfcp/OCILobCopy/g
s/ocilfer/OCILobErase/g
s/ocilfln/OCILobGetLength/g
s/ocilfrd/OCILobRead/g
s/ocilftr/OCILobTrim/g
s/ocilfwr/OCILobWrite/g
s/ocilobd/OCILobLocator/g
s/ocilobl/OCILobLocator/g
s/ocipard/OCIParam/g
s/ocipi/OCIInitialize/g
s/ocirefd/ocirefd/g
s/ocireq/OCIStmtPrepare/g
s/ociridd/OCIRowid/g
s/ocirs2sh/OCIResultSetToStmt/g
s/ocirstd/OCIResult/g
s/ocisattr/OCIAttrSet/g
s/ocisnad/OCISnapshot/g
s/ocispi/OCIStmtSetPieceInfo/g
s/ocisrvh/OCIServer/g
s/ocistmh/OCIStmt/g
s/ocisv2ld/OCISvcCtxToLda/g
s/ocisvch/OCISvcCtx/g
s/ocitac/OCISessionEnd/g
s/ocitxcm/OCITransCommit/g
s/ocitxdt/OCITransDetach/g
s/ocitxfgt/OCITransForget/g
s/ocitxnh/OCITrans/g
s/ocitxpre/OCITransPrepare/g
s/ocitxrl/OCITransRollback/g
s/ocitxst/OCITransStart/g
s/ociusrh/OCISession/g
s/ocivers/OCIServerVersion/g
