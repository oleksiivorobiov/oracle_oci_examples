# NAME
#   ori_f.sed 
# DESCRIPTION
# This sed script changes the ORI function and type names to long
# MODIFIED
#    10/03/96 - Peter Vasterd - Creation
#    10/16/96 - Peter Vasterd - Add changes from Calvin
s/oricfls/OCICacheFlush/g
s/origbgu/OCIDurationBegin/g
s/origedu/OCIDurationEnd/g
s/origpdr/OCIDurationGetParent/g
s/orioapn/OCIObjectArrayPin/g
s/oricfre/OCICacheFree/g
s/oriocpy/OCIObjectCopy/g
s/oricrfs/OCICacheRefresh/g
s/oricunp/OCICacheUnpin/g
s/oriogex/OCIObjectExists/g
s/oriogfl/OCIObjectFlushStatus/g
s/oriogns/OCIObjectGetInd/g
s/oriogor/OCIObjectGetObjectRef/g
s/oriogtb/OCIObjectPinTable/g
s/oriogtr/OCIObjectGetTypeRef/g
s/orionew/OCIObjectNew/g
s/oriofls/OCIObjectFlush/g
s/oriofre/OCIObjectFree/g
s/oriolck/OCIObjectLock/g
s/oriordl/OCIObjectMarkDeleteByRef/g
s/oriopin/OCIObjectPin/g
s/oriopdl/OCIObjectMarkDelete/g
s/oriorfs/OCIObjectRefresh/g
s/oriounp/OCIObjectUnpin/g
s/orioupd/OCIObjectMarkUpdate/g
s/orioupz/OCIObjectPinCountReset/g
s/oridset/OCIObjectSetAttr/g
s/oridget/OCIObjectGetAttr/g
