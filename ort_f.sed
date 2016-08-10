# NAME
#   ort_f.sed 
# DESCRIPTION
# This sed script changes the ORT function and  type names to long names
# MODIFIED
#    10/03/96 - Peter Vasterd - Creation
#    10/10/96 - Peter Vasterd - Merge Calvins' stuff
s/ortado/OCITypeElem/g
s/ortgabi/OCITypeAttrNext/g
s/ortgabn/OCITypeAttrByName/g
s/ortganm/OCITypeElemName/g
s/ortgpnm/OCITypeElemName/g
s/ortgasqt/OCITypeElemExtTypeCode/g
s/ortgatc/OCITypeElemTypeCode/g
s/ortgcel/OCITypeCollElem/g
s/ortgcne/OCITypeCollSize/g
s/ortgcsqt/OCITypeCollExtTypeCode/g
s/ortgdttc/OCITypeCollTypeCode/g
s/ortgmbi/OCITypeMethodNext/g
s/ortgmbn/OCITypeMethodByName/g
s/ortgmen/OCITypeMethodEncap/g
s/ortgmfl/OCITypeMethodFlags/g
s/ortgmmap/OCITypeMethodMap/g
s/ortgmnm/OCITypeMethodName/g
s/ortgmno/OCITypeMethodOverload/g
s/ortgmnp/OCITypeMethodParams/g
s/ortgmor/OCITypeMethodOrder/g
s/ortgnp/OCITypeElemNumPrec/g
s/ortgns/OCITypeElemNumScale/g
s/ortgpa/OCITypeElemParameterizedType/g
s/ortgpbn/OCITypeParamByName/g
s/ortgpbp/OCITypeParamByPos/g
s/ortgpps/OCITypeParamPos/g
s/ortgpdv/OCITypeElemDefaultValue/g
s/ortgpmo/OCITypeElemParamMode/g
s/ortgptc/OCITypeElemTypeCode/g
s/ortgscform/OCITypeElemCharSetForm/g
s/ortgscid/OCITypeElemCharSetID/g
s/ortgsl/OCITypeElemLength/g
s/ortgtme/OCITypeName/g
s/ortgtna/OCITypeAttrs/g
s/ortgtnm/OCITypeMethods/g
s/ortgttc/OCITypeTypeCode/g
s/ortgtsch/OCITypeSchema/g
s/ortgtvn/OCITypeVersion/g
s/ortgpty/OCITypeElemType/g
s/ortifre/OCITypeIterFree/g
s/ortinew/OCITypeIterNew/g
s/ortiset/OCITypeIterSet/g
s/ortitr/OCITypeIter/g
s/ortmdo/OCITypeMethod/g
s/ortpdo/OCITypeElem/g
s/ortrdo/OCITypeElem/g
s/orttdo/OCIType/g
s/ortvini/OCITypeVTInit/g
s/ortvins/OCITypeVTInsert/g
s/ortvsel/OCITypeVTSelect/g
s/ortgatyp/OCITypeArrayByName/g
s/ortgaty/OCITypeElemType/g
