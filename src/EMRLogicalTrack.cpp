#include "EMRLogicalTrack.h"

SEXP EMRLogicalTrack::vtrack() const{
    enum { SRC, TIME_SHIFT, FUNC, PARAMS, KEEPREF, ID_MAP, FILTER, NUM_NAMES };
    const char *VTRACK_NAMES[NUM_NAMES] = {
        "src", "time_shift", "func", "params", "keepref", "id_map", "filter"};

    SEXP answer, column_names, source, params, keepref;    
    rprotect(column_names = RSaneAllocVector(STRSXP, NUM_NAMES));    
    rprotect(answer = RSaneAllocVector(VECSXP, NUM_NAMES));    
    
    rprotect(source = RSaneAllocVector(STRSXP, 1));
    SET_STRING_ELT(source, 0, mkChar(this->get_source()));
    SET_VECTOR_ELT(answer, SRC, source);

    // keepref is always true
    rprotect(keepref = RSaneAllocVector(LGLSXP, 1));
    LOGICAL(keepref)[0] = true;
    SET_VECTOR_ELT(answer, KEEPREF, keepref);

    if (this->has_values()) {        
        size_t num_values = this->num_values();
        
        rprotect(params = RSaneAllocVector(INTSXP, num_values));
        for (vector<int>::const_iterator iid = this->values.begin();
            iid != this->values.end(); ++iid) {
            size_t index = iid - this->values.begin();
            INTEGER(params)[index] = *iid;
        }        
        SET_VECTOR_ELT(answer, PARAMS, params);
    } 

    for (int i = 0; i < NUM_NAMES; i++)
        SET_STRING_ELT(column_names, i, mkChar(VTRACK_NAMES[i]));    
    setAttrib(answer, R_NamesSymbol, column_names);
    
    return answer;
}