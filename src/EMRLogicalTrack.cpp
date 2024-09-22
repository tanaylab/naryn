#include "EMRLogicalTrack.h"
#include "BufferedFile.h"

bool EMRLogicalTrack::serialize(const char* filename){
    BufferedFile bf;
    if (bf.open(filename, "w", true))
        verror("Failed to open file %s: %s", filename, strerror(errno));

    vdebug(7, "Creating logical track at: %s ", filename); 

    bf.write(this->source.c_str(), this->source.size() + 1);

    uint32_t num_values =
        (uint32_t)this->values.size();  // number of attributes

    bf.write(&num_values, sizeof(num_values));
    if (!this->values.empty()) {
        bf.write(this->values.data(), sizeof(int) * this->values.size());
    }

    if (bf.error())
        verror("Error while writing file %s: %s\n", bf.file_name().c_str(),
               strerror(errno));

    // release lock
    bf.close();

    return (true);
}

EMRLogicalTrack EMRLogicalTrack::unserialize(const char* filename){
    BufferedFile bf;
    if (bf.open(filename, "r", true)) {
        if (errno != ENOENT) {
            verror("Failed to open file %s for reading: %s", filename,
                   strerror(errno));
        }        
    }

    struct stat fs;
    if (bf.stat(&fs) == -1)
        verror("stat failed on file %s: %s", bf.file_name().c_str(),
               strerror(errno));

    if (bf.file_size()) {
        int c;
        string buf;
        string track;
        string source;
        vector<int> values;
        uint32_t num_vals;       

        // the structure of file is source,number of values,values(if
        // exists)
        while ((c = bf.getc()) != EOF) {
            if (c) {
                buf.push_back(c);
            } else { // end of source string                                
                source = buf;
                if (bf.read(&num_vals, sizeof(num_vals)) !=
                    sizeof(num_vals))
                    break;
                if (num_vals > 0) {
                    values.clear();
                    values.resize(num_vals);
                    bf.read(values.data(), sizeof(int) * num_vals);
                    return (EMRLogicalTrack(source, values));
                } else {
                    return (EMRLogicalTrack(source));
                }                                    
                buf.clear();
            }
        }

        if (c != EOF) {
            return (EMRLogicalTrack());
        }        
    }

    bf.close();

    return (EMRLogicalTrack());
}

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
        uint64_t num_values = this->num_values();
        
        rprotect(params = RSaneAllocVector(INTSXP, num_values));
        for (vector<int>::const_iterator iid = this->values.begin();
            iid != this->values.end(); ++iid) {
            uint64_t index = iid - this->values.begin();
            INTEGER(params)[index] = *iid;
        }        
        SET_VECTOR_ELT(answer, PARAMS, params);
    } 

    for (int i = 0; i < NUM_NAMES; i++)
        SET_STRING_ELT(column_names, i, mkChar(VTRACK_NAMES[i]));    
    setAttrib(answer, R_NamesSymbol, column_names);
    
    return answer;
}