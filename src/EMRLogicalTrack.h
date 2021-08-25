#ifndef EMRLOGICALTRACK_H_INCLUDED
#define EMRLOGICALTRACK_H_INCLUDED

#include <unordered_set>
#include <vector>

#include "EMR.h"
#include "naryn.h"

class EMRLogicalTrack {
    public:
        string source;
        vector<int> values;

        EMRLogicalTrack(const string& _source, const vector<int>& _values):
        source(_source), values(_values) {}

        EMRLogicalTrack(const string& _source)
            : source(_source) { }

        size_t num_values() const { return values.size(); }

        bool has_values() const { return !values.empty(); }

        const char* get_source() const {
            return source.c_str();
        }

        SEXP vtrack() const;
};

#endif 