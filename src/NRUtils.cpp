#include "naryn.h"
#include "NRPoint.h"

using namespace std;

extern "C" {

SEXP C_emr_annotate(SEXP _x, SEXP _y, SEXP _envir)
{
	try {
		Naryn naryn(_envir);

        vector<EMRPoint> xpoints;
        vector<EMRPoint> ypoints;
        vector<pair<uint64_t, uint64_t>> x2y;   // maps a row from x to a row from y

        NRPoint::convert_rpoints(_x, &xpoints, "x: ");
        NRPoint::convert_rpoints(_y, &ypoints, "y: ");

        for (auto ipoint = xpoints.begin() + 1; ipoint < xpoints.end(); ++ipoint) {
            if (*ipoint < *(ipoint - 1))
                verror("\"x\" is not sorted");
        }

        for (auto ipoint = ypoints.begin() + 1; ipoint < ypoints.end(); ++ipoint) {
            if (*ipoint < *(ipoint - 1))
                verror("\"y\" is not sorted");
        }

        // count the number of matches
        auto ix = xpoints.begin();
        auto iy = ypoints.begin();

        while (ix < xpoints.end() && iy < ypoints.end()) {
            while (ix < xpoints.end() && *ix < *iy && !ix->match(*iy)) {
                if (x2y.empty() || (unsigned)x2y.back().first != ix - xpoints.begin())
                    x2y.push_back({ix - xpoints.begin(), -1});     // no match for x: x -> -1
                ++ix;
            }

            if (ix >= xpoints.end())
                break;

            while (iy < ypoints.end() && *iy < *ix && !ix->match(*iy))
                ++iy;

            if (iy >= ypoints.end())
                break;

            if (ix->match(*iy)) {
                x2y.push_back({ix - xpoints.begin(), iy - ypoints.begin()});   // there is match: x -> y

                if (ix->timestamp.refcount() == EMRTimeStamp::NA_REFCOUNT && iy->timestamp.refcount() == EMRTimeStamp::NA_REFCOUNT) {
                    ++ix;
                    ++iy;
                } else {
                    if (ix->timestamp.refcount() != EMRTimeStamp::NA_REFCOUNT)
                        ++ix;
                    if (iy->timestamp.refcount() != EMRTimeStamp::NA_REFCOUNT)
                        ++iy;
                }
            }
        }

        while (ix < xpoints.end()) {
            x2y.push_back({ix - xpoints.begin(), -1});     // no match for x: x -> -1
            ++ix;
        }

        // pack the answer
        SEXP ranswer, rcolnames, rrownames;
        unsigned ymeta_col_offset = NRPoint::REF;

        {
            SEXP rcolnames = getAttrib(_y, R_NamesSymbol);
            if (xlength(rcolnames) >= NRPoint::REF && !strcmp(CHAR(STRING_ELT(rcolnames, NRPoint::REF)), NRPoint::COL_NAMES[NRPoint::REF])) // is reference column present in y?
                ++ymeta_col_offset;
        }

        uint64_t num_cols = xlength(_x) + xlength(_y) - ymeta_col_offset;
        vector<SEXP> rsrc_cols(num_cols);
        vector<SEXP> rtgt_cols(num_cols);

        rprotect(ranswer = RSaneAllocVector(VECSXP, num_cols));

        copyMostAttrib(_y, ranswer);
        copyMostAttrib(_x, ranswer);

        rprotect(rcolnames = RSaneAllocVector(STRSXP, num_cols));
        rprotect(rrownames = RSaneAllocVector(INTSXP, x2y.size()));

        for (uint64_t i = 0; i < (uint64_t)xlength(_x); ++i) {
            rsrc_cols[i] = VECTOR_ELT(_x, i);
            rprotect(rtgt_cols[i] = RSaneAllocVector(TYPEOF(rsrc_cols[i]), x2y.size()));
            copyMostAttrib(VECTOR_ELT(_x, i), rtgt_cols[i]);
            SET_STRING_ELT(rcolnames, i, STRING_ELT(getAttrib(_x, R_NamesSymbol), i));
        }

        for (uint64_t i = ymeta_col_offset; i < (uint64_t)xlength(_y); ++i) {
            uint64_t idx = xlength(_x) + i - ymeta_col_offset;
            rsrc_cols[idx] = VECTOR_ELT(_y, i);
            rprotect(rtgt_cols[idx] = RSaneAllocVector(TYPEOF(rsrc_cols[idx]), x2y.size()));
            copyMostAttrib(VECTOR_ELT(_y, i), rtgt_cols[idx]);
            SET_STRING_ELT(rcolnames, idx, STRING_ELT(getAttrib(_y, R_NamesSymbol), i));
        }

        for (uint64_t i = 0; i < num_cols; ++i) {
            bool from_x = i < (uint64_t)xlength(_x);

            if (isInteger(rsrc_cols[i]) || isFactor(rsrc_cols[i])) {
                int *src_vals = INTEGER(rsrc_cols[i]);
                int *tgt_vals = INTEGER(rtgt_cols[i]);
                for (auto ix2y = x2y.begin(); ix2y < x2y.end(); ++ix2y) {
                    int idx = from_x ? ix2y->first : ix2y->second;
                    tgt_vals[ix2y - x2y.begin()] = idx == -1 ? NA_INTEGER : src_vals[idx];
                }
            } else if (isReal(rsrc_cols[i])) {
                double *src_vals = REAL(rsrc_cols[i]);
                double *tgt_vals = REAL(rtgt_cols[i]);
                for (auto ix2y = x2y.begin(); ix2y < x2y.end(); ++ix2y) {
                    int idx = from_x ? ix2y->first : ix2y->second;
                    tgt_vals[ix2y - x2y.begin()] = idx == -1 ? NA_REAL : src_vals[idx];
                }
            } else if (isLogical(rsrc_cols[i])) {
                int *src_vals = LOGICAL(rsrc_cols[i]);
                int *tgt_vals = LOGICAL(rtgt_cols[i]);
                for (auto ix2y = x2y.begin(); ix2y < x2y.end(); ++ix2y) {
                    int idx = from_x ? ix2y->first : ix2y->second;
                    tgt_vals[ix2y - x2y.begin()] = idx == -1 ? NA_LOGICAL : src_vals[idx];
                }
            } else if (isString(rsrc_cols[i])) {
                for (auto ix2y = x2y.begin(); ix2y < x2y.end(); ++ix2y) {
                    int idx = from_x ? ix2y->first : ix2y->second;
                    SET_STRING_ELT(rtgt_cols[i], ix2y - x2y.begin(), idx == -1 ? NA_STRING : STRING_ELT(rsrc_cols[i], idx));
                }
            } else
                verror("Unsupported column type \"%s\" found in a data frame of %s", type2char(TYPEOF(rsrc_cols[i])), from_x ? "x" : "y");
        }

        for (uint64_t i = 0; i < x2y.size(); ++i)
            INTEGER(rrownames)[i] = i + 1;

        for (uint64_t i = 0; i < (uint64_t)xlength(_x); ++i)
            SET_VECTOR_ELT(ranswer, i, rtgt_cols[i]);

        for (uint64_t i = ymeta_col_offset; i < (uint64_t)xlength(_y); ++i) {
            uint64_t idx = xlength(_x) + i - ymeta_col_offset;
            SET_VECTOR_ELT(ranswer, idx, rtgt_cols[idx]);
        }

        setAttrib(ranswer, R_NamesSymbol, rcolnames);
        setAttrib(ranswer, R_RowNamesSymbol, rrownames);

        rreturn(ranswer);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	rreturn(R_NilValue);
}

}

