#include "naryn.h"
#include "NRDb.h"
#include "NRIdsIterator.h"
#include "NRTimesIterator.h"
#include "NRIteratorFilterItem.h"

#ifdef length
#undef length
#endif
#ifdef error
#undef error
#endif

const char *NRIteratorFilterItem::OP_NAMES[NUM_OPS] = { "NONE", "OR", "AND" };

bool NRIteratorFilterItem::is_passed_node(const NRPoint &point)
{
    if (m_op == OR) {
        NRTimeStamp::Hour hour = point.timestamp.hour();

        // optimization: call is_passed only if point is greater or equal to child's jumpto
        if (((int)point.id > (int)m_child[0]->m_jumpto.id ||
             point.id == m_child[0]->m_jumpto.id && hour >= m_child[0]->m_jumpto.timestamp.hour()) && m_child[0]->is_passed(point) ||
            ((int)point.id > (int)m_child[1]->m_jumpto.id ||
             point.id == m_child[1]->m_jumpto.id && hour >= m_child[1]->m_jumpto.timestamp.hour()) && m_child[1]->is_passed(point))
            return true;

        m_jumpto = m_child[0]->m_jumpto < m_child[1]->m_jumpto ? m_child[0]->m_jumpto : m_child[1]->m_jumpto;
        return false;
    }

    if (m_op == AND) {
        bool passed[2] = { m_child[0]->is_passed(point), m_child[1]->is_passed(point) };

        if (passed[0] && passed[1])
            return true;

        if (passed[0])
            m_child[0]->m_jumpto.init(point.id, NRTimeStamp(point.timestamp.hour(), (NRTimeStamp::Refcount)-1));

        if (passed[1])
            m_child[1]->m_jumpto.init(point.id, NRTimeStamp(point.timestamp.hour(), (NRTimeStamp::Refcount)-1));

        int idx = m_child[0]->m_jumpto < m_child[1]->m_jumpto ? 1 : 0;

        while (1) {
            if (m_child[0]->m_jumpto == m_child[1]->m_jumpto || m_child[idx]->m_jumpto.id == (unsigned)-1 || m_child[1 - idx]->is_passed(m_child[idx]->m_jumpto)) {
                m_jumpto = m_child[idx]->m_jumpto;
                return false;
            }
            idx = 1 - idx;
        }
    }

    return false;
}

bool NRIteratorFilterItem::is_passed_leaf(const NRPoint &point)
{
    //        ITERATOR POINT
    //              *
    //              |        +sshift         +eshift
    //              ------------------------------
    //                          |                |
    //                          v                v
    //                          ******************
    //                           FILTER INTERVAL

    NRTimeStamp::Hour point_hour = point.timestamp.hour();

    if (m_is_not) {
        if ((int)m_true_upto.id > (int)point.id || m_true_upto.id == point.id && m_true_upto.timestamp.hour() > point_hour)
            return true;

        m_is_not = false;

        bool res = is_passed_leaf(point);

        if (res) {
            // the final answer is FALSE => set jumpto

            if (m_keepref)
                // if keepref is TRUE stay at the same time: other references with identical time might pass the filter
                m_jumpto.init(point.id, NRTimeStamp(point_hour, (NRTimeStamp::Refcount)-1));
            else {
                // if keepref is FALSE => find the next point that passes the filter, i.e. is_passed returns false when m_is_not is inversed.
                // Last is_passed returned true => we found P' within the filter interval. The filter interval of the point we seek should not
                // include P' (otherwise is_passed will again return true).
                // The first point which filter interval does not contain P' is at P+1-sshift
                // (it would be translated to a filter interval of [P+1, P+1-sshift+eshift]
                unsigned id = m_itr->point().id;

                while (1) {
                    int hour;

                    if (dynamic_cast<NRTimesIterator *>(m_itr))
                        hour = ((NRTimesIterator *)m_itr)->cur_interval().etime + 1 - m_sshift;
                    else
                        hour = (int)m_itr->point().timestamp.hour() + 1 - m_sshift;

                    if (hour > m_etime || dynamic_cast<NRIdsIterator *>(m_itr)) {
                        ++id;
                        hour = m_stime;
                    }
                    if (!is_passed_leaf(NRPoint(id, NRTimeStamp(hour, (NRTimeStamp::Refcount)-1)))) {
                        m_jumpto.init(id, NRTimeStamp(hour, (NRTimeStamp::Refcount)-1));
                        break;
                    }
                }
            }
        } else {
            m_true_upto = m_jumpto;
            m_jumpto = point;
        }

        m_is_not = true;
        return !res;
    }

    int sinterv = (int)point_hour + m_sshift;
    int einterv = (int)point_hour + m_eshift;

    if (!m_itr_started) {
        m_itr_started = true;

        if (m_etime + m_eshift < 0 || m_stime + m_sshift > (int)NRTimeStamp::MAX_HOUR)
            return filtered_end();

        m_itr->begin();

        // current point exceeds filter interval
        if (!m_itr->isend() && (m_itr->point().id > point.id || m_itr->point().id == point.id && (int)m_itr->point().timestamp.hour() > einterv))
            return filtered_next();
    }

    // current point preceeds filter interval => jump to the beginning of filter interval
    if (m_itr->point().id < point.id || m_itr->point().id == point.id && (int)m_itr->point().timestamp.hour() < sinterv) {
        if (!m_itr->next(NRPoint(point.id, NRTimeStamp(max(sinterv, 0), (NRTimeStamp::Refcount)-1))))
            return filtered_end();
    }

    // current point exceeds filter interval
    if (m_itr->point().id > point.id || (int)m_itr->point().timestamp.hour() > einterv)
        return filtered_next();

    // now current point falls within the filter interval: need only to check that references match

    if (m_itr->point().timestamp.refcount() == (NRTimeStamp::Refcount)-1 || point.timestamp.refcount() == (NRTimeStamp::Refcount)-1)
        return true;

    while (m_itr->point().id == point.id && (int)m_itr->point().timestamp.hour() <= einterv && m_itr->point().timestamp.refcount() <= point.timestamp.refcount()) {
        if (m_itr->point().timestamp.refcount() == point.timestamp.refcount())
            return true;

        // references do not match:
        // track values are sorted also by reference => move the filter iterator up until it surpasses the reference, time or id, or a new match is found
        if (!m_itr->next())
            return filtered_end();
    }

    // current point is still within the filter interval
    if (m_itr->point().id == point.id && (int)m_itr->point().timestamp.hour() <= einterv) {
        m_jumpto.init(point.id, NRTimeStamp(point.timestamp.hour(), (NRTimeStamp::Refcount)-1));
        return false;
    }

    return filtered_next();
}

void NRIteratorFilterItem::debug_print(int depth)
{
    if (is_leaf()) {
        printf("%*sNOT:     %d\n", depth * 2, "", m_is_not);
        printf("%*sSSHIFT:  %d\n", depth * 2, "", m_sshift);
        printf("%*sESHIFT:  %d\n", depth * 2, "", m_eshift);
        printf("%*sKEEPREF: %d\n", depth * 2, "", m_keepref);
        printf("%*s%s\n", depth * 2, "", m_itr->tostr().c_str());
    } else
        printf("%*s%s\n", depth * 2, "", OP_NAMES[m_op]);
}

