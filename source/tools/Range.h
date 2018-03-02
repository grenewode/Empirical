/**
 *  @note This file is part of Empirical, https://github.com/devosoft/Empirical
 *  @copyright Copyright (C) Michigan State University, MIT Software license; see doc/LICENSE.md
 *  @date 2016-2017
 *
 *  @file  Range.h
 *  @brief A simple way to track value ranges
 *  @note Status: BETA
 */

#ifndef EMP_RANGE_H
#define EMP_RANGE_H

#include <limits>

#include "../base/assert.h"
#include "../base/vector.h"

namespace emp {

  /// A range of values from a lower limit to and upper limit, of any provided type.
  template <typename T>
  class Range {
  private:
    T lower;  ///< Beginning of range, inclusive.
    T upper;  ///< End of range, inclusive.

  public:
    Range() : lower(std::numeric_limits<T>::min()), upper(std::numeric_limits<T>::max()) { ; }
    Range(T _l, T _u) : lower(_l), upper(_u) { emp_assert(_l < _u); }

    T GetLower() const { return lower; }
    T GetUpper() const { return upper; }

    size_t CalcBin(T value, size_t num_bins) const {
      return ((double) (value - lower)) / ((double) (upper - lower)) * num_bins;
    }

    Range & operator=(const Range&) = default;
    bool operator==(const Range& _in) const { return lower==_in.lower && upper==_in.upper; }
    bool operator!=(const Range& _in) const { return lower!=_in.lower || upper!=_in.upper; }

    void SetLower(T l) { lower = l; }
    void SetUpper(T u) { upper = u; }
    void Set(T _l, T _u) { emp_assert(_l < _u); lower = _l; upper = _u; }

    void SetMaxLower() { lower = std::numeric_limits<T>::min(); }
    void SetMaxUpper() { upper = std::numeric_limits<T>::max(); }

    /// Determine if a provided value is in the range.
    bool Valid(T value) { return value >= lower && value <= upper; }

    /// Force a value into range
    T Limit(T _in) { return (_in < lower) ? lower : ((_in > upper) ? upper : _in); }

    /// Produce a vector that spreads values evenly across the range.
    emp::vector<T> Spread(size_t s) {
      emp_assert(s >= 1);
      emp::vector<T> out(s);
      out[0] = lower;
      if (s > 1) {
        T range = upper - lower;
        for (size_t i = 1; i < s; i++) {
          out[i] = lower + (T) ((((double) i) / (double)(s-1)) * range);
        }
      }
      return out;
    }
  };

  /// Build a new range with auto-detected type.
  template <typename T> Range<T> MakeRange(T _l, T _u) { return Range<T>(_l,_u); }

  /// Build a new range of type int.
  inline Range<int> IntRange(int _l, int _u) { return Range<int>(_l,_u); }

  /// Build a new range of type double.
  inline Range<double> DRange(double _l, double _u) { return Range<double>(_l,_u); }
}

#endif
