#ifndef EMP_PLOTTING_ATTRIBUTES
#define EMP_PLOTTING_ATTRIBUTES

#include "tools/attrs.h"

namespace emp {
  namespace plot {
    namespace attributes {
      DEFINE_ATTR(Xyz);
      DEFINE_ATTR(X);
      DEFINE_ATTR(Y);
      DEFINE_ATTR(Z);
      DEFINE_ATTR(W);
      DEFINE_ATTR(Size);
      
      DEFINE_ATTR(XyzScaled);
      DEFINE_ATTR(PointSize);
      DEFINE_ATTR(StrokeWeight);
      DEFINE_ATTR(Color);
    }  // namespace attributes
  }  // namespace plot
}  // namespace emp

#endif