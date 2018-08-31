#ifndef PLOT_LINE_H
#define PLOT_LINE_H

#include "flow.h"
#include "math/LinAlg.h"
#include "opengl/color.h"
#include "opengl/glcanvas.h"
#include "opengl/glwrap.h"
#include "opengl/shaders.h"
#include "scales.h"
#include "scenegraph/camera.h"
#include "scenegraph/core.h"
#include "tools/attrs.h"
#include "tools/resources.h"

// Based on
// https://blog.mapbox.com/drawing-antialiased-lines-with-opengl-8766f34192dc

namespace emp {
  namespace plot {
    template <size_t D>
    class Line : public scenegraph::Node<D> {
      private:
      std::vector<std::tuple<math::Vec2f, opengl::Color, float>> points;

      public:
      Line() {}
      virtual ~Line() {}

      void RenderRelative(graphics::Graphics &g, const math::Mat4x4f &transform,
                          const math::Vec<float, D> &allocated_size) {
        using namespace emp::math;
        using namespace emp::opengl;

        g.Line(
           points.begin(), points.end(),
           MakeAttrs(
             graphics::Vertex = [](auto &pt) { return std::get<0>(pt); },
             graphics::Stroke = [](auto &pt) { return std::get<1>(pt); },
             graphics::StrokeWeight = [](auto &pt) { return std::get<2>(pt); }))
          .Draw(graphics::Transform = transform)
          .Flush();
      }

      template <typename DATA_ITER>
      void operator()(DATA_ITER begin, DATA_ITER end) {
        points.clear();

        for (; begin != end; ++begin) {
          points.emplace_back(plot::attributes::XyzScaled::Get(*begin),
                              graphics::Stroke::Get(*begin),
                              graphics::StrokeWeight::Get(*begin));
        }
      }
    };
  }  // namespace plot
}  // namespace emp

#endif  // PLOT_LINE_H
