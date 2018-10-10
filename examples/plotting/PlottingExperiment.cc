//  This file is part of Empirical, https://github.com/devosoft/Empirical
//  Copyright (C) Michigan State University, 2016-2017.
//  Released under the MIT Software license; see doc/LICENSE
//
//

#include <algorithm>
#include <iostream>
#include <iterator>
#include <limits>

#include "math/LinAlg.h"
#include "math/consts.h"
#include "opengl/defaultShaders.h"
#include "opengl/glcanvas.h"
#include "plot/line.h"
#include "plot/scales.h"
#include "plot/scatter.h"
#include "scenegraph/camera.h"
#include "scenegraph/core.h"
#include "tools/attrs.h"
// #include "scenegraph/shapes.h"
#include "plot/flow.h"
#include "scenegraph/transform.h"

#include "scenegraph/rendering.h"

#include <chrono>
#include <cstdlib>

struct geom_tag {};
struct scale_tag {};

template <typename G1, typename G2>
struct JoinGeometries : geom_tag {
  G1 first;
  G2 second;

  JoinGeometries(G1 first, G2 second) : first(first), second(second) {}

  template <typename DATA_ITER>
  void operator()(emp::graphics::Graphics &g, DATA_ITER begin,
                  DATA_ITER end) const {
    first(g, begin, end);
    second(g, begin, end);
  }
};

template <typename G1, typename G2>
JoinGeometries<G1, G2> geom_join(const G1 &g1, const G2 &g2) {
  return {g1, g2};
}

template <typename S1, typename S2>
struct JoinScales : scale_tag {
  S1 first;
  S2 second;

  JoinScales(S1 first, S2 second) : first(first), second(second) {}

  template <typename DATA_ITER>
  auto operator()(emp::graphics::Graphics &g, DATA_ITER begin,
                  DATA_ITER end) const {
    auto scaled = first(g, begin, end);
    return second(g, std::begin(scaled), std::end(scaled));
  }
};

template <typename S1, typename S2>
constexpr JoinScales<S1, S2> scale_join(const S1 &s1, const S2 &s2) {
  return {s1, s2};
}

template <typename ATTR>
struct LinearScale : scale_tag {
  float dest_min, dest_max;

  constexpr LinearScale(float dest_min, float dest_max)
    : dest_min(dest_min), dest_max(dest_max) {}

  template <typename DATA_ITER>
  auto operator()(emp::graphics::Graphics &g, DATA_ITER begin,
                  DATA_ITER end) const {
    using value_type = typename std::iterator_traits<DATA_ITER>::value_type;
    using scaled_value_type = decltype(ATTR::Get(std::declval<value_type>()));

    scaled_value_type min{
      std::numeric_limits<std::decay_t<scaled_value_type>>::max()};
    scaled_value_type max{
      std::numeric_limits<std::decay_t<scaled_value_type>>::lowest()};

    for (auto iter{begin}; iter != end; ++iter) {
      min = std::min(min, ATTR::Get(*iter));
      max = std::max(max, ATTR::Get(*iter));
    }

    auto scale_function = [=](const auto &value) {
      auto scaled{((ATTR::Get(value) - min) / (max - min)) *
                    (dest_max - dest_min) +
                  dest_min};

      return Merge(ATTR::Make(scaled), value);
    };

    using result_type = decltype(scale_function(std::declval<value_type>()));

    std::vector<result_type> results;
    std::transform(begin, end, std::back_inserter(results), scale_function);

    return results;
  }
};

template <typename X, typename Y>
struct Scatter2D : geom_tag {
  private:
  emp::graphics::Mesh point_mesh;

  public:
  Scatter2D(const emp::graphics::Mesh &mesh = emp::graphics::Mesh::Polygon(5))
    : point_mesh(mesh) {}

  template <typename DATA_ITER>
  void operator()(emp::graphics::Graphics &g, DATA_ITER begin,
                  DATA_ITER end) const {
    auto pen = g.Fill(point_mesh);

    for (; begin != end; ++begin) {
      auto color{emp::plot::attributes::Color::Get(*begin)};
      auto x{emp::plot::attributes::X::Get(*begin)};
      auto y{emp::plot::attributes::Y::Get(*begin)};
      auto size{
        emp::plot::attributes::Size::GetOrElse(*begin, [] { return 1; })};

      pen.Draw({
        emp::graphics::Fill = color,
        emp::graphics::Transform = emp::math::Mat4x4f::Translation(x, y) *
                                   emp::math::Mat4x4f::Scale(size),
      });
    }

    pen.Flush();
  }
};

template <typename X, typename Y>
auto FnScatter2D(
  const X &, const Y &,
  const emp::graphics::Mesh &mesh = emp::graphics::Mesh::Polygon(5)) {
  return [mesh](emp::graphics::Graphics &g, auto begin, auto end) {
    auto pen = g.Fill(mesh);

    for (; begin != end; ++begin) {
      auto color{emp::plot::attributes::Color::Get(*begin)};
      auto x{emp::plot::attributes::X::Get(*begin)};
      auto y{emp::plot::attributes::Y::Get(*begin)};
      auto size{
        emp::plot::attributes::Size::GetOrElse(*begin, [] { return 1; })};

      pen.Draw({
        emp::graphics::Fill = color,
        emp::graphics::Transform = emp::math::Mat4x4f::Translation(x, y) *
                                   emp::math::Mat4x4f::Scale(size),
      });
    }

    pen.Flush();
  };
}

template <typename X, typename Y>
struct Line2D : geom_tag {
  public:
  template <typename DATA_ITER>
  void operator()(emp::graphics::Graphics &g, DATA_ITER begin,
                  DATA_ITER end) const {
    g.Line(begin, end,
           MakeAttrs(
             emp::graphics::Vertex =
               [](auto &pt) {
                 return emp::math::Vec2f{
                   X::Get(pt),
                   Y::Get(pt),
                 };
               },
             emp::graphics::Stroke = emp::plot::attributes::Color::Get,
             emp::graphics::StrokeWeight = emp::plot::attributes::Size::Get))
      .Draw(emp::graphics::Transform = emp::math::Mat4x4f::Identity())
      .Flush();
  }
};

// TODO: graphics (frame?) should know it's size & allow stacking transforms

template <typename AES, typename GEOM, typename SCALE>
class CPPPlot {
  private:
  AES aes_mapping;
  GEOM geometry;
  SCALE scale;

  public:
  constexpr CPPPlot(AES aes_mapping, GEOM geometry, SCALE scale)
    : aes_mapping(aes_mapping), geometry(geometry), scale(scale) {}

  template <typename DATA_ITER>
  void operator()(emp::graphics::Graphics &g, DATA_ITER begin,
                  DATA_ITER end) const {
    using data_type = typename std::iterator_traits<DATA_ITER>::value_type;
    std::vector<decltype(aes_mapping(std::declval<data_type>()))> aes;

    std::transform(begin, end, std::back_inserter(aes), aes_mapping);

    auto scaled = scale(g, std::begin(aes), std::end(aes));

    geometry(g, std::begin(scaled), std::end(scaled));
  }

  private:
  template <typename E>
  auto impl_append(const E &element, const std::true_type &is_a_geom) const {
    auto new_geom = geom_join(geometry, element);
    return CPPPlot<AES, decltype(new_geom), SCALE>{aes_mapping, new_geom,
                                                   scale};
  }

  template <typename E>
  auto impl_append(const E &element, const std::false_type &is_a_geom) const {
    auto new_scale = scale_join(scale, element);
    return CPPPlot<AES, GEOM, decltype(new_scale)>{aes_mapping, geometry,
                                                   new_scale};
  }

  public:
  template <typename E>
  auto append(const E &element) const {
    return impl_append(element, std::is_base_of<geom_tag, E>{});
  }
};

struct NopGeom : geom_tag {
  template <typename DATA_ITER>
  void operator()(emp::graphics::Graphics &g, DATA_ITER begin,
                  DATA_ITER end) const {}
};

struct NopScale : scale_tag {
  template <typename DATA_ITER>
  auto operator()(emp::graphics::Graphics &g, DATA_ITER begin,
                  DATA_ITER end) const {
    std::vector<typename std::iterator_traits<DATA_ITER>::value_type> data;
    for (; begin != end; ++begin) data.emplace_back(*begin);
    return data;
  }
};

template <typename... AES>
auto cppplot(AES &&... aes) {
  auto aes_attrs = emp::tools::MakeAttrs(std::forward<AES>(aes)...);

  return CPPPlot<decltype(aes_attrs), NopGeom, NopScale>(aes_attrs, {}, {});
}

template <typename AES, typename GEOM, typename SCALE, typename E>
auto operator+(const CPPPlot<AES, GEOM, SCALE> &cppplot, const E &element) {
  return cppplot.append(element);
}
template <typename AES, typename GEOM, typename SCALE, typename E>
auto operator+(const E &element, const CPPPlot<AES, GEOM, SCALE> &cppplot) {
  return cppplot.append(element);
}

emp::scenegraph::FreeType ft;

int main(int argc, char *argv[]) {
  using namespace emp::plot;
  using namespace emp::opengl;

  emp::opengl::GLCanvas canvas(800, 800);
  emp::opengl::shaders::LoadShaders();
  emp::Resources<emp::scenegraph::FontFace>::Add("Roboto", [] {
    auto font = ft.load("Assets/RobotoMono-Regular.ttf");
    font.SetFreeTypePixelSize(0, 64);
    font.BulidAsciiAtlas();
    return font;
  });

  emp::scenegraph::OrthoCamera camera(-1, 1);
  camera.Enable();
  emp::scenegraph::SimpleEye eye;
  eye.Enable();

  emp::graphics::Graphics g("Roboto");
  canvas.on_resize_event.bind([&](auto &canvas, auto width, auto height) {
    // camera->SetViewbox(canvas.GetRegion().AddDimension(-100, 100));
  });

  struct pt_type {
    float x, y;
  };

  std::vector<pt_type> pts;
  for (int i = 0; i < 10; ++i)
    pts.push_back({static_cast<float>(rand()), static_cast<float>(rand())});

  emp::graphics::FramebufferRenderTarget rbrt(canvas.GetWidth() / 2,
                                              canvas.GetHeight() / 2);

  canvas.runForever([&](auto &&) {
    canvas.Enable();
    g.Clear(emp::opengl::Color::grey(0.8));

    auto region = canvas.GetRegion();
    auto p = cppplot(attributes::X = &pt_type::x, attributes::Y = &pt_type::y,
                     attributes::Color = Color::red(), attributes::Size = 1) +
             LinearScale<struct attributes::X>{region.min.x(), region.max.x()} +
             LinearScale<struct attributes::Y>{region.min.y(), region.max.y()} +
             Scatter2D<struct attributes::X, struct attributes::Y>{} +
             Line2D<struct attributes::X, struct attributes::Y>{};

    p(g, std::begin(pts), std::end(pts));

    auto p2 =
      cppplot(attributes::X = [](auto &pt) { return -pt.x; },
              attributes::Y = &pt_type::y, attributes::Color = Color::green(),
              attributes::Size = 1) +
      LinearScale<struct attributes::X>{region.min.x(), region.max.x() / 2} +
      LinearScale<struct attributes::Y>{region.min.y(), region.max.y() / 2} +
      Scatter2D<struct attributes::X, struct attributes::Y>{} +
      Line2D<struct attributes::X, struct attributes::Y>{};

    rbrt.Enable();
    p2(g, std::begin(pts), std::end(pts));

    canvas.Enable();
    g.Texture(rbrt.GetColorTexture())
      .Draw({
        emp::graphics::Transform =
          emp::math::Mat4x4f::Translation(800 / 2, 600 / 2),
      })
      .Flush();
  });
}
