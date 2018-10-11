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
constexpr auto geom_join(const G1 &g1, const G2 &g2) {
  return [g1, g2](auto begin, auto end) {
    auto g1_fn{g1(begin, end)};
    auto g2_fn{g2(begin, end)};
    return [begin, end, g1_fn, g2_fn](emp::graphics::Graphics &g) {
      g1_fn(g);
      g2_fn(g);
    };
  };
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

template <typename S1, typename S2>
constexpr auto join_scales(const S1 &s1, const S2 &s2) {
  return [s1, s2](auto begin, auto end) {
    auto s1_r = s1(begin, end);
    auto s2_r = s2(std::begin(s1_r.first), std::end(s1_r.first));

    return [begin, end, s1_fn = s1_r.second, s2_r](emp::graphics::Graphics &g) {
      s1_r.second(g);
      s2_r.second(g);
    };
  };
}

template <typename ATTR>
auto LinearScale(const ATTR &, float dest_min, float dest_max) {
  return [dest_min, dest_max](auto begin, auto end) {
    return [dest_min, dest_max, begin = begin,
            end = end](emp::graphics::Graphics &g) {
      using iter_type = decltype(begin);
      using value_type = typename std::iterator_traits<iter_type>::value_type;
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
    };
  };
}

template <typename X, typename Y>
auto Scatter2D(
  const X &, const Y &,
  const emp::graphics::Mesh &mesh = emp::graphics::Mesh::Polygon(5)) {
  return [mesh](auto begin, auto end) {
    return [begin = begin, end = end, mesh](emp::graphics::Graphics &g) {
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
  };
}

template <typename X, typename Y>
auto Line2D(const X &, const Y &) {
  return [](auto begin, auto end) {
    return [begin, end](emp::graphics::Graphics &g) {
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
    };
  };
}

struct is_a_geom {};
struct is_a_scale {};

template <typename ITER_TYPE, typename ELEMENT>
struct which_plot_element
  : std::conditional_t<
      std::is_same<decltype(std::declval<ELEMENT>()(std::declval<ITER_TYPE>(),
                                                    std::declval<ITER_TYPE>())(
                     std::declval<emp::graphics::Graphics &>())),
                   void>::value,
      is_a_geom, is_a_scale> {};

// TODO: graphics (frame?) should know it's size & allow stacking transforms

template <typename DATA_ITER, typename AES, typename GEOM, typename SCALE>
class CPPPlot {
  private:
  DATA_ITER begin, end;
  AES aes_mapping;
  GEOM geometry;
  SCALE scale;

  public:
  using data_type = typename std::iterator_traits<DATA_ITER>::value_type;
  using aes_mapping_data_type =
    decltype(aes_mapping(std::declval<data_type>()));

  private:
  using aes_mapping_type = std::vector<aes_mapping_data_type>;
  using aes_iterator = typename aes_mapping_type::iterator;

  public:
  using scale_mapping_type = decltype(std::declval<SCALE>()(
    std::declval<aes_iterator>(),  // begin
    std::declval<aes_iterator>()  // end
    )(std::declval<emp::graphics::Graphics &>()));

  constexpr CPPPlot(DATA_ITER begin, DATA_ITER end, AES aes_mapping,
                    GEOM geometry, SCALE scale)
    : begin(begin),
      end(end),
      aes_mapping(aes_mapping),
      geometry(geometry),
      scale(scale) {}

  void operator()(emp::graphics::Graphics &g) const {
    aes_mapping_type aes;

    std::transform(begin, end, std::back_inserter(aes), aes_mapping);

    auto scaled = scale(g, std::begin(aes), std::end(aes));

    geometry(g, std::begin(scaled), std::end(scaled));
  }

  private:
  template <typename E>
  auto impl_append(const E &element, const is_a_geom &) const {
    auto new_geom = geom_join(geometry, element);
    return CPPPlot<DATA_ITER, AES, decltype(new_geom), SCALE>{
      begin, end, aes_mapping, new_geom, scale};
  }

  template <typename E>
  auto impl_append(const E &element, const is_a_scale &) const {
    auto new_scale = scale_join(scale, element);
    return CPPPlot<DATA_ITER, AES, GEOM, decltype(new_scale)>{
      begin, end, aes_mapping, geometry, new_scale};
  }

  public:
  template <typename E>
  auto append(const E &element) const {
    // TODO: replace aes_mapping_type with scale_mapping_type
    return impl_append(
      element,
      which_plot_element<typename std::vector<aes_mapping_data_type>::iterator,
                         E>{});
  }
};

struct NopGeom : geom_tag {
  template <typename DATA_ITER>
  auto operator()(DATA_ITER begin, DATA_ITER end) const {
    return [](auto &&...) {};
  }
};

struct NopScale : scale_tag {
  template <typename DATA_ITER>
  auto operator()(DATA_ITER begin, DATA_ITER end) const {
    return [begin, end](auto &&...) {
      std::vector<typename std::iterator_traits<DATA_ITER>::value_type> data;
      for (; begin != end; ++begin) data.emplace_back(*begin);
      return data;
    };
  }
};

template <typename DATA_ITER, typename... AES>
auto cppplot(DATA_ITER begin, DATA_ITER end, AES &&... aes) {
  auto aes_attrs = emp::tools::MakeAttrs(std::forward<AES>(aes)...);

  return CPPPlot<DATA_ITER, decltype(aes_attrs), NopGeom, NopScale>(
    begin, end, aes_attrs, {}, {});
}

template <typename DATA_ITER, typename AES, typename GEOM, typename SCALE,
          typename E>
auto operator+(const CPPPlot<DATA_ITER, AES, GEOM, SCALE> &cppplot,
               const E &element) {
  return cppplot.append(element);
}
template <typename DATA_ITER, typename AES, typename GEOM, typename SCALE,
          typename E>
auto operator+(const E &element,
               const CPPPlot<DATA_ITER, AES, GEOM, SCALE> &cppplot) {
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
    auto p = cppplot(std::begin(pts), std::end(pts),
                     attributes::X = &pt_type::x, attributes::Y = &pt_type::y,
                     attributes::Color = Color::red(), attributes::Size = 1) +
             LinearScale(attributes::X, region.min.x(), region.max.x()) +
             LinearScale(attributes::Y, region.min.y(), region.max.y()) +
             Scatter2D(attributes::X, attributes::Y) +
             Line2D(attributes::X, attributes::Y);

    p(g);

    // auto p2 =
    //   cppplot(attributes::X = [](auto &pt) { return -pt.x; },
    //           attributes::Y = &pt_type::y, attributes::Color =
    //           Color::green(), attributes::Size = 1) +
    //   LinearScale<struct attributes::X>{region.min.x(), region.max.x() / 2} +
    //   LinearScale<struct attributes::Y>{region.min.y(), region.max.y() / 2} +
    //   Scatter2D<struct attributes::X, struct attributes::Y>{} +
    //   Line2D<struct attributes::X, struct attributes::Y>{};

    // rbrt.Enable();
    // p2(g, std::begin(pts), std::end(pts));

    // canvas.Enable();
    // g.Texture(rbrt.GetColorTexture())
    //   .Draw({
    //     emp::graphics::Transform =
    //       emp::math::Mat4x4f::Translation(800 / 2, 600 / 2),
    //   })
    //   .Flush();
  });
}
