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

template <typename G1, typename G2>
constexpr auto geom_join(const G1 &g1, const G2 &g2) {
  return [g1, g2](emp::graphics::Graphics &g, auto begin, auto end) {
    g1(g, begin, end);
    g2(g, begin, end);
  };
}

template <typename S1, typename S2>
constexpr auto scale_join(const S1 &s1, const S2 &s2) {
  return [s1, s2](emp::graphics::Graphics &g, auto begin, auto end) {
    auto post = s1(g, begin, end);
    return s2(g, std::begin(post), std::end(post));
  };
}

namespace emp__impl_call_or_get {

  template <typename VALUE_FUNC, typename... ARGS>
  auto CallOrGet(const std::true_type &,  // is_invocable
                 VALUE_FUNC &&value_func, ARGS &&... args) {
    return std::forward<VALUE_FUNC>(value_func)(std::forward<ARGS>(args)...);
  }

  template <typename VALUE_FUNC, typename... ARGS>
  decltype(auto) CallOrGet(const std::false_type &,  // is_invocable
                           VALUE_FUNC &&value_func, ARGS &&... args) {
    return std::forward<VALUE_FUNC>(value_func);
  }
}  // namespace emp__impl_call_or_get

template <typename VALUE_FUNC, typename... ARGS>
decltype(auto) CallOrGet(VALUE_FUNC &&value_func, ARGS &&... args) {
  return emp__impl_call_or_get::CallOrGet(
    emp::is_invocable<VALUE_FUNC, ARGS...>{},
    std::forward<VALUE_FUNC>(value_func), std::forward<ARGS>(args)...);
}

template <typename ATTR, typename DEST_MIN, typename DEST_MAX>
auto LinearScale(const ATTR &, DEST_MIN dest_min, DEST_MAX dest_max) {
  return [dest_min, dest_max](emp::graphics::Graphics &g, auto begin,
                              auto end) {
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

    auto scale_function = [min, max, dest_min = CallOrGet(dest_min, g),
                           dest_max = CallOrGet(dest_max)](const auto &value) {
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
}

template <typename X, typename Y>
auto Scatter2D(
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
auto Line2D(const X &, const Y &) {
  return [](emp::graphics::Graphics &g, auto begin, auto end) {
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
}

struct is_a_geom {};
struct is_a_scale {};

template <typename ITER_TYPE, typename ELEMENT>
struct which_plot_element
  : std::conditional_t<std::is_same<decltype(std::declval<ELEMENT>()(
                                      std::declval<emp::graphics::Graphics &>(),
                                      std::declval<ITER_TYPE>(),  // begin
                                      std::declval<ITER_TYPE>()  // end
                                      )),
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
  using scale_mapping_type =
    decltype(std::declval<SCALE>()(std::declval<emp::graphics::Graphics &>(),
                                   std::declval<aes_iterator>(),  // begin
                                   std::declval<aes_iterator>()  // end
                                   ));
  using scale_mapping_data_type = typename std::iterator_traits<decltype(
    std::begin(std::declval<scale_mapping_type>()))>::value_type;

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
      element, which_plot_element<
                 typename std::vector<scale_mapping_data_type>::iterator, E>{});
  }
};

struct NopGeom {
  template <typename DATA_ITER>
  void operator()(emp::graphics::Graphics &, DATA_ITER begin,
                  DATA_ITER end) const {}
};

struct NopScale {
  template <typename DATA_ITER>
  auto operator()(emp::graphics::Graphics &, DATA_ITER begin,
                  DATA_ITER end) const {
    std::vector<typename std::iterator_traits<DATA_ITER>::value_type> data;
    for (; begin != end; ++begin) data.emplace_back(*begin);
    return data;
  }
};

template <typename DATA_ITER, typename... AES>
auto Plot(DATA_ITER begin, DATA_ITER end, AES &&... aes) {
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

template <typename DATA_ITER, typename AES, typename GEOM, typename SCALE>
emp::graphics::Graphics &operator<<(
  emp::graphics::Graphics &g,
  const CPPPlot<DATA_ITER, AES, GEOM, SCALE> &cppplot) {
  cppplot(g);
  return g;
}

namespace helpers {
  const auto FrameWidth = [] {
    return emp::graphics::RenderTarget::GetCurrentRenderTarget().GetWidth();
  };
  const auto FrameHeight = [] {
    return emp::graphics::RenderTarget::GetCurrentRenderTarget().GetHeight();
  };

  template <typename T>
  struct Ref {
    T &value;
    Ref(T &value) : value(value) {}

    operator T &() { return value; }
    operator const T &() const { return value; }
  };

  template <typename T>
  Ref<T> ref(T &value) {
    return {value};
  }
}  // namespace helpers

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
    float x, y, w;
  };

  std::vector<pt_type> pts;
  for (int i = 0; i < 10; ++i)
    pts.push_back({static_cast<float>(rand()), static_cast<float>(rand()),
                   static_cast<float>(rand())});

  emp::graphics::FramebufferRenderTarget rbrt(canvas.GetWidth() / 2,
                                              canvas.GetHeight() / 2);

  canvas.runForever([&](auto &&) {
    canvas.Enable();
    g.Clear(emp::opengl::Color::grey(0.8));

    g << Plot(std::begin(pts), std::end(pts), attributes::X = &pt_type::x,
              attributes::Y = &pt_type::y, attributes::Color = Color::black(),
              attributes::Size = 1) +
           LinearScale(attributes::X, 0, helpers::FrameWidth) +
           LinearScale(attributes::Y, 0, helpers::FrameHeight) +
           Scatter2D(attributes::X, attributes::Y) +
           Line2D(attributes::X, attributes::Y);
  });
}
