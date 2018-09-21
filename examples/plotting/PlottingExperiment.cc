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

  template <typename DATA_ITER>
  void operator()(emp::graphics::Graphics &g, DATA_ITER begin,
                  DATA_ITER end) const {
    first(g, begin, end);
    second(g, begin, end);
  }
};

template <typename G1, typename G2>
JoinGeometries<G1, G2> geom_join(const G1& g1, const G2& g2) {
  return {g1, g2};
}

template <typename S1, typename S2>
struct JoinScales : scale_tag {
  S1 first;
  S2 second;

  template <typename DATA_ITER>
  void operator()(emp::graphics::Graphics &g, DATA_ITER begin,
                  DATA_ITER end) const {
    auto scaled = first(g, begin, end);
    second(g, std::begin(scaled), std::end(scaled));
  }
};

template <typename S1, typename S2>
JoinGeometries<S1, S2> scale_join(const S1& s1, const S2& s2) {
  return {s1, s2};
}

template<typename A>
struct LinearScale {
  template <typename DATA_ITER>
  void operator()(emp::graphics::Graphics &g, DATA_ITER begin,
                  DATA_ITER end) const {
    auto minmax = std::minmax_element(begin, end);
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
    std::vector<decltype(aes_mapping(*begin))> aes;

    std::transform(begin, end, std::back_inserter(aes), aes_mapping);

    auto scaled = scale(std::begin(aes), std::end(aes));

    geometry(g, std::begin(scaled), std::end(scaled));
  }

  private:
  template <typename E>
  auto impl_append(const E &element, const std::true_type &is_a_geom) {
    auto new_geom = geom_join(geometry, element);
    return CPPPlot<AES, decltype(new_geom), SCALE>{aes_mapping, new_geom,
                                                   scale};
  }

  template <typename E>
  auto impl_append(const E &element, const std::false_type &is_a_geom) {
    auto new_scale = scale_join(scale, element);
    return CPPPlot<AES, GEOM, decltype(new_scale)>{aes_mapping, geometry,
                                                   new_scale};
  }

  public:
  template <typename E>
  auto append(const E &element) {
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
    return std::vector<typename std::iterator_traits<DATA_ITER>::value_type>{
      begin, end};
  }
};

template <typename... AES>
auto cppplot(AES &&... aes) {
  auto aes_attrs = emp::tools::MakeAttrs(std::forward<AES>(aes)...);

  return CPPPlot<decltype(aes_attrs), NopGeom, NopScale>(aes_attrs, {}, {});
}

template<typename AES, typename GEOM, typename SCALE, typename E>
auto operator+(const CPPPlot<AES, GEOM, SCALE>& cppplot, const E& element) {
return cppplot.append(element);
}
template<typename AES, typename GEOM, typename SCALE, typename E>
auto operator+(const E& element, const CPPPlot<AES, GEOM, SCALE>& cppplot) {
return cppplot.append(element);
}

emp::scenegraph::FreeType ft;

int main(int argc, char *argv[]) {

  emp::opengl::GLCanvas canvas(800, 800);
  emp::opengl::shaders::LoadShaders(canvas);
  emp::Resources<emp::scenegraph::FontFace>::Add("Roboto", [] {
    auto font = ft.load("Assets/RobotoMono-Regular.ttf");
    font.SetFreeTypePixelSize(0, 64);
    font.BulidAsciiAtlas();
    return font;
  });


  auto camera =
    std::make_shared<emp::scenegraph::OrthoCamera>(canvas.getRegion().AddDimension(-100,
    100));
  auto eye = std::make_shared<emp::scenegraph::SimpleEye>();

  emp::graphics::Graphics g(canvas, "Roboto", camera, eye);
  canvas.on_resize_event.bind([&](auto &canvas, auto width, auto height) {
    camera->SetViewbox(canvas.getRegion().AddDimension(-100, 100));
  });

// The part we care about for now:
  // cppplot(emp::plot::attributes::X = [](auto& pt) {return pt.x;}, emp::plot::attributes::Y = [](auto& pt) {return pt.x;}) + ;

  // using namespace emp::opengl;
  // using namespace emp::math;
  // using namespace emp::scenegraph;
  // using namespace emp::graphics;
  // using namespace emp::plot;
  // using namespace emp::opengl;
  // using namespace emp::math;
  // using namespace emp::scenegraph;
  // using namespace emp::graphics;
  // using namespace emp::plot;
  // using namespace emp::plot::attributes;
  // using namespace emp::plot::attributes;

  // GLCanvas canvas(800, 800);
  // shaders::LoadShaders(canvas);

  // emp::Resources<FontFace>::Add("Roboto", [] {
  //   auto font = ft.load("Assets/RobotoMono-Regular.ttf");
  //   font.SetFreeTypePixelSize(0, 64);
  //   font.BulidAsciiAtlas();
  //   return font;
  // });

  // Stage<2> stage;
  // auto root = stage.MakeRoot<Flow<2>>(true, FlowDirection<2>::Y);
  // auto line{std::make_shared<Line<2>>()};
  // auto scatter{std::make_shared<Scatter<2>>(Mesh::Polygon(32, {2, 2}))};
  // auto scale{std::make_shared<Scale<2>>()};

  // auto plot{std::make_shared<Stack<2>>()};
  // auto plot_title{std::make_shared<Text<2>>("Hello World", 32)};
  // // auto plot_subtitle{std::make_shared<Text<2>>("details", 18)};
  // plot->Append(line).Append(scatter).Append(scale);
  // root->Append(plot_title, 0);
  // // root->Append(plot_subtitle, 0);
  // root->Append(plot);

  // struct data_t {
  //   Vec2f value;
  //   Color color;
  // };

  // std::vector<data_t> particles;
  // size_t count_particles = 5000;
  // for (int i = 0; i < count_particles; ++i) {
  //   particles.push_back(
  //     {Vec2f{i, rand() % 100 - 50}, Color{
  //                                     (rand() % 1000) / 1000.0f,
  //                                     (rand() % 1000) / 1000.0f,
  //                                     (rand() % 1000) / 1000.0f,
  //                                   }});
  // }

  // auto flow = MakeFlow().Then(line).Then(scatter).Then(scale).Data(
  //   Xyz = [](auto &p) { return p.value; }, PointSize = 1,
  //   emp::graphics::Fill =
  //     [](auto &p) { return p.color; },  // TODO: allow &data_t::color
  //   emp::graphics::Stroke = [](auto &p) { return p.color; },
  //   emp::graphics::StrokeWeight = 1, emp::graphics::TextSize = 16);

  // auto camera =
  //   std::make_shared<OrthoCamera>(canvas.getRegion().AddDimension(-100,
  //   100));
  // auto eye = std::make_shared<SimpleEye>();

  // emp::graphics::Graphics g(canvas, "Roboto", camera, eye);
  // canvas.on_resize_event.bind([&](auto &canvas, auto width, auto height) {
  //   camera->SetViewbox(canvas.getRegion().AddDimension(-100, 100));
  // });

  // canvas.runForever([&](auto &&) {
  //   g.Clear(Color::grey(0.8));

  //   // auto g = plot(particles.begin(), particles.end(),
  //   //               aes(X = [](auto) {}, Y = [](auto) {})) +
  //   //          linear_scale<X>() + linear_scale<Y>() +
  //   //          scatter(aes(Color = Color::Red));

  //   flow(particles.begin(), particles.end());

  //   stage.Render(g, canvas.getRegion());

  //   for (int i = 0; i < particles.size(); ++i) {
  //     int count = 0;
  //     double m = 0;
  //     for (int j = std::max(i - 1, 0);
  //          j <= std::min(i + 1, (int)particles.size() - 1); ++j) {
  //       m += particles[j].value.y();
  //       ++count;
  //     }
  //     particles[i].value.y() = m / count;
  //   }

  //   std::cout << std::endl << std::endl;
  // });

  // return 0;
}
