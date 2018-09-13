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

emp::scenegraph::FreeType ft;

int main(int argc, char *argv[]) {
  using namespace emp::opengl;
  using namespace emp::math;
  using namespace emp::scenegraph;
  using namespace emp::graphics;
  using namespace emp::plot;
  using namespace emp::plot::attributes;

  GLCanvas canvas(800, 800);
  shaders::LoadShaders(canvas);

  emp::Resources<FontFace>::Add("Roboto", [] {
    auto font = ft.load("Assets/RobotoMono-Regular.ttf");
    font.SetFreeTypePixelSize(0, 64);
    font.BulidAsciiAtlas();
    return font;
  });

  Stage<2> stage;
  auto root = stage.MakeRoot<Flow<2>>(true, FlowDirection<2>::Y);
  auto line{std::make_shared<Line<2>>()};
  auto scatter{std::make_shared<Scatter<2>>(Mesh::Polygon(32, {2, 2}))};
  auto scale{std::make_shared<Scale<2>>()};

  auto plot{std::make_shared<Stack<2>>()};
  auto plot_title{std::make_shared<Text<2>>("Hello World", 32)};
  // auto plot_subtitle{std::make_shared<Text<2>>("details", 18)};
  plot->Append(line).Append(scatter).Append(scale);
  root->Append(plot_title, 0);
  // root->Append(plot_subtitle, 0);
  root->Append(plot);

  struct data_t {
    Vec2f value;
    Color color;
  };

  std::vector<data_t> particles;
  size_t count_particles = 5000;
  for (int i = 0; i < count_particles; ++i) {
    particles.push_back(
      {Vec2f{i, rand() % 100 - 50}, Color{
                                      (rand() % 1000) / 1000.0f,
                                      (rand() % 1000) / 1000.0f,
                                      (rand() % 1000) / 1000.0f,
                                    }});
  }

  auto flow = MakeFlow().Then(line).Then(scatter).Then(scale).Data(
    Xyz = [](auto &p) { return p.value; }, PointSize = 1,
    emp::graphics::Fill =
      [](auto &p) { return p.color; },  // TODO: allow &data_t::color
    emp::graphics::Stroke = [](auto &p) { return p.color; },
    emp::graphics::StrokeWeight = 1, emp::graphics::TextSize = 16);

  auto camera =
    std::make_shared<OrthoCamera>(canvas.getRegion().AddDimension(-100, 100));
  auto eye = std::make_shared<SimpleEye>();

  emp::graphics::Graphics g(canvas, "Roboto", camera, eye);
  canvas.on_resize_event.bind([&](auto &canvas, auto width, auto height) {
    camera->SetViewbox(canvas.getRegion().AddDimension(-100, 100));
  });

  canvas.runForever([&](auto &&) {
    g.Clear(Color::grey(0.8));

    flow(particles.begin(), particles.end());

    stage.Render(g, canvas.getRegion());

    for (int i = 0; i < particles.size(); ++i) {
      int count = 0;
      double m = 0;
      for (int j = std::max(i - 1, 0);
           j <= std::min(i + 1, (int)particles.size() - 1); ++j) {
        m += particles[j].value.y();
        ++count;
      }
      particles[i].value.y() = m / count;
    }

    std::cout << std::endl << std::endl;
  });

  return 0;
}
