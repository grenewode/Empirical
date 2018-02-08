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
#include "opengl/glcanvas.h"
#include "plot/flow.h"
#include "plot/line.h"
#include "plot/scales.h"
#include "plot/scatter.h"
#include "scenegraph/camera.h"
#include "scenegraph/core.h"
// #include "scenegraph/shapes.h"
#include "scenegraph/transform.h"

#include <chrono>
#include <cstdlib>

int main(int argc, char* argv[]) {
  using namespace emp::opengl;
  using namespace emp::math;
  using namespace emp::scenegraph;
  using namespace emp::plot;
  using namespace emp::plot::attrs;

  GLCanvas canvas;
  Group root;
  // auto line{std::make_shared<Line>(canvas)};
  auto scatter{std::make_shared<Scatter>(canvas, 6)};
  auto scale{std::make_shared<Scale>(canvas.getRegion())};
  root.attachAll(scatter);

  std::vector<Vec3f> data;

  auto flow = (xy([](auto& p) { return p; }) + stroke(Color::red()) +
               strokeWeight(2) + fill(Color::blue()) + pointSize(10)) >>
              scale >> scatter;

  PerspectiveCamera camera(canvas.getRegion());
  canvas.bindOnResize([&camera, &scale](auto& canvas, auto width, auto height) {
    camera.setRegion(canvas.getRegion());
    scale->screenSpace = canvas.getRegion();
  });
  auto random = [] {
    using rand_t = decltype(rand());
    using limits_t = std::numeric_limits<rand_t>;
    return (rand() + limits_t::min()) /
           (limits_t::max() - (float)limits_t::lowest());
  };

  for (int i = 0; i < 10; ++i) {
    data.emplace_back(random(), random(), random());
  }

  flow.apply(data.begin(), data.end());

  canvas.runForever([&](auto&&) {
    glClearColor(1.0f, 1.0f, 1.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT);

    root.render(camera);
  });

  return 0;
}