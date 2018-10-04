#ifndef EMP_SCENEGRAPH_RENDERING_H
#define EMP_SCENEGRAPH_RENDERING_H

#include "math/consts.h"
#include "math/region.h"
#include "opengl/color.h"
#include "opengl/glcanvas.h"
#include "opengl/glwrap.h"
#include "opengl/shaders.h"
#include "scenegraph/camera.h"
#include "scenegraph/freetype.h"
#include "tools/attrs.h"
#include "tools/resources.h"

#include <memory>
#include <vector>

namespace emp {
  namespace graphics {
    struct RenderSettings {
      math::Mat4x4f projection;
      math::Mat4x4f view;
    };

    enum class TextDirections { Horizontal, Vertical };

    DEFINE_ATTR(Vertex);
    DEFINE_ATTR(Transform);
    DEFINE_ATTR(Fill);
    DEFINE_ATTR(Stroke);
    DEFINE_ATTR(StrokeWeight);
    DEFINE_ATTR(Text);
    DEFINE_ATTR(TextSize);
    DEFINE_ATTR(TextDirection);

    struct Face {
      int a, b, c;
    };

    struct Mesh {
      std::vector<emp::math::Vec3f> verticies;
      std::vector<Face> faces;

      static Mesh Region(const emp::math::Region2f &region) {
        emp::math::Vec3f bottom_left{region.min.x(), region.min.y(), 0};
        emp::math::Vec3f bottom_right{region.max.x(), region.min.y(), 0};
        emp::math::Vec3f upper_right{region.max.x(), region.max.y(), 0};
        emp::math::Vec3f upper_left{region.min.x(), region.max.y(), 0};

        return {{bottom_left, bottom_right, upper_right, upper_left},
                {{0, 3, 1}, {3, 2, 1}}};
      }
      static Mesh Polygon(size_t vertex_count,
                          const emp::math::Vec2f &radius = {0.5, 0.5}) {
        Mesh mesh;
        int first = 0;
        for (auto i = 0; i < vertex_count; ++i) {
          auto r =
            static_cast<float>(i) / vertex_count * 2 * math::consts::pi<float> +
            math::consts::pi<float> / vertex_count;

          int num = mesh.verticies.size();
          mesh.verticies.emplace_back(cos(r) * radius.x() * 2,
                                      sin(r) * radius.y() * 2, 0);

          if (i == 0) first = num;

          auto next = num + 1;
          if (i == vertex_count - 1) {
            next = first;
          }

          mesh.faces.push_back({first, num, next});
        }

        return mesh;
      }
    };

    class FillRenderer {
      ResourceRef<opengl::ShaderProgram> fill_shader;

      opengl::VertexArrayObject vao;

      opengl::BufferVector<opengl::BufferType::Array, math::Vec3f>
        gpu_vertex_buffer;

      opengl::BufferVector<opengl::BufferType::ElementArray, int>
        gpu_elements_buffer;

      struct {
        opengl::Uniform model;
        opengl::Uniform view;
        opengl::Uniform projection;
        opengl::Uniform fill;
      } fill_shader_uniforms;

      public:
      using instance_attributes_type =
        tools::Attrs<TransformValue<math::Mat4x4f>, FillValue<opengl::Color>>;

      private:
      std::vector<instance_attributes_type> draw_queue;

      public:
      template <typename S = const char *>
      FillRenderer(opengl::GLCanvas &canvas,
                   S &&fill_shader = "DefaultSolidColor")
        : fill_shader(std::forward<S>(fill_shader)),
          vao(canvas.MakeVAO()),
          gpu_vertex_buffer(canvas.makeBuffer<opengl::BufferType::Array>()),
          gpu_elements_buffer(
            canvas.makeBuffer<opengl::BufferType::ElementArray>()) {
        this->fill_shader.OnSet([this](auto &) {
          fill_shader_uniforms.model = this->fill_shader->Uniform("model");
          fill_shader_uniforms.view = this->fill_shader->Uniform("view");
          fill_shader_uniforms.projection =
            this->fill_shader->Uniform("projection");
          fill_shader_uniforms.fill = this->fill_shader->Uniform("fill");

          vao.bind();
          gpu_vertex_buffer.bind();
          gpu_elements_buffer.bind();
          vao.attr(this->fill_shader->Attribute<math::Vec3f>("position"));
        });
      }

      void BeginBatch(const RenderSettings &settings, const Mesh &mesh) {
        auto first = 0;

        gpu_elements_buffer.Clear();
        gpu_vertex_buffer.Clear();

        for (auto &vertex : mesh.verticies) {
          gpu_vertex_buffer.EmplaceData(vertex);
        }

        for (auto &face : mesh.faces) {
          gpu_elements_buffer.PushData(face.a);
          gpu_elements_buffer.PushData(face.b);
          gpu_elements_buffer.PushData(face.c);
        }

        vao.bind();
        gpu_vertex_buffer.SendToGPU();
        gpu_elements_buffer.SendToGPU();

        fill_shader->Use();
        fill_shader_uniforms.projection = settings.projection;
        fill_shader_uniforms.view = settings.view;
      }

      template <typename I = instance_attributes_type>
      void Instance(I &&attrs) {
        draw_queue.emplace_back(std::forward<I>(attrs));
      }

      template <typename I = instance_attributes_type>
      void Instance(I &&attrs, float width, float height) {
        Transform::Get(attrs) *= emp::math::Mat4x4f::Scale(width, height, 1);

        draw_queue.emplace_back(std::forward<I>(attrs));
      }

      void FinishBatch() {
        // #ifndef EMSCRIPTEN
        //         if (draw_queue.size() > 10000) {
        //           return;
        //         }
        // #endif

        fill_shader->Use();
        vao.bind();

        for (auto &attrs : draw_queue) {
          fill_shader_uniforms.model = attrs.GetTransform();
          fill_shader_uniforms.fill = attrs.GetFill();

          gpu_elements_buffer.Draw(GL_TRIANGLES);
        }
        draw_queue.clear();
      }
    };

    class LineRenderer {
      ResourceRef<opengl::ShaderProgram> fill_shader;

      opengl::VertexArrayObject vao;

      struct LineVertex {
        math::Vec3f position;
        opengl::Color color;
      };

      opengl::BufferVector<opengl::BufferType::Array, LineVertex>
        gpu_vertex_buffer;

      opengl::BufferVector<opengl::BufferType::ElementArray, int>
        gpu_elements_buffer;

      struct {
        opengl::Uniform model;
        opengl::Uniform view;
        opengl::Uniform projection;
      } fill_shader_uniforms;

      public:
      using instance_attributes_type =
        tools::Attrs<TransformValue<math::Mat4x4f>>;

      using vertex_attributes_type =
        tools::Attrs<VertexValue<math::Vec3f>, StrokeValue<opengl::Color>>;

      private:
      std::vector<instance_attributes_type> draw_queue;

      struct DefaultTransform {
        constexpr DefaultTransform() {}

        template <typename V>
        constexpr auto operator()(V &&v) -> std::decay_t<decltype(v)> {
          return std::forward<decltype(v)>(v);
        }
      };

      public:
      template <typename S = const char *>
      LineRenderer(opengl::GLCanvas &canvas,
                   S &&fill_shader = "DefaultVaryingColor")
        : fill_shader(std::forward<S>(fill_shader)),
          vao(canvas.MakeVAO()),
          gpu_vertex_buffer(canvas.makeBuffer<opengl::BufferType::Array>()),
          gpu_elements_buffer(
            canvas.makeBuffer<opengl::BufferType::ElementArray>()) {
        this->fill_shader.OnSet([this](auto &) {
          fill_shader_uniforms.model = this->fill_shader->Uniform("model");
          fill_shader_uniforms.view = this->fill_shader->Uniform("view");
          fill_shader_uniforms.projection =
            this->fill_shader->Uniform("projection");

          vao.bind();
          gpu_vertex_buffer.bind();
          gpu_elements_buffer.bind();
          // TODO: this probably should not use this feature, as it may break on
          // some compilers
          vao.attr(
            this->fill_shader->Attribute("position", &LineVertex::position));
          vao.attr(this->fill_shader->Attribute("color", &LineVertex::color));
        });
      }

      template <typename I, typename T = LineRenderer::DefaultTransform>
      void BeginBatch(const RenderSettings &settings, I begin, I end,
                      const T &transform = LineRenderer::DefaultTransform{}) {
        gpu_elements_buffer.Clear();
        gpu_vertex_buffer.Clear();
        if (begin == end) return;
        auto segment_start = begin++;
        // Don't draw trivial shapes with only one point
        if (begin == end) return;
        auto segment_center = begin++;
        // if (begin == end) {
        //   // TODO: Handle line segments
        // } else {
        int last_segment_plus, last_segment_minus;
        {
          auto first_attrs = transform(*segment_start);
          math::Vec3f first{Vertex::Get(first_attrs).x(),
                            Vertex::Get(first_attrs).y(), 0};
          auto second_attrs = transform(*segment_center);
          math::Vec3f second = {Vertex::Get(second_attrs).x(),
                                Vertex::Get(second_attrs).y(), 0};

          auto tangent =
            (second - first).Normalized() * StrokeWeight::Get(first_attrs);

          auto normal = math::Vec3f{-tangent.y(), tangent.x(), 0};

          last_segment_plus = gpu_vertex_buffer.PushData(LineVertex{
            first + normal,
            Stroke::Get(first_attrs),
          });

          last_segment_minus = gpu_vertex_buffer.PushData(LineVertex{
            first - normal,
            Stroke::Get(first_attrs),
          });
        }

        // // Don't advance here. We will do that *after* we finish each loop
        // // iteration
        auto segment_end = begin;

        while (segment_end != end) {
          auto start_attrs = transform(*segment_start);
          math::Vec3f start{Vertex::Get(start_attrs).x(),
                            Vertex::Get(start_attrs).y(), 0};

          auto center_attrs = transform(*segment_center);
          math::Vec3f center{Vertex::Get(center_attrs).x(),
                             Vertex::Get(center_attrs).y(), 0};

          auto end_attrs = transform(*segment_end);
          math::Vec3f end{Vertex::Get(end_attrs).x(),
                          Vertex::Get(end_attrs).y(), 0};

          auto tangent1 = center - start;
          auto segment1_length = tangent1.Mag();
          tangent1 /= segment1_length;
          auto tangent2 = end - center;
          auto segment2_length = tangent2.Mag();
          tangent2 /= segment2_length;

          auto stroke_weight = StrokeWeight::Get(center_attrs);

          auto tangent = (tangent2 + tangent1).Normalized();

          math::Vec3f normal1{-tangent1.y(), tangent1.x(), 0};
          math::Vec3f normal2{-tangent2.y(), tangent2.x(), 0};
          math::Vec3f miter{-tangent.y(), tangent.x(), 0};

          auto length = stroke_weight / (miter * normal1);

          auto color = Stroke::Get(center_attrs);

          int current_segment_plus, current_segment_minus;

          auto miter_offset = miter * length;
          auto miter_offset_length = miter_offset.Mag();

          miter_offset *= std::min(miter_offset_length,
                                   std::min(segment1_length, segment2_length)) /
                          miter_offset_length;

          if (miter_offset_length > stroke_weight) {
            auto cross = normal1.x() * normal2.y() - normal1.y() * normal2.x();

            normal1 = normal1.Normalized() * stroke_weight;
            normal2 = normal2.Normalized() * stroke_weight;
            if (cross > 0) {
              current_segment_plus =
                gpu_vertex_buffer.PushData({center + miter_offset, color});

              int start_join =
                gpu_vertex_buffer.PushData({center - normal1, color});

              int center_join = gpu_vertex_buffer.PushData(
                {center - miter.Normalized() * stroke_weight, color});

              int end_join =
                gpu_vertex_buffer.PushData({center - normal2, color});

              gpu_elements_buffer.PushData(last_segment_plus);
              gpu_elements_buffer.PushData(last_segment_minus);
              gpu_elements_buffer.PushData(current_segment_plus);

              gpu_elements_buffer.PushData(current_segment_plus);
              gpu_elements_buffer.PushData(start_join);
              gpu_elements_buffer.PushData(last_segment_minus);

              gpu_elements_buffer.PushData(current_segment_plus);
              gpu_elements_buffer.PushData(start_join);
              gpu_elements_buffer.PushData(center_join);

              gpu_elements_buffer.PushData(current_segment_plus);
              gpu_elements_buffer.PushData(center_join);
              gpu_elements_buffer.PushData(end_join);

              current_segment_minus = end_join;

            } else {
              int start_join =
                gpu_vertex_buffer.PushData({center + normal1, color});

              int center_join = gpu_vertex_buffer.PushData(
                {center + miter.Normalized() * stroke_weight, color});

              int end_join =
                gpu_vertex_buffer.PushData({center + normal2, color});

              current_segment_minus =
                gpu_vertex_buffer.PushData({center - miter_offset, color});

              gpu_elements_buffer.PushData(last_segment_plus);
              gpu_elements_buffer.PushData(last_segment_minus);
              gpu_elements_buffer.PushData(start_join);

              gpu_elements_buffer.PushData(start_join);
              gpu_elements_buffer.PushData(current_segment_minus);
              gpu_elements_buffer.PushData(last_segment_minus);

              gpu_elements_buffer.PushData(start_join);
              gpu_elements_buffer.PushData(center_join);
              gpu_elements_buffer.PushData(current_segment_minus);

              gpu_elements_buffer.PushData(center_join);
              gpu_elements_buffer.PushData(end_join);
              gpu_elements_buffer.PushData(current_segment_minus);

              current_segment_plus = end_join;
            }
          } else {
            current_segment_plus =
              gpu_vertex_buffer.PushData({center + miter_offset, color});
            current_segment_minus =
              gpu_vertex_buffer.PushData({center - miter_offset, color});

            gpu_elements_buffer.PushData(last_segment_plus);
            gpu_elements_buffer.PushData(last_segment_minus);
            gpu_elements_buffer.PushData(current_segment_plus);

            gpu_elements_buffer.PushData(current_segment_plus);
            gpu_elements_buffer.PushData(current_segment_minus);
            gpu_elements_buffer.PushData(last_segment_minus);
          }

          last_segment_plus = current_segment_plus;
          last_segment_minus = current_segment_minus;

          segment_start = segment_center;
          segment_center = segment_end;
          segment_end = ++begin;
        }

        {
          auto start_attrs = transform(*segment_start);
          math::Vec3f start{Vertex::Get(start_attrs).x(),
                            Vertex::Get(start_attrs).y(), 0};

          auto center_attrs = transform(*segment_center);
          math::Vec3f end{Vertex::Get(center_attrs).x(),
                          Vertex::Get(center_attrs).y(), 0};
          auto stroke_weight = StrokeWeight::Get(center_attrs);
          auto color = Stroke::Get(center_attrs);

          auto tangent = (end - start).Normalized() * stroke_weight;

          auto normal = math::Vec3f{-tangent.y(), tangent.x(), 0};

          int current_segment_plus = gpu_vertex_buffer.PushData(LineVertex{
            end + normal,
            color,
          });

          int current_segment_minus = gpu_vertex_buffer.PushData(LineVertex{
            end - normal,
            color,
          });

          gpu_elements_buffer.PushData(last_segment_plus);
          gpu_elements_buffer.PushData(last_segment_minus);
          gpu_elements_buffer.PushData(current_segment_plus);

          gpu_elements_buffer.PushData(current_segment_plus);
          gpu_elements_buffer.PushData(current_segment_minus);
          gpu_elements_buffer.PushData(last_segment_minus);
        }

        vao.bind();
        gpu_vertex_buffer.SendToGPU();
        gpu_elements_buffer.SendToGPU();

        fill_shader->Use();
        fill_shader_uniforms.projection = settings.projection;
        fill_shader_uniforms.view = settings.view;
      }

      template <typename I = instance_attributes_type>
      void Instance(I &&attrs) {
        draw_queue.emplace_back(std::forward<I>(attrs));
      }

      template <typename I = instance_attributes_type>
      void Instance(I &&attrs, float width, float height) {
        Transform::Get(attrs) *= emp::math::Mat4x4f::Scale(width, height, 1);

        draw_queue.emplace_back(std::forward<I>(attrs));
      }

      void FinishBatch() {
        // #ifndef EMSCRIPTEN
        //         if (draw_queue.size() > 10000) {
        //           return;
        //         }
        // #endif

        fill_shader->Use();
        vao.bind();

        for (auto &attrs : draw_queue) {
          fill_shader_uniforms.model = attrs.GetTransform();

          gpu_elements_buffer.Draw(GL_TRIANGLES);
        }
        draw_queue.clear();
      }
    };  // namespace graphics

    class TextRenderer {
      struct data_t {
        math::Vec3f position;
        math::Vec2f texture_coordinates;
      };

      opengl::VertexArrayObject vao;
      opengl::BufferVector<opengl::BufferType::Array, data_t> vertices_buffer;
      ResourceRef<scenegraph::FontFace> font;
      ResourceRef<opengl::ShaderProgram> shader;

      struct {
        opengl::Uniform model;
        opengl::Uniform view;
        opengl::Uniform projection;
        opengl::Uniform tex;
        opengl::Uniform fill;
      } shader_uniforms;

      public:
      using instance_attributes_type =
        tools::Attrs<TransformValue<math::Mat4x4f>, FillValue<opengl::Color>,
                     TextValue<std::string>, TextSizeValue<float>>;

      template <typename F, typename S = std::string>
      TextRenderer(opengl::GLCanvas &canvas, F &&font,
                   S &&shader = "DefaultFont")
        : vao(canvas.MakeVAO()),
          vertices_buffer(canvas.makeBuffer<opengl::BufferType::Array>()),
          font(std::forward<F>(font)),
          shader(std::forward<S>(shader)) {
        using namespace emp::opengl;
        using namespace emp::math;

        this->shader.OnSet([this](auto &value) {
          shader_uniforms.model = this->shader->Uniform("model");
          shader_uniforms.view = this->shader->Uniform("view");
          shader_uniforms.projection = this->shader->Uniform("projection");
          shader_uniforms.tex = this->shader->Uniform("tex");
          shader_uniforms.fill = this->shader->Uniform("fill");

          vao.bind();
          vertices_buffer.bind();
          vao.attr(this->shader->Attribute("position", &data_t::position));
          vao.attr(this->shader->Attribute("uv", &data_t::texture_coordinates));
        });
      }

      void BeginBatch(const RenderSettings &settings) {
        this->shader->Use();
        shader_uniforms.projection = settings.projection;
        shader_uniforms.view = settings.view;
      }

      emp::math::Vec2f Measure(
        const std::string &text, float text_size,
        TextDirections direction = TextDirections::Horizontal) const {
        float scale = text_size / font->atlas_height;
        emp::math::Vec2f cursor{0, 0};
        bool first = true;
        for (auto &c : text) {
          auto info = font->Lookup(c);

          switch (direction) {
            // Advance the cursor by the size of the character
            case TextDirections::Horizontal:
              if (first) {
                cursor.x() = info.bitmap_size.x() * scale;
                first = false;
              }
              cursor.x() += info.cursor_advance.x() * scale;
              cursor.y() = std::max(cursor.y(), info.bitmap_size.y() * scale);
              break;
            case TextDirections::Vertical:
              if (first) {
                cursor.y() = info.bitmap_size.y() * scale;
                first = false;
              }
              cursor.x() = std::max(cursor.x(), info.bitmap_size.x() * scale);
              cursor.y() += info.cursor_advance.y() * scale;
              break;
          }
        }

        return cursor;
      }

      void Instance(const instance_attributes_type &attrs) {
        using namespace emp::opengl;
        using namespace emp::math;

        Vec2f cursor{0, 0};
        vertices_buffer.Clear();

        float scale = TextSize::Get(attrs) / font->atlas_height;

        int i = 0;
        for (auto &c : attrs.GetText()) {
          auto info = font->Lookup(c);
          auto lcursor = cursor;
          // Calculate the start of the next character
          cursor = cursor + Vec2f{info.cursor_advance.x() * scale,
                                  info.cursor_advance.y() * scale};

          // Skip characters who have no size, such as spaces
          if (info.bitmap_size.x() <= 0 || info.bitmap_size.y() <= 0) continue;

          // See https://www.freetype.org/freetype2/docs/tutorial/step2.html for
          // what bearing is. Basically, it is the position of this character
          // relative to the last
          auto max =
            lcursor + Vec2f{info.bearing.x() * scale, info.bearing.y() * scale};
          auto min = max - Vec2f{info.bitmap_size.x() * scale,
                                 info.bitmap_size.y() * scale};

          auto tmin = info.texture_region.min;
          auto tmax = info.texture_region.max;

          vertices_buffer.PushData(
            {{min.x(), min.y(), 0}, {tmin.x(), tmax.y()}});
          vertices_buffer.PushData(
            {{max.x(), min.y(), 0}, {tmax.x(), tmax.y()}});

          vertices_buffer.PushData(
            {{min.x(), max.y(), 0}, {tmin.x(), tmin.y()}});
          vertices_buffer.PushData(
            {{max.x(), min.y(), 0}, {tmax.x(), tmax.y()}});

          vertices_buffer.PushData(
            {{min.x(), max.y(), 0}, {tmin.x(), tmin.y()}});
          vertices_buffer.PushData(
            {{max.x(), max.y(), 0}, {tmax.x(), tmin.y()}});
        }
        shader->Use();
        vao.bind();
        vertices_buffer.SendToGPU();

        shader_uniforms.model = attrs.GetTransform();
        shader_uniforms.tex = *font->ComputeAtlasTexture();
        shader_uniforms.fill = attrs.GetFill();

        vertices_buffer.Draw(GL_TRIANGLES);
      }

      void FinishBatch() {}
    };

    template <typename R>
    class Pen {
      private:
      R *renderer;

      public:
      using instance_attributes_type = typename R::instance_attributes_type;

      template <typename... T>
      Pen(R *renderer, const RenderSettings &settings, T &&... args)
        : renderer(renderer) {
        renderer->BeginBatch(settings, std::forward<T>(args)...);
      }

      template <typename I, typename... U>
      Pen &Data(I begin, I end, const tools::Attrs<U...> &transform) {
        for (; begin != end; ++begin) {
          Draw(transform(*begin));
        }
        return *this;
      }
      template <typename I, typename... U>
      Pen &Data(I &iterable, const tools::Attrs<U...> &transform) {
        return Data(std::begin(iterable), std::end(iterable), transform);
      }
      template <typename I, typename... U>
      Pen &Data(const I &iterable, const tools::Attrs<U...> &transform) {
        return Data(std::begin(iterable), std::end(iterable), transform);
      }

      template <typename T0 = instance_attributes_type, typename... T>
      Pen &Draw(T0 &&args0, T &&... args) {
        renderer->Instance(std::forward<T0>(args0), std::forward<T>(args)...);
        return *this;
      }

      void Flush() { renderer->FinishBatch(); }
    };

    class Graphics {
      FillRenderer fill_renderer;
      LineRenderer line_renderer;
      TextRenderer text_renderer;

      public:
      std::shared_ptr<scenegraph::Camera> camera;
      std::shared_ptr<scenegraph::Eye> eye;

      template <typename F>
      Graphics(opengl::GLCanvas &canvas, F &&font,
               std::shared_ptr<scenegraph::Camera> camera,
               std::shared_ptr<scenegraph::Eye> eye)
        : fill_renderer(canvas),
          line_renderer(canvas),
          text_renderer(canvas, std::forward<F>(font)),
          camera(camera),
          eye(eye) {}

      Graphics(const Graphics &) = delete;
      Graphics(Graphics &&) = delete;

      Graphics &operator=(const Graphics &) = delete;
      Graphics &operator=(Graphics &&) = delete;

      auto Measure(const std::string &text, float text_size) const {
        return text_renderer.Measure(text, text_size);
      }

      void Clear(float r, float g, float b, float a = 1) {
        glClearColor(r, g, b, a);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
      }

      void Clear(const opengl::Color &color) {
        Clear(color.r, color.g, color.b, color.a);
      }

      Pen<FillRenderer> Fill(const Mesh &mesh) {
        return {&fill_renderer,
                {camera->GetProjection(), eye->CalculateView()},
                mesh};
      }

      template <typename... Args>
      Pen<LineRenderer> Line(Args &&... args) {
        return {&line_renderer,
                {camera->GetProjection(), eye->CalculateView()},
                std::forward<Args>(args)...};
      }

      template <typename A0 = typename FillRenderer::instance_attributes_type,
                typename... A>
      void DrawFilled(const Mesh &mesh, A0 &&attributes, A &&... args) {
        Fill(mesh)
          .Draw(std::forward<A0>(attributes), std::forward<A>(args)...)
          .Flush();
      }

      Pen<TextRenderer> Text() {
        return {&text_renderer,
                {camera->GetProjection(), eye->CalculateView()}};
      }
    };
  }  // namespace graphics
}  // namespace emp

#endif
