#ifndef DEFAULT_SHADERS_H
#define DEFAULT_SHADERS_H

#include "../tools/resources.h"
#include "color.h"
#include "shaders.h"

namespace emp {
  namespace opengl {
    namespace shaders {

      constexpr auto DEFAULT_VARYING_SHADER_VERTEX_SRC =
#ifdef EMSCRIPTEN
        "precision mediump float;"
#endif
        R"glsl(
                attribute vec3 position;
                attribute vec4 color;

                uniform mat4 model;
                uniform mat4 view;
                uniform mat4 projection;

                varying vec4 f_color;

                void main()
                {
                    gl_Position = projection * view * model * vec4(position, 1.0);
                    f_color = color;
                }
            )glsl";

      constexpr auto DEFAULT_SOLID_SHADER_VERTEX_SRC =
#ifdef EMSCRIPTEN
        "precision mediump float;"
#endif
        R"glsl(
                attribute vec3 position;
                uniform vec4 fill;

                uniform mat4 model;
                uniform mat4 view;
                uniform mat4 projection;

                varying vec4 f_color;

                void main()
                {
                    gl_Position = projection * view * model * vec4(position, 1.0);
                    f_color = fill;
                }
            )glsl";

      constexpr auto DEFAULT_SIMPLE_SHADER_FRAGMENT_SRC =
#ifdef EMSCRIPTEN
        "precision mediump float;"
#endif
        R"glsl(
                  varying vec4 f_color;

                  void main()
                  {
                      gl_FragColor = f_color;
                  }
              )glsl";

      constexpr auto DEFAULT_TEXTURE_SHADER_VERTEX_SRC =
#ifdef EMSCRIPTEN
        R"glsl(
		precision mediump float;
		attribute vec2 uv;
		attribute vec3 position;

		uniform mat4 model;
		uniform mat4 view;
		uniform mat4 projection;

		varying vec2 f_uv;

		void main()
		{
		    gl_Position = projection * view * model * vec4(position, 1.0);
		    f_uv = uv;
		}
            )glsl";
#else
        R"glsl(
          #version 150 core
          in vec2 uv;
          in vec3 position;

          uniform mat4 model;
          uniform mat4 view;
          uniform mat4 projection;
          uniform vec4 color;
          out vec2 f_uv;

          void main()
          {
            gl_Position = projection * view * model * vec4(position, 1.0);
            f_uv = uv;
          }
            )glsl";
#endif

      constexpr auto DEFAULT_TEXTURE_SHADER_FRAGMENT_SRC =
#ifdef EMSCRIPTEN
        R"glsl(
          precision mediump float;

	        varying vec2 f_uv;
          uniform sampler2D tex;

          void main()
          {
              gl_FragColor = texture2D(tex, f_uv);
          }
        )glsl";
#else
        R"glsl(
          #version 150 core

          in vec2 f_uv;

          uniform sampler2D tex;
          out vec4 color;

          void main()
          {
            color = texture(tex, f_uv);
          }
        )glsl";
#endif

      constexpr auto DEFAULT_FONT_SHADER_VERTEX_SRC =
#ifdef EMSCRIPTEN
        R"glsl(
		precision mediump float;
		attribute vec2 uv;
		attribute vec3 position;

		uniform mat4 model;
		uniform mat4 view;
		uniform mat4 projection;

		varying vec2 f_uv;

		void main()
		{
		    gl_Position = projection * view * model * vec4(position, 1.0);
		    f_uv = uv;
		}
            )glsl";
#else
        R"glsl(
          #version 150 core
          in vec2 uv;
          in vec3 position;

          uniform mat4 model;
          uniform mat4 view;
          uniform mat4 projection;
          out vec2 f_uv;

          void main()
          {
            gl_Position = projection * view * model * vec4(position, 1.0);
            f_uv = uv;
          }
            )glsl";
#endif

      constexpr auto DEFAULT_FONT_SHADER_FRAGMENT_SRC =
#ifdef EMSCRIPTEN
        R"glsl(
          precision mediump float;

          varying vec2 f_uv;
          uniform sampler2D tex;
          uniform vec4 fill;

          void main()
          {
              // gl_FragColor = vec4(1, 1, 1, texture2D(tex, f_uv).r);
               gl_FragColor = vec4(0, 0, 0, fill.a * texture2D(tex, f_uv).a);
              // gl_FragColor =  texture2D(tex, f_uv);
          }
        )glsl";
#else
        R"glsl(
          #version 150 core

          in vec2 f_uv;

          uniform vec4 fill;
          uniform sampler2D tex;

          out vec4 color;

          void main()
          {
            // color = vec4(1, 1, 1, texture2D(tex, f_uv).b);
            color = vec4(fill.rgb, fill.a * texture2D(tex, f_uv).r);
          }
        )glsl";
#endif

      void LoadShaders() {
        Resources<ShaderProgram>::Add("DefaultVaryingColor", [] {
          return ShaderProgram{DEFAULT_VARYING_SHADER_VERTEX_SRC,
                               DEFAULT_SIMPLE_SHADER_FRAGMENT_SRC};
        });

        Resources<ShaderProgram>::Add("DefaultSolidColor", [] {
          return ShaderProgram(DEFAULT_SOLID_SHADER_VERTEX_SRC,
                               DEFAULT_SIMPLE_SHADER_FRAGMENT_SRC);
        });

        Resources<ShaderProgram>::Add("DefaultTextured", [] {
          return ShaderProgram(DEFAULT_TEXTURE_SHADER_VERTEX_SRC,
                               DEFAULT_TEXTURE_SHADER_FRAGMENT_SRC);
        });
        Resources<ShaderProgram>::Add("DefaultFont", [] {
          return ShaderProgram(DEFAULT_FONT_SHADER_VERTEX_SRC,
                               DEFAULT_FONT_SHADER_FRAGMENT_SRC);
        });
      }

    }  // namespace shaders
  }  // namespace opengl
}  // namespace emp

#endif
