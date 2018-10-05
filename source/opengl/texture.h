#ifndef EMP_OPENGL_TEXTURE_H
#define EMP_OPENGL_TEXTURE_H

#include <vector>
#include "opengl/color.h"
#include "opengl/glutils.h"

namespace emp {
  namespace opengl {
    enum class TextureBindTarget : GLenum {
      TwoDimensional = GL_TEXTURE_2D,
      CubeMap = GL_TEXTURE_CUBE_MAP,
#ifndef __EMSCRIPTEN__
      OneDimensional = GL_TEXTURE_1D,
      ThreeDimensional = GL_TEXTURE_3D,
      OneDimensionalArray = GL_TEXTURE_1D_ARRAY,
      TwoDimensionalArray = GL_TEXTURE_2D_ARRAY,
      Rectangle = GL_TEXTURE_RECTANGLE,
      CubeMapArray = GL_TEXTURE_CUBE_MAP_ARRAY,
      TwoDimensionalMultisample = GL_TEXTURE_2D_MULTISAMPLE,
      TwoDimensionalMultisampleArray = GL_TEXTURE_2D_MULTISAMPLE_ARRAY,
#endif
    };

    enum class TextureSwizzle : decltype(GL_RED) {
      Red = GL_RED,
      Green = GL_GREEN,
      Blue = GL_BLUE,
      Alpha = GL_ALPHA,
      Zero = GL_ZERO,
      One = GL_ONE
    };

    enum class TextureWrap : decltype(GL_CLAMP_TO_EDGE) {

      MirroredRepeat = GL_MIRRORED_REPEAT,
      Repeat = GL_REPEAT,
      ClampToEdge = GL_CLAMP_TO_EDGE,
#ifndef __EMSCRIPTEN__
      MirrorClampToEdge = GL_MIRROR_CLAMP_TO_EDGE,
      ClampToBorder = GL_CLAMP_TO_BORDER,
#endif
    };

    enum class TextureMinFilter : decltype(GL_NEAREST) {
      Nearest = GL_NEAREST,
      Linear = GL_LINEAR,
      NearestMipmapNearest = GL_NEAREST_MIPMAP_NEAREST,
      LinearMipmapNearest = GL_LINEAR_MIPMAP_NEAREST,
      NearestMipmapLienar = GL_NEAREST_MIPMAP_LINEAR,
      LinaerMipmapLinear = GL_LINEAR_MIPMAP_LINEAR
    };

    enum class TextureMagFilter : decltype(GL_NEAREST) {
      Nearest = GL_NEAREST,
      Linear = GL_LINEAR
    };

    enum class Texture2DFormat : GLint {
#if EMSCRIPTEN
      Luminance = GL_LUMINANCE,
      LuminanceAlpha = GL_LUMINANCE_ALPHA,
      Alpha = GL_ALPHA,
#else
      DepthComponent = GL_DEPTH_COMPONENT,
      DepthStencil = GL_DEPTH_STENCIL,
      R = GL_RED,
      RG = GL_RG,
#endif
      RGB = GL_RGB,
      RGBA = GL_RGBA,
    };

    enum class TextureType : GLenum {
      UnsignedByte = GL_UNSIGNED_BYTE,
      UnsignedShort = GL_UNSIGNED_SHORT_5_6_5,
      UnsignedShort_4_4_4_4 = GL_UNSIGNED_SHORT_4_4_4_4,
      UnsignedShort_5_5_5_1 = GL_UNSIGNED_SHORT_5_5_5_1,

#ifndef __EMSCRIPTEN__
      // OpenGL
      Byte = GL_BYTE,
      Short = GL_SHORT,
      UnsignedInt = GL_UNSIGNED_INT,
      Int = GL_INT,
      Float = GL_FLOAT,
      UnsignedByte_3_3_2 = GL_UNSIGNED_BYTE_3_3_2,
      UnsignedByte_2_3_3_Rev = GL_UNSIGNED_BYTE_2_3_3_REV,
      UnsignedShort_5_6_5 = GL_UNSIGNED_SHORT_5_6_5,
      UnsignedShort_5_6_5_Rev = GL_UNSIGNED_SHORT_5_6_5_REV,
      UnsignedShort_4_4_4_4_Rev = GL_UNSIGNED_SHORT_4_4_4_4_REV,
      UnsignedShort_1_5_5_5 = GL_UNSIGNED_SHORT_1_5_5_5_REV,
      UnsignedInt_8_8_8_8 = GL_UNSIGNED_INT_8_8_8_8,
      UnsignedInt_8_8_8_8_Rev = GL_UNSIGNED_INT_8_8_8_8_REV,
      UnsignedInt_10_10_10_2 = GL_UNSIGNED_INT_10_10_10_2,
      UnsignedInt_2_10_10_10 = GL_UNSIGNED_INT_2_10_10_10_REV,
#endif
    };
    template <typename>
    constexpr TextureType TextureTypeOf();

#define __emp_opengl_DEFINE_TEXTURE_TYPE_OF(gltype, variant) \
  template <>                                                \
  constexpr TextureType TextureTypeOf<gltype>() {            \
    return TextureType::variant;                             \
  }

    __emp_opengl_DEFINE_TEXTURE_TYPE_OF(GLubyte, UnsignedByte);
    __emp_opengl_DEFINE_TEXTURE_TYPE_OF(GLushort, UnsignedShort);

#ifndef __EMSCRIPTEN__
    __emp_opengl_DEFINE_TEXTURE_TYPE_OF(GLbyte, Byte);
    __emp_opengl_DEFINE_TEXTURE_TYPE_OF(GLshort, Short);
    __emp_opengl_DEFINE_TEXTURE_TYPE_OF(GLint, Int);
    __emp_opengl_DEFINE_TEXTURE_TYPE_OF(GLuint, UnsignedInt);
    __emp_opengl_DEFINE_TEXTURE_TYPE_OF(GLfloat, Float);
    __emp_opengl_DEFINE_TEXTURE_TYPE_OF(Color, Float);
#endif
#undef __emp_opengl_DEFINE_TEXTURE_TYPE_OF

    template <typename>
    constexpr Texture2DFormat Texture2DFormatOf();

#define __emp_opengl_DEFINE_TEXTURE_2D_FORMAT_OF(gltype, variant) \
  template <>                                                     \
  constexpr Texture2DFormat Texture2DFormatOf<gltype>() {         \
    return Texture2DFormat::variant;                              \
  }

    __emp_opengl_DEFINE_TEXTURE_2D_FORMAT_OF(Color, RGBA);

#undef __emp_opengl_DEFINE_TEXTURE_2D_FORMAT_OF

    namespace __impl_emp_opengl_texture_base {
      GLenum active_texture = GL_TEXTURE0;

      template <TextureBindTarget TARGET>
      class Texture {
        protected:
        static GLuint bound;
        GLuint name = 0;

        public:
        GLenum texture;
        static constexpr TextureBindTarget target{TARGET};

        explicit Texture(GLenum texture = GL_TEXTURE0) : texture(texture) {
          emp_checked_gl_void(glActiveTexture(texture));

          emp_checked_gl_void(glGenTextures(1, &name));
        }

        Texture(const Texture&) = delete;
        Texture(Texture&& other) : texture(other.texture), name(other.name) {
          other.name = 0;
        }

        Texture& operator=(const Texture&) = delete;
        Texture& operator=(Texture&& other) {
          if (this != &other) {
            name = other.name;
            other.name = 0;
            texture = other.texture;
          }

          return *this;
        }

        virtual ~Texture() {
          if (name != 0) {
            if (name == bound) bound = 0;

            emp_checked_gl_void(glDeleteTextures(1, &name));
          }
        }

        operator GLuint() const { return name; }
        operator bool() const { return name != 0; }

        void Activate(GLenum texture) {
          this->texture = texture;
          Activate();
        }

        void Activate() { emp_checked_gl_void(glActiveTexture(texture)); }

        void Bind() {
          emp_assert(name != bound, "the texture must not be already bound");
          Activate();
          emp_checked_gl_void(glBindTexture(static_cast<GLenum>(target), name));
          bound = name;
        }

        void SetDepthStencilTextureMode() {}
        void SetBaseMipmapLevel(GLint value = 0) {
          emp_assert(name == bound, "the Texture must be bound");
          emp_checked_gl_void(glTexParameteri(static_cast<GLenum>(target),
                                              GL_TEXTURE_BASE_LEVEL, value));
        }
#ifndef __EMSCRIPTEN__
        void SetBorderColor(const Color& color) {
          emp_assert(name == bound, "the Texture must be bound");
          emp_checked_gl_void(glTexParameterfv(static_cast<GLenum>(target),
                                               GL_TEXTURE_BORDER_COLOR,
                                               color.RgbaPtr()));
        }
#endif

        void SetCompareFunc() {}

        void SetCompareMode() {}

        void SetLODBias();

        void SetMinFilter(TextureMinFilter filter) {
          emp_assert(name == bound, "the Texture must be bound");
          emp_checked_gl_void(
            glTexParameteri(static_cast<GLenum>(target), GL_TEXTURE_MIN_FILTER,
                            static_cast<decltype(GL_NEAREST)>(filter)));
        }

        void SetMagFilter(TextureMagFilter filter) {
          emp_assert(name == bound, "the Texture must be bound");
          emp_checked_gl_void(
            glTexParameteri(static_cast<GLenum>(target), GL_TEXTURE_MAG_FILTER,
                            static_cast<decltype(GL_NEAREST)>(filter)));
        }

        void SetMinLOD(float value) {
          emp_assert(name == bound, "the Texture must be bound");
          emp_checked_gl_void(glTexParameterf(static_cast<GLenum>(target),
                                              GL_TEXTURE_MIN_LOD, value));
        }
        void SetMaxLOD(float value) {
          emp_assert(name == bound, "the Texture must be bound");
          emp_checked_gl_void(glTexParameterf(static_cast<GLenum>(target),
                                              GL_TEXTURE_MAX_LOD, value));
        }
        void SetMaxMipmapLevel(int value) {
          emp_assert(name == bound, "the Texture must be bound");
          emp_checked_gl_void(glTexParameteri(static_cast<GLenum>(target),
                                              GL_TEXTURE_MAX_LEVEL, value));
        }

        void SetRedSwizzle(TextureSwizzle value) {
          emp_assert(name == bound, "the Texture must be bound");
          emp_checked_gl_void(glTexParameteri(static_cast<GLenum>(target),
                                              GL_TEXTURE_SWIZZLE_R,
                                              static_cast<GLint>(value)));
        }
        void SetGreenSwizzle(TextureSwizzle value) {
          emp_assert(name == bound, "the Texture must be bound");
          emp_checked_gl_void(glTexParameteri(static_cast<GLenum>(target),
                                              GL_TEXTURE_SWIZZLE_G,
                                              static_cast<GLint>(value)));
        }
        void SetBlueSwizzle(TextureSwizzle value) {
          emp_assert(name == bound, "the Texture must be bound");
          emp_checked_gl_void(glTexParameteri(static_cast<GLenum>(target),
                                              GL_TEXTURE_SWIZZLE_B,
                                              static_cast<GLint>(value)));
        }
        void SetAlphaSwizzle(TextureSwizzle value) {
          emp_assert(name == bound, "the Texture must be bound");
          emp_checked_gl_void(glTexParameteri(static_cast<GLenum>(target),
                                              GL_TEXTURE_SWIZZLE_A,
                                              static_cast<GLint>(value)));
        }

        void SetSwizzle(TextureSwizzle red = TextureSwizzle::Red,
                        TextureSwizzle green = TextureSwizzle::Green,
                        TextureSwizzle blue = TextureSwizzle::Blue,
                        TextureSwizzle alpha = TextureSwizzle::Alpha) {
          emp_assert(name == bound, "the Texture must be bound");
#ifdef __EMSCRIPTEN__
          SetRedSwizzle(red);
          SetGreenSwizzle(green);
          SetBlueSwizzle(blue);
          SetAlphaSwizzle(alpha);
#else
          GLint params[] = {static_cast<GLint>(red), static_cast<GLint>(green),
                            static_cast<GLint>(blue),
                            static_cast<GLint>(alpha)};
          emp_checked_gl_void(glTexParameteriv(
            static_cast<GLenum>(target), GL_TEXTURE_SWIZZLE_RGBA, params));
#endif
        }

        void SetTextureWrapS(TextureWrap wrap) {
          emp_assert(name == bound, "the Texture must be bound");
          emp_checked_gl_void(glTexParameteri(static_cast<GLenum>(target),
                                              GL_TEXTURE_WRAP_S,
                                              static_cast<GLint>(wrap)));
        }

        void SetTextureWrapT(TextureWrap wrap) {
          emp_assert(name == bound, "the Texture must be bound");
          emp_checked_gl_void(glTexParameteri(static_cast<GLenum>(target),
                                              GL_TEXTURE_WRAP_T,
                                              static_cast<GLint>(wrap)));
        }
      };
      template <TextureBindTarget TARGET>
      constexpr TextureBindTarget Texture<TARGET>::target;

      template <TextureBindTarget TARGET>
      GLuint Texture<TARGET>::bound{0};

      template <TextureBindTarget TARGET>
      class Texture2d : public Texture<TARGET> {
        private:
        GLsizei width = 0;
        GLsizei height = 0;

        public:
        using Texture<TARGET>::target;
        using Texture<TARGET>::Texture;
        using Texture<TARGET>::operator=;

        void Data(GLint mipmap_level, Texture2DFormat internal_format,
                  GLsizei width, GLsizei height, Texture2DFormat format,
                  TextureType type, std::nullptr_t) {
          emp_assert(Texture<TARGET>::name == Texture<TARGET>::bound,
                     "the Texture must be bound");
          this->width = width;
          this->height = height;
          emp_checked_gl_void(glTexImage2D(
            static_cast<GLenum>(target), mipmap_level,
            static_cast<GLint>(internal_format), width, height, 0,
            static_cast<GLint>(format), static_cast<GLenum>(type), nullptr));
        }

        template <typename T>
        void Data(GLint mipmap_level, Texture2DFormat internal_format,
                  GLsizei width, GLsizei height, Texture2DFormat format,
                  TextureType type, T&& data) {
          emp_assert(Texture<TARGET>::name == Texture<TARGET>::bound,
                     "the Texture must be bound");
          this->width = width;
          this->height = height;
          emp_checked_gl_void(glTexImage2D(
            static_cast<GLenum>(target), mipmap_level,
            static_cast<GLint>(internal_format), width, height, 0,
            static_cast<GLint>(format), static_cast<GLenum>(type), &data[0]));
        }

        template <typename T>
        void Data(GLint mipmap_level, Texture2DFormat internal_format,
                  GLsizei width, GLsizei height, Texture2DFormat format,
                  T&& data) {
          using value_type = std::decay_t<decltype(*std::begin(data))>;
          Data(mipmap_level, internal_format, width, height, format,
               TextureTypeOf<value_type>(), data);
        }

        template <typename T>
        void Data(GLint mipmap_level, Texture2DFormat format, GLsizei width,
                  GLsizei height, TextureType type, T&& data) {
          Data(mipmap_level, format, width, height, format, type,
               std::forward<T>(data));
        }

        template <typename T>
        void Data(GLint mipmap_level, Texture2DFormat format, GLsizei width,
                  GLsizei height, T&& data) {
          Data(mipmap_level, format, width, height, format,
               std::forward<T>(data));
        }

        template <typename T>
        void Data(Texture2DFormat format, GLsizei width, GLsizei height,
                  TextureType type, T&& data) {
          Data(0, format, width, height, format, type, std::forward<T>(data));
        }

        template <typename T>
        void Data(Texture2DFormat format, GLsizei width, GLsizei height,
                  T&& data) {
          Data(0, format, width, height, format, std::forward<T>(data));
        }

        template <typename T>
        void Data(GLsizei width, GLsizei height, T&& data) {
          using value_type = std::decay_t<decltype(*std::begin(data))>;
          Data(0, Texture2DFormatOf<value_type>(), width, height, data);
        }

        template <typename T>
        void Data(GLint mipmap_level, GLsizei width, GLsizei height, T&& data) {
          using value_type = std::decay_t<decltype(*std::begin(data))>;
          Data(mipmap_level, Texture2DFormatOf<value_type>(), width, height,
               data);
        }
        //////

        template <typename T>
        void SubData(GLint mipmap_level, GLint xoffset, GLint yoffset,
                     GLsizei width, GLsizei height, Texture2DFormat format,
                     TextureType type, T&& data) {
          emp_assert(Texture<TARGET>::name == Texture<TARGET>::bound,
                     "the Texture must be bound");

          emp_assert((xoffset + width) <= this->width &&
                       (yoffset + height) <= this.height,
                     "SubData is too large for this texture");

          emp_checked_gl_void(
            glTexSubImage2D(static_cast<GLenum>(target), mipmap_level, xoffset,
                            yoffset, width, height, static_cast<GLint>(format),
                            static_cast<GLenum>(type), &data[0]));
        }

        template <typename T>
        void SubData(GLint mipmap_level, GLint xoffset, GLint yoffset,
                     GLsizei width, GLsizei height, Texture2DFormat format,
                     T&& data) {
          using value_type = std::decay_t<decltype(data[0])>;
          SubData(mipmap_level, xoffset, yoffset, width, height, format,
                  TextureTypeOf<value_type>(), std::forward<T>(data));
        }
        template <typename T>
        void SubData(GLint mipmap_level, Texture2DFormat format, T&& data) {
          using value_type = std::decay_t<decltype(data[0])>;
          SubData(mipmap_level, 0, 0, width, height, format,
                  TextureTypeOf<value_type>(), std::forward<T>(data));
        }

        template <typename T>
        void SubData(GLint xoffset, GLint yoffset, GLsizei width,
                     GLsizei height, Texture2DFormat format, T&& data) {
          SubData(0, xoffset, yoffset, width, height, format,
                  std::forward<T>(data));
        }

        template <typename T>
        void SubData(Texture2DFormat format, T&& data) {
          SubData(0, 0, 0, width, height, format, std::forward<T>(data));
        }

        template <typename T>
        void SubData(GLint xoffset, GLint yoffset, GLsizei width,
                     GLsizei height, Texture2DFormat format, TextureType type,
                     T&& data) {
          SubData(0, xoffset, yoffset, width, height, format, type,
                  std::forward<T>(data));
        }

        template <typename T>
        void SubData(Texture2DFormat format, TextureType type, T&& data) {
          SubData(0, 0, 0, width, height, format, type, std::forward<T>(data));
        }

        template <typename T>
        void SubData(GLint xoffset, GLint yoffset, GLsizei width,
                     GLsizei height, T&& data) {
          using value_type = std::decay_t<decltype(*std::begin(data))>;
          SubData(0, xoffset, yoffset, width, height,
                  Texture2DFormatOf<value_type>(), data);
        }
        template <typename T>
        void SubData(T&& data) {
          using value_type = std::decay_t<decltype(*std::begin(data))>;
          SubData(0, 0, 0, width, height, Texture2DFormatOf<value_type>(),
                  data);
        }

        template <typename T>
        void SubData(GLint mipmap_level, GLint xoffset, GLint yoffset,
                     GLsizei width, GLsizei height, T&& data) {
          using value_type = std::decay_t<decltype(*std::begin(data))>;
          SubData(mipmap_level, xoffset, yoffset, width, height,
                  Texture2DFormatOf<value_type>(), data);
        }

        template <typename T>
        void SubData(GLint mipmap_level, T&& data) {
          using value_type = std::decay_t<decltype(*std::begin(data))>;
          SubData(mipmap_level, 0, 0, width, height,
                  Texture2DFormatOf<value_type>(), data);
        }

        void SetTextureWrap(TextureWrap s, TextureWrap t) {
          Texture<TARGET>::SetTextureWrapS(s);
          Texture<TARGET>::SetTextureWrapT(t);
        }
        auto GetWidth() const { return width; }
        auto GetHeight() const { return height; }
      };

    };  // namespace __impl_emp_opengl_texture_base

    template <TextureBindTarget>
    class Texture;

    template <>
    class Texture<TextureBindTarget::TwoDimensional>
      : public __impl_emp_opengl_texture_base::Texture2d<
          TextureBindTarget::TwoDimensional> {
      using __impl_emp_opengl_texture_base::Texture2d<
        TextureBindTarget::TwoDimensional>::target;
      using __impl_emp_opengl_texture_base::Texture2d<target>::Texture2d;
    };

    using Texture2d = Texture<TextureBindTarget::TwoDimensional>;

    template <TextureBindTarget TARGET>
    void setUniform(
      GLint uniform,
      const __impl_emp_opengl_texture_base::Texture<TARGET>& texture) {
      emp_checked_gl_void(glUniform1i(uniform, texture.texture - GL_TEXTURE0));
    }

  }  // namespace opengl
}  // namespace emp

#endif
