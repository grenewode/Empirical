#ifndef GLWRAP_H
#define GLWRAP_H

#include "gl.h"

#include <cstdint>
#include <iostream>
#include <ostream>
#include <unordered_map>
#include <vector>

#include "VertexAttributes.h"
#include "base/assert.h"
#include "glutils.h"
#include "texture.h"

namespace emp {
  namespace opengl {

    enum class BufferType : GLenum {
      Array = GL_ARRAY_BUFFER,
      // AtomicCounter = GL_ATOMIC_COUNTER_BUFFER,
      CopyRead = GL_COPY_READ_BUFFER,
      CopyWrite = GL_COPY_WRITE_BUFFER,
      // DrawIndirect = GL_DRAW_INDIRECT_BUFFER,
      // DispatchIndirect = GL_DISPATCH_INDIRECT_BUFFER,
      ElementArray = GL_ELEMENT_ARRAY_BUFFER,
      PixelPack = GL_PIXEL_PACK_BUFFER,
      PixelUnpack = GL_PIXEL_UNPACK_BUFFER,
      // ShaderStorage = GL_SHADER_STORAGE_BUFFER,
      TransformFeedback = GL_TRANSFORM_FEEDBACK_BUFFER,
      Uniform = GL_UNIFORM_BUFFER
    };

    std::ostream& operator<<(std::ostream& out, const BufferType& buffer) {
      switch (buffer) {
        case BufferType::Array:
          return out << "GL_ARRAY_BUFFER";
        case BufferType::CopyRead:
          return out << "GL_COPY_READ_BUFFER";
        case BufferType::CopyWrite:
          return out << "GL_COPY_WRITE_BUFFER";
        case BufferType::ElementArray:
          return out << "GL_ELEMENT_ARRAY_BUFFER";
        case BufferType::PixelPack:
          return out << "GL_PIXEL_PACK_BUFFER";
        case BufferType::PixelUnpack:
          return out << "GL_PIXEL_UNPACK_BUFFER";
        case BufferType::TransformFeedback:
          return out << "GL_TRANSFORM_FEEDBACK_BUFFER";
        case BufferType::Uniform:
        default:
          return out << "GL_UNIFORM_BUFFER";
      }
    }
#ifndef EMSCRIPTEN
    class BufferAccess {
      private:
      GLenum access;

      constexpr BufferAccess(GLenum access) : access(access) {}

      public:
      static constexpr BufferAccess read() { return {GL_MAP_READ_BIT}; };
      static constexpr BufferAccess write() { return {GL_MAP_WRITE_BIT}; };

      BufferAccess& invalidatesRange(bool set = true) {
        access |= GL_MAP_INVALIDATE_RANGE_BIT & set;
        return *this;
      }

      BufferAccess& invalidatesBuffer(bool set = true) {
        access |= GL_MAP_INVALIDATE_BUFFER_BIT & set;
        return *this;
      }

      BufferAccess& explicitFlush(bool set = true) {
        access |= GL_MAP_FLUSH_EXPLICIT_BIT & set;
        return *this;
      }

      BufferAccess& unsynchronized(bool set = true) {
        access |= GL_MAP_UNSYNCHRONIZED_BIT & set;
        return *this;
      }

      explicit operator GLenum() const { return access; }
    };
#endif

    enum class BufferUsage : GLenum {
      StreamDraw = GL_STREAM_DRAW,
      StreamRead = GL_STREAM_READ,
      StreamCopy = GL_STREAM_COPY,
      StaticDraw = GL_STATIC_DRAW,
      StaticRead = GL_STATIC_READ,
      StaticCopy = GL_STATIC_COPY,
      DynamicDraw = GL_DYNAMIC_DRAW,
      DynamicRead = GL_DYNAMIC_READ,
      DynamicCopy = GL_DYNAMIC_COPY
    };

    namespace emp__impl_BufferObject_bound_buffer {
      GLuint bound_buffer = 0;
    }

    template <BufferType TYPE>
    class BufferObject {
      private:
      GLuint handle = 0;

      public:
      BufferObject(GLuint handle) : handle(handle) {}

      BufferObject(const BufferObject&) = delete;
      BufferObject(BufferObject&& other) noexcept : handle(other.handle) {
        other.handle = 0;
      }

      BufferObject& operator=(const BufferObject&) = delete;
      BufferObject& operator=(BufferObject&& other) noexcept {
        if (this != &other) {
          destroy();

          std::swap(handle, other.handle);
        }
        return *this;
      }

      ~BufferObject() { destroy(); }

      void destroy() {
        if (handle != 0) {
          emp_checked_gl_void(glDeleteBuffers(1, &handle));
          if (emp__impl_BufferObject_bound_buffer::bound_buffer == handle)
            emp__impl_BufferObject_bound_buffer::bound_buffer = 0;
        }
      }

      template <typename T>
      void init(const T* data, std::size_t size, BufferUsage usage) {
        emp_assert(handle == emp__impl_BufferObject_bound_buffer::bound_buffer,
                   "the BufferObject must be bound");
        emp_checked_gl_void(glBufferData(static_cast<GLenum>(TYPE), size, data,
                                         static_cast<GLenum>(usage)));
      }

      template <typename T, std::size_t N>
      void init(const T (&data)[N], BufferUsage usage) {
        init(&data, sizeof(data), usage);
      }

      template <typename T>
      void init(const std::vector<T>& data, BufferUsage usage) {
        init(data.data(), sizeof(T) * data.size(), usage);
      }

      template <typename T>
      void subset(const T* data, std::size_t size, std::size_t offset = 0) {
        emp_assert(handle == emp__impl_BufferObject_bound_buffer::bound_buffer,
                   "the BufferObject must be bound");
        emp_checked_gl_void(
          glBufferSubData(static_cast<GLenum>(TYPE), offset, size, data));
      }

      template <typename T, std::size_t N>
      void subset(const T (&data)[N]) {
        subset(&data, sizeof(data));
      }

      template <typename T>
      void subset(const std::vector<T>& data) {
        subset(data.data(), sizeof(T) * data.size());
      }
#ifndef EMSCRIPTEN
      template <class T>
      T* map(std::size_t offset, std::size_t length, BufferAccess access) {
        emp_assert(handle == emp__impl_BufferObject_bound_buffer::bound_buffer,
                   "the BufferObject must be bound");
        auto buffer = static_cast<T*>(emp_checked_gl(
          glMapBufferRange(static_cast<GLenum>(TYPE), offset, length,
                           static_cast<GLenum>(access))));
        return buffer;
      }

      template <class T>
      T* map(std::size_t length, BufferAccess access) {
        return map<T>(0, length, access);
      }

      bool unmap() {
        emp_assert(handle == emp__impl_BufferObject_bound_buffer::bound_buffer,
                   "the BufferObject must be bound");
        auto unmap = emp_checked_gl(glUnmapBuffer(static_cast<GLenum>(TYPE)));
        return unmap;
      }
#endif

      BufferObject& bind() {
        emp_assert(handle != 0, "the BufferObject must have been created");
        emp_assert(handle != emp__impl_BufferObject_bound_buffer::bound_buffer,
                   "the BufferObject must not be bound");
        emp_checked_gl_void(glBindBuffer(static_cast<GLenum>(TYPE), handle));
        emp__impl_BufferObject_bound_buffer::bound_buffer = handle;

        return *this;
      }

      operator bool() const { return handle != 0; }
      explicit operator GLuint() const { return handle; }
    };
#ifndef EMSCRIPTEN
    template <class F, BufferType... TYPES>
    void mapBuffers(F&& callback, BufferAccess access,
                    BufferObject<TYPES>&... buffers) {
      // Map all the buffers and send them into a callback to use them
      std::forward<F>(callback)(buffers.map(access)...);
      // Now that we are done with the buffers, unmap them
      // Hack to get around not having the c++17 fold expressions
      auto _ = [](auto&&...) {};
      _(buffers.unmap()...);
    }
#endif

    namespace __impl_VertexBuffer {
      template <BufferType TYPE, typename T>
      class BufferVector : public BufferObject<TYPE> {
        protected:
        std::vector<T> data;
        size_t gpu_buffer_capacity = 0;

        public:
        BufferVector(const BufferObject<TYPE>& buffer)
          : BufferObject<TYPE>(buffer) {}
        BufferVector(BufferObject<TYPE>&& buffer)
          : BufferObject<TYPE>(std::move(buffer)) {}

        BufferVector(BufferVector&& other)
          : BufferObject<TYPE>(std::move(other)), data(std::move(other.data)) {}

        BufferVector& operator=(BufferVector&& other) {
          BufferObject<TYPE>::operator=(std::move(other.gpu_buffer));
          data = std::move(other.data);
          return *this;
        }

        private:
        void impl__PushData() {}

        template <typename U = T, typename... Us>
        void impl__PushData(U&& i, Us&&... us) {
          data.push_back(std::forward<U>(i));
          impl__PushData(std::forward<Us>(us)...);
        }

        public:
        template <typename U = T, typename... Us>
        size_t PushData(U&& i) {
          data.push_back(std::forward<U>(i));
          return data.size() - 1;
        }
        template <typename U0 = T, typename U1, typename... U>
        void PushData(U0&& u0, U1&& u1, U&&... u) {
          impl__PushData(std::forward<U0>(u0), std::forward<U1>(u1),
                         std::forward<U>(u)...);
        }

        template <typename U0 = T, typename... U>
        void EmplaceData(U0&& arg0, U&&... args) {
          data.emplace_back(std::forward<U0>(arg0), std::forward<U>(args)...);
        }

        template <typename Iter>
        void PushCollection(Iter begin, Iter end) {
          data.insert(data.end(), begin, end);
        }

        template <typename C>
        void PushCollection(const C& collection) {
          PushAll(std::begin(collection), std::end(collection));
        }

        void Reserve(size_t size, BufferUsage usage) {
          data.reserve(size);
          if (size > gpu_buffer_capacity) {
            BufferObject<TYPE>::init(nullptr, size, usage);
            gpu_buffer_capacity = size;
          }
        }

        void Clear() { data.clear(); }

        auto Size() const { return data.size(); }
        auto GpuCapacity() const { return gpu_buffer_capacity; }

        T& operator[](size_t i) { return data[i]; }
        const T& operator[](size_t i) const { return data[i]; }

        void SendToGPU(BufferUsage usage = BufferUsage::DynamicDraw) {
          if (data.size() > gpu_buffer_capacity) {
            BufferObject<TYPE>::init(data, usage);
            gpu_buffer_capacity = data.size();
          } else {
#ifdef EMSCRIPTEN
            BufferObject<TYPE>::subset(data);
#else
            auto mem_mapped_buffer = BufferObject<TYPE>::template map<T>(
              data.size(), BufferAccess::write().invalidatesBuffer());

            std::copy(data.begin(), data.end(), mem_mapped_buffer);
            BufferObject<TYPE>::unmap();
#endif
          }
        }
      };
    }  // namespace __impl_VertexBuffer
    template <BufferType TYPE, typename T>
    class BufferVector : public __impl_VertexBuffer::BufferVector<TYPE, T> {
      public:
      using __impl_VertexBuffer::BufferVector<TYPE, T>::BufferVector;
    };

    template <typename T>
    class BufferVector<BufferType::Array, T>
      : public __impl_VertexBuffer::BufferVector<BufferType::Array, T> {
      public:
      using __impl_VertexBuffer::BufferVector<BufferType::Array,
                                              T>::BufferVector;

      void Draw(GLenum mode, int start = 0, int count = -1) {
        if (count < 0) {
          count = __impl_VertexBuffer::BufferVector<BufferType::Array, T>::data
                    .size();
          count -= start;
        }
        emp_checked_gl_void(glDrawArrays(mode, start, count));
      }
    };

    template <typename T>
    class BufferVector<BufferType::ElementArray, T>
      : public __impl_VertexBuffer::BufferVector<BufferType::ElementArray, T> {
      public:
      using __impl_VertexBuffer::BufferVector<BufferType::ElementArray,
                                              T>::BufferVector;
      void Draw(GLenum mode) {
        emp_checked_gl_void(glDrawElements(
          mode,
          __impl_VertexBuffer::BufferVector<BufferType::ElementArray, T>::data
            .size(),
          GL_UNSIGNED_INT, nullptr));
      }
    };

    class VertexAttribute {
      private:
      GLuint index;
      VertexAttributeSize size;
      VertexAttributeType type;
      GLsizei stride;
      const void* offset;

      public:
      VertexAttribute(GLuint index, VertexAttributeSize size,
                      VertexAttributeType type, GLsizei stride = 0,
                      const void* offset = nullptr)
        : index(index),
          size(size),
          type(type),
          stride(stride),
          offset(offset) {}

      void apply() {
        emp_checked_gl_void(
          glVertexAttribIPointer(index, static_cast<GLint>(size),
                                 static_cast<GLenum>(type), stride, offset));

        emp_checked_gl_void(glEnableVertexAttribArray(index));
      }
    };

    class FloatingVertexAttribute {
      private:
      GLuint index;
      VertexAttributeSize size;
      FloatingVertexAttributeType type;
      bool normalized;
      GLsizei stride;
      const void* offset;

      public:
      FloatingVertexAttribute(GLuint index, VertexAttributeSize size,
                              FloatingVertexAttributeType type,
                              bool normalized = false, GLsizei stride = 0,
                              const void* offset = nullptr)
        : index(index),
          size(size),
          type(type),
          normalized(normalized),
          stride(stride),
          offset(offset) {}

      void apply() {
        emp_checked_gl_void(glVertexAttribPointer(
          index, static_cast<GLint>(size), static_cast<GLenum>(type),
          normalized, stride, offset));

        emp_checked_gl_void(glEnableVertexAttribArray(index));
      }
    };

    template <typename... C>
    void applyAll(C&&...);

    template <typename H, typename... T>
    void applyAll(H&& head, T&&... tail) {
      std::forward<H>(head).apply();
      applyAll(std::forward<T>(tail)...);
    }

    template <>
    void applyAll() {}

    class VertexArrayObject {
      private:
      static GLuint boundVAO;
      GLuint handle = 0;

      public:
      explicit VertexArrayObject() {
        emp_checked_gl_void(glGenVertexArrays(1, &handle));
      }

      explicit VertexArrayObject(GLuint handle) : handle(handle) {}
      VertexArrayObject(const VertexArrayObject&) = delete;
      VertexArrayObject(VertexArrayObject&& other) : handle(other.handle) {
        other.handle = 0;
      }

      VertexArrayObject& operator=(const VertexArrayObject&) = delete;
      VertexArrayObject& operator=(VertexArrayObject&& other) {
        if (this != &other) {
          destoy();
          std::swap(handle, other.handle);
        }
        return *this;
      }

      ~VertexArrayObject() { destoy(); }

      void destoy() {
        if (handle != 0) {
          unbind();
          emp_checked_gl_void(glDeleteVertexArrays(1, &handle));
          handle = 0;
        }
      }

      void bind() {
        emp_assert(handle != 0, "the VertexArrayObject must have been created");
        if (boundVAO != handle) {
          emp_checked_gl_void(glBindVertexArray(handle));
          boundVAO = handle;
        }
      }

      void unbind() {
        emp_assert(handle != 0, "the VertexArrayObject must have been created");
        emp_assert(handle == boundVAO, "the VertexArrayObject must be bound");
        emp_checked_gl_void(glBindVertexArray(0));
        boundVAO = 0;
      }

      template <typename... T>
      VertexArrayObject& attr(T&&... vertexAttributes) {
        emp_assert(handle == boundVAO, "the VertexArrayObject must be bound");
        applyAll(std::forward<T>(vertexAttributes)...);

        return *this;
      }

      operator bool() const { return handle != 0; }
      operator GLuint() const { return handle; }
    };

    GLuint VertexArrayObject::boundVAO = 0;

    enum class RenderbufferFormat : GLenum {
      RGBA4 = GL_RGB4,
      RGB565 = GL_RGB565,
      RGB5_A1 = GL_RGB5_A1,
      DepthComponent16 = GL_DEPTH_COMPONENT16,
      StencilIndex8 = GL_STENCIL_INDEX8,
      Depth24Stencil8 = GL_DEPTH24_STENCIL8
    };

    class Renderbuffer {
      private:
      static GLuint bound_renderbuffer;
      GLuint handle = 0;
      RenderbufferFormat format;

      public:
      explicit Renderbuffer(RenderbufferFormat format) : format(format) {
        emp_checked_gl_void(glGenRenderbuffers(1, &handle));
      }
      Renderbuffer(const Renderbuffer&) = delete;

      Renderbuffer(Renderbuffer&& other) : handle(other.handle) {
        other.handle = 0;
      }

      Renderbuffer& operator=(const Renderbuffer&) = delete;
      Renderbuffer& operator=(Renderbuffer&& other) {
        if (this != &other) {
          Delete();
          handle = other.handle;
          other.handle = 0;
          format = other.format;
        }
        return *this;
      }

      ~Renderbuffer() { Delete(); }

      void Delete() {
        if (handle != 0) {
          emp_checked_gl_void(glDeleteRenderbuffers(1, &handle));
          handle = 0;
        }
      }

      void Bind() {
        emp_assert(handle != 0,
                   "Cannot bind an buffer which is not initialized");
        if (handle != bound_renderbuffer) {
          emp_checked_gl_void(glBindRenderbuffer(GL_RENDERBUFFER, handle));
          bound_renderbuffer = handle;
        }
      }

      void Unbind() {
        emp_assert(handle == bound_renderbuffer,
                   "the renderbuffer must be bound");
        emp_checked_gl_void(glBindBuffer(GL_RENDERBUFFER, 0));
        bound_renderbuffer = 0;
      }

      void Store(int width, int height) { Store(format, width, height); }

      void Store(RenderbufferFormat format, int width, int height) {
        emp_assert(handle == bound_renderbuffer,
                   "the renderbuffer must be bound");
        emp_checked_gl_void(glRenderbufferStorage(
          GL_RENDERBUFFER, static_cast<GLenum>(format), width, height));
        this->format = format;
      }

      RenderbufferFormat GetFormat() const { return format; }

      operator GLuint() const { return handle; }
      operator bool() const { return handle != 0; }
    };

    GLuint Renderbuffer::bound_renderbuffer = 0;

    enum class FramebufferAttachment : GLenum {
      Color0 = GL_COLOR_ATTACHMENT0,
      Depth = GL_DEPTH_ATTACHMENT,
      Stencil = GL_STENCIL_ATTACHMENT,
      DepthStencil = GL_DEPTH_STENCIL_ATTACHMENT
    };

    class Framebuffer {
      private:
      static GLuint bound_framebuffer;
      GLuint handle = 0;

      public:
      explicit Framebuffer() {
        emp_checked_gl_void(glGenFramebuffers(1, &handle));
      }

      Framebuffer(const Framebuffer&) = delete;
      Framebuffer(Framebuffer&& other) : handle(other.handle) {
        other.handle = 0;
      }
      Framebuffer& operator=(const Framebuffer&) = delete;
      Framebuffer& operator=(Framebuffer&& other) {
        if (&other != this) {
          Delete();
          handle = other.handle;
          other.handle = 0;
        }
        return *this;
      }

      ~Framebuffer() { Delete(); }

      void Bind() {
        if (handle != bound_framebuffer) {
          bound_framebuffer = handle;
          emp_checked_gl_void(glBindFramebuffer(GL_FRAMEBUFFER, handle));
        }
      }

      void Unbind() {
        emp_assert(handle == bound_framebuffer,
                   "the framebuffer must be bound");
        bound_framebuffer = 0;
        emp_checked_gl_void(glBindFramebuffer(GL_FRAMEBUFFER, 0));
      }

      void Attach(
        const Texture2d& target,
        FramebufferAttachment attachement = FramebufferAttachment::Color0,
        int mipmap_level = 0) {
        emp_assert(handle == bound_framebuffer,
                   "the framebuffer must be bound");
        emp_checked_gl_void(glFramebufferTexture2D(
          GL_FRAMEBUFFER, static_cast<GLenum>(attachement), GL_TEXTURE_2D,
          target, mipmap_level));
      }

      void Attach(const Renderbuffer& renderbuffer,
                  FramebufferAttachment attachement) {
        emp_assert(handle == bound_framebuffer,
                   "the framebuffer must be bound");
        emp_checked_gl_void(glFramebufferRenderbuffer(
          GL_FRAMEBUFFER, static_cast<GLenum>(attachement), GL_RENDERBUFFER,
          renderbuffer));
      }

      void Delete() {
        if (*this) {
          Unbind();
          emp_checked_gl_void(glDeleteFramebuffers(1, &handle));
        }
      }

      bool IsComplete() const {
        emp_assert(handle == bound_framebuffer,
                   "the framebuffer must be bound");
        return emp_checked_gl(glCheckFramebufferStatus(GL_FRAMEBUFFER)) ==
               GL_FRAMEBUFFER_COMPLETE;
      }

      operator bool() const { return handle != 0; }
      operator GLuint() const { return handle; }
    };

    GLuint Framebuffer::bound_framebuffer = 0;

  }  // namespace opengl
}  // namespace emp
#endif
