#ifndef GL_UTILS_H
#define GL_UTILS_H

#ifndef NDEBUG
#include <iostream>
#endif

#include "assert.h"

#include <ostream>

#include "gl.h"

namespace emp {
  namespace opengl {
    namespace utils {
      enum class GlError : GLenum {
        NoError = GL_NO_ERROR,
        InvalidEnum = GL_INVALID_ENUM,
        InvalidValue = GL_INVALID_VALUE,
        InvalidOperation = GL_INVALID_OPERATION,
        InvalidFramebufferOperation = GL_INVALID_FRAMEBUFFER_OPERATION,
        OutOfMemory = GL_OUT_OF_MEMORY,
      };

      std::ostream& operator<<(std::ostream& out, const GlError& error) {
        switch (error) {
          case GlError::NoError:
            return out << "NO_ERROR";
          case GlError::InvalidEnum:
            return out << "INVALID_ENUM";
          case GlError::InvalidValue:
            return out << "INVALID_VALUE";
          case GlError::InvalidOperation:
            return out << "INVALID_OPERATION";
          case GlError::InvalidFramebufferOperation:
            return out << "INVALID_FRAMEBUFFER_OPERATION";
          case GlError::OutOfMemory:
            return out << "OUT_OF_MEMORY";
          default:
            return out;
        }
      }

#ifndef NDEBUG
      bool emp_get_gl_errors(const char* file, int line, const char* call) {
        GLenum first_error{glGetError()};
        for (GLenum error{first_error}; error != GL_NO_ERROR;
             error = glGetError()) {
          std::cerr << file << ": " << line << ": error in call to " << call
                    << ": " << static_cast<emp::opengl::utils::GlError>(error)
                    << std::endl;
        }

        return first_error == GL_NO_ERROR;
      }
#define emp_checked_gl(GL_CALL)                                            \
  ([&] {                                                                   \
    auto result{GL_CALL};                                                  \
    emp_assert(                                                            \
      emp::opengl::utils::emp_get_gl_errors(__FILE__, __LINE__, #GL_CALL), \
      "expected no errors in " #GL_CALL);                                  \
    return result;                                                         \
  })()
#define emp_checked_gl_void(GL_CALL)                                       \
  ([&] {                                                                   \
    GL_CALL;                                                               \
    emp_assert(                                                            \
      emp::opengl::utils::emp_get_gl_errors(__FILE__, __LINE__, #GL_CALL), \
      "expected no errors in " #GL_CALL);                                  \
  })()
#else

#define emp_checked_gl(GL_CALL) GL_CALL
#define emp_checked_gl_void(GL_CALL) GL_CALL
#endif

    }  // namespace utils
  }  // namespace opengl
}  // namespace emp

#endif  // GL_UTILS_H
