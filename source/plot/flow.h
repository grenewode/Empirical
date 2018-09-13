#ifndef EMP_PLOT_FLOW_H
#define EMP_PLOT_FLOW_H

#include <algorithm>
#include <iterator>
#include <memory>
#include <vector>

#include "scenegraph/rendering.h"
#include "tools/attrs.h"

namespace emp {
  namespace plot {

    namespace impl__emp_flow {
      struct generic_deref {
        template <typename T>
        static constexpr T& deref(T& target) {
          return target;
        }
        template <typename T>
        static constexpr const T& deref(const T& target) {
          return target;
        }

        template <typename T>
        static constexpr T& deref(T* target) {
          return *target;
        }
        template <typename T>
        static constexpr const T& deref(const T* target) {
          return *target;
        }

        template <typename T>
        static constexpr T& deref(std::shared_ptr<T> target) {
          return *target;
        }
      };

    }  // namespace impl__emp_flow

    template <typename...>
    class Sinks;
    template <typename, typename>
    class Source;

    template <>
    class Sinks<> {
      public:
      constexpr Sinks() = default;
      constexpr Sinks(const Sinks&) = default;
      constexpr Sinks(Sinks&&) = default;
      constexpr Sinks& operator=(const Sinks&) = default;
      constexpr Sinks& operator=(Sinks&&) = default;

      template <typename INPUT_ITER>
      constexpr void operator()(INPUT_ITER begin, INPUT_ITER end) const {}

      template <typename NEXT>
      constexpr Sinks<std::decay_t<NEXT>> Then(NEXT&& next) const {
        return {std::forward<NEXT>(next), *this};
      }

      template <typename MAP>
      constexpr Source<std::decay_t<MAP>, Sinks> Data(MAP&& map) const {
        return {std::forward<MAP>(map), *this};
      }
    };

    Sinks<> MakeFlow() { return {}; }

    template <typename HEAD, typename... TAIL>
    class Sinks<HEAD, TAIL...> {
      private:
      HEAD head;
      Sinks<TAIL...> tail;

      public:
      template <typename H, typename T>
      Sinks(H&& head, T&& tail)
        : head(std::forward<H>(head)), tail(std::forward<T>(tail)) {}

      constexpr Sinks() = delete;
      constexpr Sinks(const Sinks&) = default;
      constexpr Sinks(Sinks&&) = default;
      constexpr Sinks& operator=(const Sinks&) = default;
      constexpr Sinks& operator=(Sinks&&) = default;

      private:
      template <typename INPUT_ITER>
      constexpr auto impl_CallHead(INPUT_ITER begin, INPUT_ITER end) const {
        std::cout << "HEAD" << sizeof...(TAIL) << std::endl;
        return impl__emp_flow::generic_deref::deref(head)(begin, end);
      }

      template <typename INPUT_ITER>
      constexpr auto impl_CallTail(INPUT_ITER begin, INPUT_ITER end) const {
        std::cout << "TAIL" << sizeof...(TAIL) << std::endl;
        return impl__emp_flow::generic_deref::deref(tail)(begin, end);
      }

      template <typename INPUT_ITER>
      constexpr auto impl_Apply(
        INPUT_ITER begin, INPUT_ITER end,
        const std::false_type& tail_is_not_transform) const {
        auto tmp{impl_CallHead(begin, end)};
        std::cout << "-->>: " << sizeof...(TAIL) << tmp[0] << std::endl;
        return impl_CallTail(std::begin(tmp), std::end(tmp));
      }

      template <typename INPUT_ITER>
      constexpr auto impl_Apply(
        INPUT_ITER begin, INPUT_ITER end,
        const std::true_type& tail_is_not_transform) const {
        impl_CallHead(begin, end);
        std::cout << "-->>: void " << sizeof...(TAIL) << std::endl;
        // Note that it's safe to reuse begin and end here, because they get
        // COPYED into impl_CallTail, not passed by reference.
        return impl_CallTail(begin, end);
      }

      public:
      template <typename INPUT_ITER>
      constexpr auto operator()(INPUT_ITER begin, INPUT_ITER end) const {
        return impl_Apply(begin, end,
                          std::is_void<decltype(impl_CallHead(begin, end))>{});
      }

      template <typename NEXT>
      constexpr Sinks<std::decay_t<NEXT>, HEAD, TAIL...> Then(
        NEXT&& next) const {
        return {std::forward<NEXT>(next), *this};
      }

      template <typename... MAPPING>
      constexpr auto Data(MAPPING&&... mapping) const {
        return Source<decltype(MakeAttrs(std::forward<MAPPING>(mapping)...)),
                      Sinks>{MakeAttrs(std::forward<MAPPING>(mapping)...),
                             *this};
      }
    };  // namespace plot

    template <typename MAP, typename TARGET>
    class Source {
      public:
      using map_t = MAP;
      using target_t = TARGET;

      private:
      map_t map;
      target_t target;

      public:
      template <typename M, typename T>
      Source(M&& map, T&& target)
        : map(std::forward<M>(map)), target(std::forward<T>(target)) {}

      constexpr Source() = default;
      constexpr Source(const Source&) = default;
      constexpr Source(Source&&) = default;
      constexpr Source& operator=(const Source&) = default;
      constexpr Source& operator=(Source&&) = default;

      template <typename DATA_ITER>
      void operator()(DATA_ITER begin, DATA_ITER end) const {
        // Todo: setup typesafe cacheing
        std::vector<decltype(map(*begin))> mapped;

        std::transform(begin, end, std::back_inserter(mapped), map);

        impl__emp_flow::generic_deref::deref(target)(mapped.begin(),
                                                     mapped.end());
      }
    };

  };  // namespace plot

}  // namespace emp

#endif  // EMP_PLOT_FLOW_H
