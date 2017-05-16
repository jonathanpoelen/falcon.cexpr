/* The MIT License (MIT)

Copyright (c) 2016 jonathan poelen

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

/**
* \author    Jonathan Poelen <jonathan.poelen+falcon@gmail.com>
* \version   0.1
* \brief     Compile time selection statements
*/

#pragma once

#include <falcon/cxx/cxx.hpp>

#include <initializer_list>
#include <type_traits>
#include <utility>

namespace falcon { namespace cstmt {

template<class T>
struct cbool_trait;

template<> struct cbool_trait<std::true_type> : std::true_type {};
template<> struct cbool_trait<std::false_type> : std::false_type {};

template<class T>
using cbool_trait_t = typename cbool_trait<T>::type;

#if FALCON_CXX_FEATURE_VARIABLE_TEMPLATES
template<class T>
constexpr auto cbool_trait_v = cbool_trait_t<T>{};
#endif

template<class T>
constexpr cbool_trait_t<T> cbool(T const &) noexcept
{ return {}; }

constexpr std::true_type cbool(std::true_type) noexcept
{ return {}; }

constexpr std::false_type cbool(std::false_type) noexcept
{ return {}; }


class select_fn
{
  template<class True, class False>
  static constexpr True _(std::true_type, True && yes, False &&)
  FALCON_RETURN_NOEXCEPT(std::forward<True>(yes))

  template<class True, class False>
  static constexpr False _(std::false_type, True &&, False && no)
  FALCON_RETURN_NOEXCEPT(std::forward<False>(no))

public:
  /// \return \p yes if \p cond is a \c true constant expression, otherwise \p no
  template<class B, class True, class False>
  constexpr auto operator()(B && cond, True && yes, False && no) const
  FALCON_DECLTYPE_AUTO_RETURN_NOEXCEPT(
    _(cbool(cond), std::forward<True>(yes), std::forward<False>(no))
  )
};
FALCON_SCOPED_INLINE_VARIABLE(select_fn, select)


class cif_fn
{
  template<class True, class False>
  static constexpr auto _(std::true_type, True && yes, False &&)
  FALCON_DECLTYPE_AUTO_RETURN_NOEXCEPT(std::forward<True>(yes)())

  template<class True, class False>
  static constexpr auto _(std::false_type, True &&, False && no)
  FALCON_DECLTYPE_AUTO_RETURN_NOEXCEPT(std::forward<False>(no)())

  template<class True>
  static constexpr auto _(std::true_type, True && yes)
  FALCON_DECLTYPE_AUTO_RETURN_NOEXCEPT(std::forward<True>(yes)())

  template<class True>
  static constexpr void _(std::false_type, True &&)
  {}

public:
  /// \return \p yes() if \p cond is a \c true constant expression, otherwise \p no()
  template<class B, class True, class False>
  constexpr auto operator()(B && cond, True && yes, False && no) const
  FALCON_DECLTYPE_AUTO_RETURN_NOEXCEPT(
    _(cbool(cond), std::forward<True>(yes), std::forward<False>(no))
  )

  /// \return \p yes() if \p cond is a \c true constant expression, otherwise \c void
  template<class B, class True>
  constexpr auto operator()(B && cond, True && yes) const
  FALCON_DECLTYPE_AUTO_RETURN_NOEXCEPT(
    _(cbool(cond), std::forward<True>(yes))
  )
};
FALCON_SCOPED_INLINE_VARIABLE(cif_fn, cif)


FALCON_EMPTY_CLASS(nodefault_t);
FALCON_SCOPED_INLINE_VARIABLE(nodefault_t, nodefault)


namespace detail
{
  template<class T, class I>
  struct rswitch_result
  {
    template<class Func, class DefaultFunc, class Int, Int... ic>
    using type = T;
  };

  class auto_;

  template<class I, class Ints, I i, Ints... ic>
  struct not_contains_int_impl
  {
    using type = typename std::is_same<
      std::integer_sequence<bool, (ic == i)...>,
      std::integer_sequence<bool, ((void)ic, false)...>
    >::type;
  };

  template<class I, class Ints, I i, Ints... ic>
  using not_contains_int = typename not_contains_int_impl<I, Ints, i, ic...>::type;

  template<class I>
  struct rswitch_result<auto_, I>
  {
    template<class Func, class DefaultFunc, class Int, Int... ic>
    using type = std::common_type_t<
      decltype(std::declval<DefaultFunc>()(std::declval<I>())),
      decltype(std::declval<Func>()(std::integral_constant<Int, ic>{}))...
    >;
  };

  template<class I, I i>
  struct rswitch_result<auto_, std::integral_constant<I, i>>
  {
    template<class Func, class DefaultFunc, class Int, Int... ic>
    using type = decltype(select(
      not_contains_int<I, Int, i, ic...>{},
      std::declval<DefaultFunc>(),
      std::declval<Func>()
    )(std::integral_constant<I, i>{}));
  };

  template<
    class T, class Func, class DefaultFunc, class I, class Int, Int... ic>
  using rswitch_result_t
    = typename rswitch_result<T, I>
      ::template type<Func&&, DefaultFunc&&, Int, ic...>;

  template<
    class T, class Int, Int... ic, class I, class Func, class DefaultFunc>
  constexpr rswitch_result_t<T, Func, DefaultFunc, I, Int, ic...>
  rswitch(
    std::integer_sequence<Int, ic...> ints,
    I const & i, Func && func, DefaultFunc && default_func
  );

  template<
    class T, class Int, Int... ic, class I, I i, class Func, class DefaultFunc>
  constexpr rswitch_result_t<T, Func, DefaultFunc, std::integral_constant<I, i>, Int, ic...>
  rswitch(
    std::integer_sequence<Int, ic...> ints,
    std::integral_constant<I, i> ii,
    Func && func, DefaultFunc && default_func
  );


  struct noop
  {
    template<class T>
    constexpr void operator()(T const &) const noexcept
    {}
  };
}

struct cswitch_fn
{
  /// \brief \p func(\c ic) if \p i equals \c ic
  template<class Int, Int... ic, class I, class Func>
  constexpr void operator()(
    std::integer_sequence<Int, ic...> ints,
    I const & i, Func && func, nodefault_t
  ) const
  {
    detail::rswitch<void>(ints, i, std::forward<Func>(func), detail::noop{});
  }

  /// \brief \p func(\c ic) if \p i equals \c ic, otherwise \p default_func(\p i)
  template<class Int, Int... ic, class I, class Func, class DefaultFunc>
  constexpr void operator()(
    std::integer_sequence<Int, ic...> ints,
    I const & i, Func && func, DefaultFunc && default_func
  ) const
  {
    detail::rswitch<void>(
      ints, i,
      std::forward<Func>(func),
      std::forward<DefaultFunc>(default_func)
    );
  }

  /// \brief shortcut for \c cswitch(\p ints, \p i, \p func, \p func)
  template<class Ints, class I, class Func>
  constexpr void operator()(Ints ints, I const & i, Func && func) const
  {
    detail::rswitch<void>(
      ints, i,
      std::forward<Func>(func),
      std::forward<Func>(func)
    );
  }
};
FALCON_SCOPED_INLINE_VARIABLE(cswitch_fn, cswitch)


namespace detail
{
  template<class T>
  struct make_value
  {
    template<class U>
    constexpr T operator()(U const &) const
    FALCON_RETURN_NOEXCEPT(T{})
  };

  template<>
  struct make_value<void>
  {
    template<class U>
    constexpr void operator()(U const &) const noexcept
    {}
  };

  template<class Int, Int... ic, class Func, class DefaultFunc>
  DefaultFunc && wrap_default_func(
    std::integer_sequence<Int, ic...>,
    Func &&, DefaultFunc && default_func
  )
  {
    return std::forward<DefaultFunc>(default_func);
  }

  template<class Int, Int... ic, class Func>
  auto wrap_default_func(
    std::integer_sequence<Int, ic...>,
    Func && func, nodefault_t
  ) -> detail::make_value<std::common_type_t<decltype(
    std::forward<Func>(func)(std::integral_constant<Int, ic>{})
  )...>>
  {
    return {};
  }
}

struct rswitch_fn
{
  /// \return \p func(\p ic) if \p i equals \c ic, otherwise \p default_func(\p i)
  template<class Int, Int... ic, class I, class Func, class DefaultFunc>
  constexpr auto operator()(
    std::integer_sequence<Int, ic...> ints,
    I const & i, Func && func, DefaultFunc && default_func
  ) const
  FALCON_DECLTYPE_AUTO_RETURN(
    detail::rswitch<detail::auto_>(
      ints, i,
      std::forward<Func>(func),
      detail::wrap_default_func(
        ints,
        std::forward<Func>(func),
        std::forward<DefaultFunc>(default_func)
      )
    )
  )

  /// \brief shortcut for \c rswitch(\c ints, \p i, \p func, \p func)
  template<class Int, Int... ic, class I, class Func>
  constexpr auto operator()(
    std::integer_sequence<Int, ic...> ints,
    I const & i, Func && func
  ) const
  FALCON_DECLTYPE_AUTO_RETURN(
    detail::rswitch<detail::auto_>(
      ints, i,
      std::forward<Func>(func),
      std::forward<Func>(func)
    )
  )
};
FALCON_SCOPED_INLINE_VARIABLE(rswitch_fn, rswitch)


namespace detail
{
  template<class T>
  struct make_value_with
  {
    T & x;

    template<class I>
    std::decay_t<T> operator()(I)
    FALCON_RETURN_NOEXCEPT(static_cast<T&&>(x))
  };
}

struct rswitch_or_fn
{
  /// \brief \p default_value = \p func(\c ic) if \p i equals \c ic
  template<class T, class Int, Int... ic, class I, class Func>
  constexpr std::decay_t<T> operator()(
    std::integer_sequence<Int, ic...> ints,
    I const & i, Func && func, T && default_value
  ) const
  {
    return detail::rswitch<T>(
      ints, i,
      std::forward<Func>(func),
      detail::make_value_with<T>{default_value}
    );
  }
};
FALCON_SCOPED_INLINE_VARIABLE(rswitch_or_fn, rswitch_or)


namespace detail
{
  template<class T, class Int, class I, class Func, class DefaultFunc>
  static constexpr T rswitch_impl(
    std::false_type, std::integer_sequence<Int>,
    I const & i, Func &&, DefaultFunc && default_func
  )
  {
    return std::forward<DefaultFunc>(default_func)(i);
  }

#define FALCON_LNAME_1(c) c##0
#define FALCON_LNAME_2(c) c##0, c##1
#define FALCON_LNAME_4(c) FALCON_LNAME_2(c##0), FALCON_LNAME_2(c##1)
#define FALCON_LNAME_8(c) FALCON_LNAME_4(c##0), FALCON_LNAME_4(c##1)
#define FALCON_LNAME_16(c) FALCON_LNAME_8(c##0), FALCON_LNAME_8(c##1)
#define FALCON_LNAME_32(c) FALCON_LNAME_16(c##0), FALCON_LNAME_16(c##1)
#define FALCON_LNAME_64(c) FALCON_LNAME_32(c##0), FALCON_LNAME_32(c##1)
#define FALCON_LNAME_128(c) FALCON_LNAME_64(c##0), FALCON_LNAME_64(c##1)
#define FALCON_LNAME_256(c) FALCON_LNAME_128(c##0), FALCON_LNAME_128(c##1)

#define FALCON_TCASE_1(c)                    \
  case c##0: return std::forward<Func>(func) \
    (std::integral_constant<Int, c##0>{});
#define FALCON_TCASE_2(c)                    \
  case c##0: return std::forward<Func>(func) \
    (std::integral_constant<Int, c##0>{});   \
  case c##1: return std::forward<Func>(func) \
    (std::integral_constant<Int, c##1>{});
#define FALCON_TCASE_4(c) FALCON_TCASE_2(c##0) FALCON_TCASE_2(c##1)
#define FALCON_TCASE_8(c) FALCON_TCASE_4(c##0) FALCON_TCASE_4(c##1)
#define FALCON_TCASE_16(c) FALCON_TCASE_8(c##0) FALCON_TCASE_8(c##1)
#define FALCON_TCASE_32(c) FALCON_TCASE_16(c##0) FALCON_TCASE_16(c##1)
#define FALCON_TCASE_64(c) FALCON_TCASE_32(c##0) FALCON_TCASE_32(c##1)
#define FALCON_TCASE_128(c) FALCON_TCASE_64(c##0) FALCON_TCASE_64(c##1)
#define FALCON_TCASE_256(c) FALCON_TCASE_128(c##0) FALCON_TCASE_128(c##1)

# define FALCON_RSWITCH_I(TN, LN, Cases)                    \
  template<                                                 \
    class T, class B, class Int, TN, Int... ints,           \
    class I, class Func, class DefaultFunc>                 \
  static constexpr T rswitch_impl(                          \
    B, std::integer_sequence<Int, LN, ints...>,             \
    I const & i, Func && func, DefaultFunc && default_func) \
  {                                                         \
    switch (i)                                              \
    {                                                       \
      Cases                                                 \
      default: return rswitch_impl<T>(                      \
        std::false_type{},                                  \
        std::integer_sequence<Int, ints...>{},              \
        i, std::forward<Func>(func),                        \
        std::forward<DefaultFunc>(default_func)             \
      );                                                    \
    }                                                       \
  }

#define FALCON_RSWITCH(X) FALCON_RSWITCH_I(\
  FALCON_LNAME_##X(Int i), FALCON_LNAME_##X(i), FALCON_TCASE_##X(i))

  FALCON_RSWITCH(1)
  FALCON_RSWITCH(2)
  FALCON_RSWITCH(4)
  FALCON_RSWITCH(8)
  FALCON_RSWITCH(16)
  FALCON_RSWITCH(32)
  FALCON_RSWITCH(64)
  FALCON_RSWITCH(128)
  FALCON_RSWITCH(256)

  template<
    class T, class Int, FALCON_LNAME_256(Int i),
    class I, class Func, class DefaultFunc>
  static constexpr T rswitch_impl(
    std::true_type,
    std::integer_sequence<Int, FALCON_LNAME_256(i)>,
    I const & i, Func && func, DefaultFunc &&)
  {
    switch (i)
    {
      FALCON_TCASE_256(i)
    }
  }

#if defined(__GNUC__) && !defined(__clang__)

// GCC: Nested switch is not optimized.
// Generate switch statement with all cases from 1 to 16.

# ifndef FALCON_PP_COMMA
#   define FALCON_PP_COMMA ,
# endif

  // 3
  FALCON_RSWITCH_I(
    FALCON_LNAME_2(Int i) FALCON_PP_COMMA FALCON_LNAME_1(Int ix),
    FALCON_LNAME_2(i) FALCON_PP_COMMA FALCON_LNAME_1(ix),
    FALCON_TCASE_2(i) FALCON_TCASE_1(ix)
  )
  // 5
  FALCON_RSWITCH_I(
    FALCON_LNAME_4(Int i) FALCON_PP_COMMA FALCON_LNAME_1(Int ix),
    FALCON_LNAME_4(i) FALCON_PP_COMMA FALCON_LNAME_1(ix),
    FALCON_TCASE_4(i) FALCON_TCASE_1(ix)
  )
  // 6
  FALCON_RSWITCH_I(
    FALCON_LNAME_4(Int i) FALCON_PP_COMMA FALCON_LNAME_2(Int ix),
    FALCON_LNAME_4(i) FALCON_PP_COMMA FALCON_LNAME_2(ix),
    FALCON_TCASE_4(i) FALCON_TCASE_2(ix)
  )
  // 7
  FALCON_RSWITCH_I(
    FALCON_LNAME_4(Int i) FALCON_PP_COMMA FALCON_LNAME_2(Int ix) FALCON_PP_COMMA
    FALCON_LNAME_1(Int iy),
    FALCON_LNAME_4(i) FALCON_PP_COMMA FALCON_LNAME_2(ix) FALCON_PP_COMMA
    FALCON_LNAME_1(iy),
    FALCON_TCASE_4(i) FALCON_TCASE_2(ix) FALCON_TCASE_1(iy)
  )
  // 9
  FALCON_RSWITCH_I(
    FALCON_LNAME_8(Int i) FALCON_PP_COMMA FALCON_LNAME_1(Int ix),
    FALCON_LNAME_8(i) FALCON_PP_COMMA FALCON_LNAME_1(ix),
    FALCON_TCASE_8(i) FALCON_TCASE_1(ix)
  )
  // 10
  FALCON_RSWITCH_I(
    FALCON_LNAME_8(Int i) FALCON_PP_COMMA FALCON_LNAME_2(Int ix),
    FALCON_LNAME_8(i) FALCON_PP_COMMA FALCON_LNAME_2(ix),
    FALCON_TCASE_8(i) FALCON_TCASE_2(ix)
  )
  // 11
  FALCON_RSWITCH_I(
    FALCON_LNAME_8(Int i) FALCON_PP_COMMA FALCON_LNAME_2(Int ix) FALCON_PP_COMMA
    FALCON_LNAME_1(Int iy),
    FALCON_LNAME_8(i) FALCON_PP_COMMA FALCON_LNAME_2(ix) FALCON_PP_COMMA
    FALCON_LNAME_1(iy),
    FALCON_TCASE_8(i) FALCON_TCASE_2(ix) FALCON_TCASE_1(iy)
  )
  // 12
  FALCON_RSWITCH_I(
    FALCON_LNAME_8(Int i) FALCON_PP_COMMA FALCON_LNAME_4(Int ix),
    FALCON_LNAME_8(i) FALCON_PP_COMMA FALCON_LNAME_4(ix),
    FALCON_TCASE_8(i) FALCON_TCASE_4(ix)
  )
  // 13
  FALCON_RSWITCH_I(
    FALCON_LNAME_8(Int i) FALCON_PP_COMMA FALCON_LNAME_4(Int ix) FALCON_PP_COMMA
    FALCON_LNAME_1(Int iy),
    FALCON_LNAME_8(i) FALCON_PP_COMMA FALCON_LNAME_4(ix) FALCON_PP_COMMA
    FALCON_LNAME_1(iy),
    FALCON_TCASE_8(i) FALCON_TCASE_4(ix) FALCON_TCASE_1(iy)
  )
  // 14
  FALCON_RSWITCH_I(
    FALCON_LNAME_8(Int i) FALCON_PP_COMMA FALCON_LNAME_4(Int ix) FALCON_PP_COMMA
    FALCON_LNAME_2(Int iy),
    FALCON_LNAME_8(i) FALCON_PP_COMMA FALCON_LNAME_4(ix) FALCON_PP_COMMA
    FALCON_LNAME_2(iy),
    FALCON_TCASE_8(i) FALCON_TCASE_4(ix) FALCON_TCASE_2(iy)
  )
  // 15
  FALCON_RSWITCH_I(
    FALCON_LNAME_8(Int i) FALCON_PP_COMMA FALCON_LNAME_4(Int ix) FALCON_PP_COMMA
    FALCON_LNAME_2(Int iy) FALCON_PP_COMMA FALCON_LNAME_1(Int iz),
    FALCON_LNAME_8(i) FALCON_PP_COMMA FALCON_LNAME_4(ix) FALCON_PP_COMMA
    FALCON_LNAME_2(iy) FALCON_PP_COMMA FALCON_LNAME_1(iz),
    FALCON_TCASE_8(i) FALCON_TCASE_4(ix) FALCON_TCASE_2(iy) FALCON_TCASE_1(iz)
  )

#endif

#undef FALCON_RSWITCH_I
#undef FALCON_RSWITCH
#undef FALCON_LNAME_1
#undef FALCON_LNAME_2
#undef FALCON_LNAME_4
#undef FALCON_LNAME_8
#undef FALCON_LNAME_16
#undef FALCON_LNAME_32
#undef FALCON_LNAME_64
#undef FALCON_LNAME_128
#undef FALCON_LNAME_256
#undef FALCON_TCASE_1
#undef FALCON_TCASE_2
#undef FALCON_TCASE_4
#undef FALCON_TCASE_8
#undef FALCON_TCASE_16
#undef FALCON_TCASE_32
#undef FALCON_TCASE_64
#undef FALCON_TCASE_128
#undef FALCON_TCASE_256

  template<class F>
  struct rswitch_fn_wrap
  {
    F & f;
    template<class... Ts>
    constexpr void operator()(Ts && ... args)
    noexcept(noexcept(static_cast<F&&>(f)(std::forward<Ts>(args)...)))
    {
      static_cast<F&&>(f)(std::forward<Ts>(args)...);
    }
  };

  template<class F>
  static constexpr auto rswitch_fn(std::true_type, F && f) noexcept
  {
    return rswitch_fn_wrap<F>{f};
  }

  template<class F>
  static constexpr F && rswitch_fn(std::false_type, F && f) noexcept
  {
    return std::forward<F>(f);
  }

  template<class T>
  std::integral_constant<bool, (
    (std::is_enum<T>::value || std::is_integral<T>::value)
    && sizeof(T) == 1
  )>
  rswitch_opti_i(T const &);


  template<class Int, Int... ic>
  class check_unique_int : std::integral_constant<Int, ic>... {};


  template<
    class T, class Int, Int... ic, class I, class Func, class DefaultFunc>
  constexpr rswitch_result_t<T, Func, DefaultFunc, I, Int, ic...>
  rswitch(
    std::integer_sequence<Int, ic...> ints,
    I const & i, Func && func, DefaultFunc && default_func
  )
  {
    check_unique_int<Int, ic...>{};

    using result_type = rswitch_result_t<T, Func, DefaultFunc, I, Int, ic...>;
    using is_void = typename std::is_same<void, result_type>::type;

    return rswitch_impl<result_type>(
      decltype(rswitch_opti_i(i)){}, ints, i,
      rswitch_fn(is_void{}, std::forward<Func>(func)),
      rswitch_fn(is_void{}, std::forward<DefaultFunc>(default_func))
    );
  }

  template<
    class T, class Int, Int... ic, class I, I i, class Func, class DefaultFunc>
  constexpr rswitch_result_t<T, Func, DefaultFunc, std::integral_constant<I, i>, Int, ic...>
  rswitch(
    std::integer_sequence<Int, ic...>,
    std::integral_constant<I, i> ii,
    Func && func, DefaultFunc && default_func
  )
  {
    check_unique_int<Int, ic...>{};

    using result_type = rswitch_result_t<
      T, Func, DefaultFunc, std::integral_constant<I, i>, Int, ic...>;
    using is_void = typename std::is_same<void, result_type>::type;

    return rswitch_fn(is_void{}, select(
      not_contains_int<I, Int, i, ic...>{},
      std::forward<DefaultFunc>(default_func),
      std::forward<Func>(func)
    ))(ii);
  }
}

} }
