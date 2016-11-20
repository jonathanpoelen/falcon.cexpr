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

template<class T>
constexpr
cbool_trait_t<T>
cbool(T const &) noexcept
{ return {}; }

constexpr
std::true_type
cbool(std::true_type) noexcept
{ return {}; }

constexpr
std::false_type
cbool(std::false_type) noexcept
{ return {}; }


class select_fn
{
  template<class True, class False>
  static constexpr
  True
  _(std::true_type, True && yes, False &&)
  FALCON_RETURN_NOEXCEPT(
    std::forward<True>(yes)
  )

  template<class True, class False>
  static constexpr
  False
  _(std::false_type, True &&, False && no)
  FALCON_RETURN_NOEXCEPT(
    std::forward<False>(no)
  )

public:
  /// \return \p yes if \p cond is a \c true constant expression, otherwise \p no
  template<class B, class True, class False>
  constexpr
  auto
  operator()(B cond, True && yes, False && no) const
  FALCON_DECLTYPE_AUTO_RETURN_NOEXCEPT(
    _(cbool(cond), std::forward<True>(yes), std::forward<False>(no))
  )
};
FALCON_SCOPED_INLINE_VARIABLE(select_fn, select)


class cif_fn
{
  template<class True, class False>
  static constexpr
  auto
  _(std::true_type, True && yes, False &&)
  FALCON_DECLTYPE_AUTO_RETURN_NOEXCEPT(
    std::forward<True>(yes)()
  )

  template<class True, class False>
  static constexpr
  auto
  _(std::false_type, True &&, False && no)
  FALCON_DECLTYPE_AUTO_RETURN_NOEXCEPT(
    std::forward<False>(no)()
  )

  template<class True>
  static constexpr
  auto
  _(std::true_type, True && yes)
  FALCON_DECLTYPE_AUTO_RETURN_NOEXCEPT(
    std::forward<True>(yes)()
  )

  template<class True>
  static constexpr
  void
  _(std::false_type, True &&)
  {}

public:
  /// \return \p yes() if \p cond is a \c true constant expression, otherwise \p no()
  template<class B, class True, class False>
  constexpr
  auto
  operator()(B cond, True && yes, False && no) const
  FALCON_DECLTYPE_AUTO_RETURN_NOEXCEPT(
    _(cbool(cond), std::forward<True>(yes), std::forward<False>(no))
  )

  /// \return \p yes() if \p cond is a \c true constant expression, otherwise \c void
  template<class B, class True>
  constexpr
  auto
  operator()(B cond, True && yes) const
  FALCON_DECLTYPE_AUTO_RETURN_NOEXCEPT(
    _(cbool(cond), std::forward<True>(yes))
  )
};
FALCON_SCOPED_INLINE_VARIABLE(cif_fn, cif)


struct nodefault_t {
  constexpr nodefault_t() noexcept {}
};
FALCON_SCOPED_INLINE_VARIABLE(nodefault_t, nodefault)


namespace detail
{
  template<class Int, Int... ic>
  class check_unique_int : std::integral_constant<Int, ic>... {};

  struct noop
  { template<class T> constexpr void operator()(T) const noexcept { } };

  template<class I, class Ints, I i, Ints... ic>
  using not_contains_int = typename std::is_same<
    std::integer_sequence<bool, (ic == i)...>,
    std::integer_sequence<bool, (void(ic), false)...>
  >::type;
}


struct cswitch_fn
{
  /// \brief \p func(\c ic) if \p i equals \c ic
  template<class Int, Int... ic, class I, class Func>
  constexpr
  void
  operator()(
    std::integer_sequence<Int, ic...>,
    I i, Func && func, nodefault_t
  ) const
  {
    detail::check_unique_int<Int, ic...>{};

    (void)std::initializer_list<int>{(void(
      ic == i
      ? void(std::forward<Func>(func)(std::integral_constant<Int, ic>{}))
      : void()
    ), 1)...};
  }

  /// \brief \p func(\c ic) if \p i equals \c ic, otherwise \p default_func(\p i)
  template<class Int, Int... ic, class I, class Func, class DefaultFunc>
  constexpr
  void
  operator()(
    std::integer_sequence<Int, ic...>,
    I i, Func && func, DefaultFunc && default_func
  ) const
  {
    detail::check_unique_int<Int, ic...>{};

#if defined(_MSC_VER) && _MSC_VER < 1910
    [&]{
#endif
    bool has_int = false;
    (void)std::initializer_list<int>{(void(
      ic == i
      ? void((std::forward<Func>(func)(std::integral_constant<Int, ic>{}), void(has_int = true)))
      : void()
    ), 1)...};

    if (!has_int) {
      std::forward<DefaultFunc>(default_func)(i);
    }
#if defined(_MSC_VER) && _MSC_VER < 1910
    }();
#endif
  }

  /// \brief shortcut for \c cswitch(\p ints, \p i, \p func, \p func)
  template<class Ints, class I, class Func>
  constexpr
  void
  operator()(Ints ints, I i, Func && func) const
  { operator()(ints, i, std::forward<Func>(func), std::forward<Func>(func)); }


  /// \brief \p func(\c ic) if \p i equals \c ic, otherwise \p default_func(\p i)
  template<class Int, Int... ic, class I, I i, class Func, class DefaultFunc>
  constexpr
  void
  operator()(
    std::integer_sequence<Int, ic...>,
    std::integral_constant<I, i> ii,
    Func && func, DefaultFunc && default_func
  ) const
  {
    detail::check_unique_int<Int, ic...>{};

    select(
      detail::not_contains_int<I, Int, i, ic...>{},
      std::forward<DefaultFunc>(default_func),
      std::forward<Func>(func)
    )(ii);
  }

  /// \brief \p func(\c ic) if \p i equals \c ic
  template<class Int, Int... ic, class I, I i, class Func>
  constexpr
  void
  operator()(
    std::integer_sequence<Int, ic...> ints,
    std::integral_constant<I, i> ii,
    Func && func, nodefault_t
  ) const
  { operator()(ints, ii, std::forward<Func>(func), detail::noop{}); }
};
FALCON_SCOPED_INLINE_VARIABLE(cswitch_fn, cswitch)


struct rswitch_fn
{
  /// \return \p func(\p ic) if \p i equals \c ic, otherwise \p default_func(\p i)
  template<class Int, Int... ic, class I, class Func, class DefaultFunc>
  constexpr
  auto
  operator()(
    std::integer_sequence<Int, ic...> ints,
    I i, Func && func, DefaultFunc && default_func
  ) const
  -> std::common_type_t<
    decltype(std::forward<DefaultFunc>(default_func)(i)),
    decltype(std::forward<Func>(func)(std::integral_constant<Int, ic>{}))...
  >
  {
    detail::check_unique_int<Int, ic...>{};

    return _<std::common_type_t<
      decltype(std::forward<DefaultFunc>(default_func)(i)),
      decltype(std::forward<Func>(func)(std::integral_constant<Int, ic>{}))...
    >>(ints, i, std::forward<Func>(func), std::forward<DefaultFunc>(default_func));
  }

  /// \return \p func(\p ic) if \p i equals \c ic, otherwise \a result_type{}
  template<class Int, Int... ic, class I, class Func>
  constexpr
  auto
  operator()(
    std::integer_sequence<Int, ic...> ints,
    I i, Func && func, nodefault_t
  ) const
  -> std::common_type_t<
    decltype(std::forward<Func>(func)(std::integral_constant<Int, ic>{}))...
  >
  {
    detail::check_unique_int<Int, ic...>{};

    return _<std::common_type_t<
      decltype(std::forward<Func>(func)(std::integral_constant<Int, ic>{}))...
    >>(ints, i, std::forward<Func>(func));
  }

  /// \brief shortcut for \c rswitch(\c ints, \p i, \p func, \p func)
  template<class Int, Int... ic, class I, class Func>
  constexpr
  auto
  operator()(
    std::integer_sequence<Int, ic...> ints,
    I i, Func && func
  ) const
  -> std::common_type_t<
    decltype(std::forward<Func>(func)(i)),
    decltype(std::forward<Func>(func)(std::integral_constant<Int, ic>{}))...
  >
  {
    detail::check_unique_int<Int, ic...>{};

    return _<std::common_type_t<
      decltype(std::forward<Func>(func)(i)),
      decltype(std::forward<Func>(func)(std::integral_constant<Int, ic>{}))...
    >>(ints, i, std::forward<Func>(func), std::forward<Func>(func));
  }


  /// \return \p func(\p ic) if \p i equals \c ic, otherwise \p default_func(\p i)
  template<class Int, Int... ic, class I, I i, class Func, class DefaultFunc>
  constexpr
  auto
  operator()(
    std::integer_sequence<Int, ic...>,
    std::integral_constant<I, i> ii,
    Func && func, DefaultFunc && default_func
  ) const
  FALCON_DECLTYPE_AUTO_RETURN(
    void(detail::check_unique_int<Int, ic...>{}),
    select(
      detail::not_contains_int<I, Int, i, ic...>{},
      std::forward<DefaultFunc>(default_func),
      std::forward<Func>(func)
    )(ii)
  )

private:
  template<class Func, class Int, Int... ic>
  struct make_common_type_fn;

public:
  /// \return \p func(\p ic) if \p i equals \c ic, otherwise \a result_type{}
  template<class Int, Int... ic, class I, I i, class Func>
  constexpr
  auto
  operator()(
    std::integer_sequence<Int, ic...>,
    std::integral_constant<I, i> ii,
    Func && func, nodefault_t
  ) const
  FALCON_DECLTYPE_AUTO_RETURN(
    void(detail::check_unique_int<Int, ic...>{}),
    select(
      detail::not_contains_int<I, Int, i, ic...>{},
      make_common_type_fn<Func, Int, ic...>{},
      std::forward<Func>(func)
    )(ii)
  )

  /// \brief call \p func(\p i)
  template<class Int, Int... ic, class I, I i, class Func>
  constexpr
  auto
  operator()(
    std::integer_sequence<Int, ic...>,
    std::integral_constant<I, i> ii,
    Func && func
  ) const
  -> decltype(std::forward<Func>(func)(ii))
  {
    detail::check_unique_int<Int, ic...>{};

    return std::forward<Func>(func)(ii);
  }

private:
#if defined(__GNUC__) && !defined(__clang__)
  template<class Func, class Int, Int... ic>
  struct make_common_type_fn
  {
    template<class I>
    constexpr
    auto
    operator()(I) const
    FALCON_DECLTYPE_AUTO_RETURN_NOEXCEPT(
      std::common_type_t<
        decltype(std::declval<Func>()(std::integral_constant<Int, ic>{}))...
      >{}
    )
  };
#else
  template<class, class Func, class Int, Int... ic>
  struct result_common_type
  {
    using result_type = std::common_type_t<
      decltype(std::declval<Func>()(std::integral_constant<Int, ic>{}))...
    >;
  };

  template<class Func, class Int, Int... ic>
  struct make_common_type_fn
  {
    template<class I>
    constexpr
    auto
    operator()(I) const
    FALCON_DECLTYPE_AUTO_RETURN_NOEXCEPT(
      typename result_common_type<I, Func, Int, ic...>::type{}
    )
  };
#endif

  template<class R, class Int, class I, class Func, class DefaultFunc>
  static constexpr
  R
  _(std::integer_sequence<Int>, I i, Func &&, DefaultFunc && default_func)
  {
    return std::forward<DefaultFunc>(default_func)(i);
  }

  template<class R, class Int, Int ic, Int... ints, class I, class Func, class DefaultFunc>
  static constexpr
  R
  _(std::integer_sequence<Int, ic, ints...>, I i, Func && func, DefaultFunc && default_func)
  {
    return ic == i
      ? std::forward<Func>(func)(ic)
      : _<R>(
        std::integer_sequence<Int, ints...>{},
        i,
        std::forward<Func>(func),
        std::forward<DefaultFunc>(default_func)
      );
  }

  template<class R, class Int, class I, class Func>
  static constexpr
  R
  _(std::integer_sequence<Int>, I, Func &&)
  {
    return R{};
  }

  template<class R, class Int, Int ic, Int... ints, class I, class Func>
  static constexpr
  R
  _(std::integer_sequence<Int, ic, ints...>, I i, Func && func)
  {
    return ic == i
      ? std::forward<Func>(func)(ic)
      : _<R>(
        std::integer_sequence<Int, ints...>{},
        i,
        std::forward<Func>(func)
      );
  }

  friend class rswitch_or_fn;
};
FALCON_SCOPED_INLINE_VARIABLE(rswitch_fn, rswitch)


class rswitch_or_fn
{
  template<class T>
  struct default_value_fn;

public:
  /// \brief \p default_value = \p func(\c ic) if \p i equals \c ic
  template<class T, class Int, Int... ic, class I, class Func>
  constexpr
  std::decay_t<T>
  operator()(
    std::integer_sequence<Int, ic...> ints,
    I i, Func && func, T && default_value
  ) const
  {
    detail::check_unique_int<Int, ic...>{};

    return rswitch_fn::_<T>(
      ints, i,
      std::forward<Func>(func),
      default_value_fn<T>{default_value}
    );
  }

  /// \brief \p default_value = \p func(\c ic) if \p i equals \c ic
  template<class T, class Int, Int... ic, class I, I i, class Func>
  constexpr
  std::decay_t<T>
  operator()(
    std::integer_sequence<Int, ic...>,
    std::integral_constant<I, i> ii,
    Func && func, T && default_value
  ) const
  {
    detail::check_unique_int<Int, ic...>{};

    return select(
      detail::not_contains_int<I, Int, i, ic...>{},
      default_value_fn<T>{default_value},
      std::forward<Func>(func)
    )(ii);
  }

private:
  template<class T>
  struct default_value_fn
  {
    T & x;

    template<class I>
    std::decay_t<T>
    operator()(I)
    { return std::forward<T>(x); }
  };
};
FALCON_SCOPED_INLINE_VARIABLE(rswitch_or_fn, rswitch_or)

} }
