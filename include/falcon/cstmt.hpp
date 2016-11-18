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

namespace detail
{
  template<class True, class False>
  constexpr True select_(std::true_type, True && yes, False &&)
  { return std::forward<True>(yes); }

  template<class True, class False>
  constexpr False select_(std::false_type, True &&, False && no)
  { return std::forward<False>(no); }
}

/// \return \p yes if \p cond is a \c true constant expression, otherwise \p no
template<class B, class True, class False>
constexpr auto select(B cond, True && yes, False && no)
FALCON_DECLTYPE_AUTO_RETURN_NOEXCEPT(
  detail::select_(cbool(cond), std::forward<True>(yes), std::forward<False>(no))
)

namespace detail
{
  template<class True, class False>
  constexpr auto cif_(std::true_type, True && yes, False &&)
  FALCON_DECLTYPE_AUTO_RETURN_NOEXCEPT(
    std::forward<True>(yes)()
  )

  template<class True, class False>
  constexpr auto cif_(std::false_type, True &&, False && no)
  FALCON_DECLTYPE_AUTO_RETURN_NOEXCEPT(
    std::forward<False>(no)()
  )

  template<class True>
  constexpr
  auto cif_(std::true_type, True && yes)
  FALCON_DECLTYPE_AUTO_RETURN_NOEXCEPT(
    std::forward<True>(yes)()
  )

  template<class True>
  constexpr
  void cif_(std::false_type, True &&)
  {}
}

/// \return \p yes() if \p cond is a \c true constant expression, otherwise \p no()
template<class B, class True, class False>
constexpr auto cif(B cond, True && yes, False && no)
FALCON_DECLTYPE_AUTO_RETURN_NOEXCEPT(
  detail::cif_(cbool(cond), std::forward<True>(yes), std::forward<False>(no))
)

/// \return \p yes() if \p cond is a \c true constant expression, otherwise \c void
template<class B, class True>
constexpr
auto cif(B cond, True && yes)
FALCON_DECLTYPE_AUTO_RETURN_NOEXCEPT(
  detail::cif_(cbool(cond), std::forward<True>(yes))
)


struct nodefault_t {
  constexpr nodefault_t() noexcept {}
};
FALCON_SCOPED_INLINE_VARIABLE(nodefault_t, nodefault)

namespace detail
{
  template<class Int, Int... ic>
  class check_unique_int : std::integral_constant<Int, ic>... {};
}

/// \brief \p func(\c ic) if \p i equals \c ic
template<class Int, Int... ic, class I, class Func>
constexpr
void cswitch(std::integer_sequence<Int, ic...>, I i, Func && func, nodefault_t)
{
  detail::check_unique_int<Int, ic...>{};

  (void)std::initializer_list<int>{(void(
    ic == i
    ? void(std::forward<Func>(func)(std::integral_constant<Int, ic>{}))
    : void()
  ), 1)...};
}

/// \brief \p func(\c ic) if \p i equals \c ic, otherwise \p default_func(\p i)
template<class Int, Int... ic, class I, class Func, class Default>
constexpr
void cswitch(std::integer_sequence<Int, ic...>, I i, Func && func, Default && default_func)
{
  detail::check_unique_int<Int, ic...>{};

  bool has_int = false;
  (void)std::initializer_list<int>{(void(
    ic == i
    ? void((std::forward<Func>(func)(std::integral_constant<Int, ic>{}), void(has_int = true)))
    : void()
  ), 1)...};
  if (!has_int) {
    std::forward<Default>(default_func)(i);
  }
}

/// \brief shortcut for \c cswitch(\p ints, \p i, \p func, \p func)
template<class Ints, class I, class Func>
constexpr
void cswitch(Ints ints, I i, Func && func)
{ cswitch(ints, i, std::forward<Func>(func), std::forward<Func>(func)); }


/// \brief \p default_value = \p func(\c ic) if \p i equals \c ic
/// \return \p default_value
template<class T, class Int, Int... ic, class I, class Func>
T rswitch_or(std::integer_sequence<Int, ic...>, I i, Func && func, T default_value)
{
  detail::check_unique_int<Int, ic...>{};

  (void)std::initializer_list<int>{(void(
    ic == i
    ? void(default_value = std::forward<Func>(func)(std::integral_constant<Int, ic>{}))
    : void()
  ), 1)...};

  return default_value;
}

namespace detail
{
  template<class R, class Int, class I, class Func, class Default>
  constexpr
  R rswitch_(std::integer_sequence<Int>, I i, Func &&, Default && default_func)
  {
    return std::forward<Default>(default_func)(i);
  }

  template<class R, class Int, Int ic, Int... ints, class I, class Func, class Default>
  constexpr
  R rswitch_(std::integer_sequence<Int, ic, ints...>, I i, Func && func, Default && default_func)
  {
    return ic == i
      ? std::forward<Func>(func)(ic)
      : rswitch_<R>(
        std::integer_sequence<Int, ints...>{},
        i,
        std::forward<Func>(func),
        std::forward<Default>(default_func)
      );
  }

  template<class R, class Int, class I, class Func>
  constexpr
  R rswitch_(std::integer_sequence<Int>, I, Func &&)
  {
    return R{};
  }

  template<class R, class Int, Int ic, Int... ints, class I, class Func>
  constexpr
  R rswitch_(std::integer_sequence<Int, ic, ints...>, I i, Func && func)
  {
    return ic == i
      ? std::forward<Func>(func)(ic)
      : rswitch_<R>(
        std::integer_sequence<Int, ints...>{},
        i,
        std::forward<Func>(func)
      );
  }
}


/// \return \p func(\p ic) if \p i equals \c ic, otherwise \p default_func(\p i)
template<class Int, Int... ic, class I, class Func, class Default>
constexpr
auto rswitch(std::integer_sequence<Int, ic...> ints, I i, Func && func, Default && default_func)
-> std::common_type_t<
  decltype(std::forward<Default>(default_func)(i)),
  decltype(std::forward<Func>(func)(std::integral_constant<Int, ic>{}))...
>
{
  detail::check_unique_int<Int, ic...>{};

  return detail::rswitch_<std::common_type_t<
    decltype(std::forward<Default>(default_func)(i)),
    decltype(std::forward<Func>(func)(std::integral_constant<Int, ic>{}))...
  >>(ints, i, std::forward<Func>(func), std::forward<Default>(default_func));
}

/// \return \p func(\p ic) if \p i equals \c ic, otherwise \a result_type{}
template<class Int, Int... ic, class I, class Func>
constexpr
auto rswitch(std::integer_sequence<Int, ic...> ints, I i, Func && func, nodefault_t)
-> std::common_type_t<
  decltype(std::forward<Func>(func)(std::integral_constant<Int, ic>{}))...
>
{
  detail::check_unique_int<Int, ic...>{};

  return detail::rswitch_<std::common_type_t<
    decltype(std::forward<Func>(func)(std::integral_constant<Int, ic>{}))...
  >>(ints, i, std::forward<Func>(func));
}

/// \brief shortcut for \c rswitch(\c ints, \p i, \p func, \p func)
template<class Int, Int... ic, class I, class Func>
constexpr
auto rswitch(std::integer_sequence<Int, ic...> ints, I i, Func && func)
-> std::common_type_t<
  decltype(std::forward<Func>(func)(i)),
  decltype(std::forward<Func>(func)(std::integral_constant<Int, ic>{}))...
>
{
  detail::check_unique_int<Int, ic...>{};

  return detail::rswitch_<std::common_type_t<
    decltype(std::forward<Func>(func)(i)),
    decltype(std::forward<Func>(func)(std::integral_constant<Int, ic>{}))...
  >>(ints, i, std::forward<Func>(func), std::forward<Func>(func));
}

} }
