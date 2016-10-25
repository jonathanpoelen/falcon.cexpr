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

#include <initializer_list>
#include <type_traits>
#include <utility>

namespace falcon { namespace cexpr {

template<class T>
constexpr
std::integral_constant<bool, bool(T::value)>
cbool(T const &) noexcept
{ return {}; }

// TODO falcon.cxx project
#ifdef IN_IDE_PARSER

# define FALCON_CEXPR_DECLTYPE_AUTO_RETURN(a) \
  -> decltype(a)                              \
  { return (a); }                             \

# define FALCON_CEXPR_DECLTYPE_AUTO_RETURN_NOEXCEPT(a) \
  noexcept(noexcept(decltype(a)(a))) ->                \
  decltype(a)                                          \
  { return (a); }                                      \

#else

# define FALCON_CEXPR_DECLTYPE_AUTO_RETURN(...) \
  -> decltype(__VA_ARGS__)                      \
  { return (__VA_ARGS__); }                     \

# define FALCON_CEXPR_DECLTYPE_AUTO_RETURN_NOEXCEPT(...)    \
  noexcept(noexcept(decltype(__VA_ARGS__)(__VA_ARGS__))) -> \
  decltype(__VA_ARGS__)                                     \
  { return (__VA_ARGS__); }                                 \

#endif

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
FALCON_CEXPR_DECLTYPE_AUTO_RETURN(
  detail::select_(cbool(cond), std::forward<True>(yes), std::forward<False>(no))
)

namespace detail
{
  template<class True, class False>
  constexpr auto cif_(std::true_type, True && yes, False &&)
  FALCON_CEXPR_DECLTYPE_AUTO_RETURN_NOEXCEPT(
    std::forward<True>(yes)()
  )

  template<class True, class False>
  constexpr auto cif_(std::false_type, True &&, False && no)
  FALCON_CEXPR_DECLTYPE_AUTO_RETURN_NOEXCEPT(
    std::forward<False>(no)()
  )

  template<class True>
  constexpr
  auto cif_(std::true_type, True && yes)
  FALCON_CEXPR_DECLTYPE_AUTO_RETURN_NOEXCEPT(
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
FALCON_CEXPR_DECLTYPE_AUTO_RETURN_NOEXCEPT(
  detail::cif_(cbool(cond), std::forward<True>(yes), std::forward<False>(no))
)

/// \return \p yes() if \p cond is a \c true constant expression, otherwise \c void
template<class B, class True>
constexpr
auto cif(B cond, True && yes)
FALCON_CEXPR_DECLTYPE_AUTO_RETURN_NOEXCEPT(
  detail::cif_(cbool(cond), std::forward<True>(yes))
)


struct nodefault_t {
  constexpr nodefault_t() noexcept {}
};
constexpr nodefault_t nodefault;

namespace detail
{
  template<class Int, Int... ints>
  class check_unique_int : std::integral_constant<Int, ints>... {};
}

/// \brief run \p func(\c ints) if \p i equal \c ints
template<class Int, Int... ints, class I, class Func>
constexpr
void cswitch(std::integer_sequence<Int, ints...>, I i, Func && func, nodefault_t)
{
  detail::check_unique_int<Int, ints...>{};

  (void)std::initializer_list<int>{(void(
    i == ints
    ? void(std::forward<Func>(func)(std::integral_constant<Int, ints>{}))
    : void()
  ), 1)...};
}

/// \brief run \p func(\c ints) if \p i equal \c ints, otherwise \p default_func(\p i)
template<class Int, Int... ints, class I, class Func, class Default>
constexpr
void cswitch(std::integer_sequence<Int, ints...>, I i, Func && func, Default && default_func)
{
  detail::check_unique_int<Int, ints...>{};
  [&]{ // msvc...
    bool has_int = false;
    (void)std::initializer_list<int>{(void(
      i == ints
      ? void((std::forward<Func>(func)(std::integral_constant<Int, ints>{}), void(has_int = true)))
      : void()
    ), 1)...};
    if (!has_int) {
      std::forward<Default>(default_func)(i);
    }
  }();
}

/// \brief shortcut for \c cswitch(\p ints, \p i, \p func, \p func)
template<class Ints, class I, class Func>
constexpr
void cswitch(Ints ints, I i, Func && func)
{ cswitch(ints, i, std::forward<Func>(func), std::forward<Func>(func)); }


/// \brief run \p default_value = \p func(\c ints) if \p i equal \c ints
/// \return \p default_value
template<class T, class Int, Int... ints, class I, class Func>
T rswitch(T default_value, std::integer_sequence<Int, ints...> intseq, I i, Func && func, nodefault_t)
{
  cswitch(
    intseq, i,
    [&](auto ic) { default_value = std::forward<Func>(func)(ic); },
    nodefault_t{}
  );
  return default_value;
}

/// \brief run \p default_value = \p func(\c ints) if \p i equal \c ints, otherwise \p default_value = \p default_func(\p i)
/// \return \p default_value
template<class T, class Int, Int... ints, class I, class Func, class Default>
constexpr
T rswitch(T default_value, std::integer_sequence<Int, ints...> intseq, I i, Func && func, Default && default_func)
{
  cswitch(
    intseq, i,
    [&](auto ic) { default_value = std::forward<Func>(func)(ic); },
    [&](auto i_) { default_value = std::forward<Default>(default_func)(i_); }
  );
  return default_value;
}

/// \brief shortcut for \c rswitch(\p default_value, \p intseq, \p i, \p func, \p func)
template<class T, class Int, Int... ints, class I, class Func>
constexpr
T rswitch(T default_value, std::integer_sequence<Int, ints...> intseq, I i, Func && func)
{ return rswitch(std::forward<T>(default_value), intseq, i, std::forward<Func>(func), std::forward<Func>(func)); }


/// \return \p func(\p ints) if \p i equal \c ints, otherwise \p default_func(\p i)
template<class Int, Int... ints, class I, class Func, class Default>
constexpr
auto rswitch(std::integer_sequence<Int, ints...>, I i, Func && func, Default && default_func)
-> std::common_type_t<
  decltype(std::forward<Default>(default_func)(i)),
  decltype(std::forward<Func>(func)(std::integral_constant<Int, ints>{}))...
>
{
  return [](auto recursive, auto... iic) {
    return recursive(recursive, iic...);
  }([&](auto recf, auto ic, auto ... ics){
    return i == ic
      ? std::forward<Func>(func)(ic)
      : select(
          std::integral_constant<bool, bool(sizeof...(ics))>{},
          recf,
          [&](auto){ return std::forward<Default>(default_func)(i); }
      )(recf, ics...);
  }, std::integral_constant<Int, ints>{}...);
}

/// \return \p func(\p ints) if \p i equal \c ints, otherwise \a result_type{}
template<class Int, Int... ints, class I, class Func>
constexpr
auto rswitch(std::integer_sequence<Int, ints...> intseq, I i, Func && func, nodefault_t)
-> std::common_type_t<
  decltype(std::forward<Func>(func)(std::integral_constant<Int, ints>{}))...
>
{
  return rswitch(intseq, i, std::forward<Func>(func), [](I const &){
    return std::common_type_t<
      decltype(std::forward<Func>(func)(std::integral_constant<Int, ints>{}))...
    >{};
  });
}

/// \brief shortcut for \c rswitch(\c intseq, \p i, \p func, \p func)
template<class Int, Int... ints, class I, class Func>
constexpr
auto rswitch(std::integer_sequence<Int, ints...> intseq, I i, Func && func)
-> std::common_type_t<
  decltype(std::forward<Func>(func)(std::integral_constant<Int, ints>{}))...
>
{ return rswitch(intseq, i, std::forward<Func>(func), std::forward<Func>(func)); }

} }
