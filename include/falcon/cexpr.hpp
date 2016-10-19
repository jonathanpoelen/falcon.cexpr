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

template<class True, class False>
constexpr
True select(std::true_type, True && yes, False &&)
{ return std::forward<True>(yes); }

template<class True, class False>
constexpr
False select(std::false_type, True &&, False && no)
{ return std::forward<False>(no); }


template<class True, class False>
constexpr
auto cif(std::true_type, True && yes, False &&)
-> decltype(yes())
{ return yes(); }

template<class True, class False>
constexpr
auto cif(std::false_type, True &&, False && no)
-> decltype(no())
{ return no(); }

template<class True>
constexpr
auto cif(std::true_type, True && yes)
-> decltype(yes())
{ return yes(); }

template<class True>
constexpr
void cif(std::false_type, True &&)
{}


class nodefault {};

template<class Int, Int... ints, class I, class Func>
constexpr
void cswitch(std::integer_sequence<Int, ints...>, I i, Func && func, nodefault)
{
  class has_unique_int : std::integral_constant<Int, ints>... {};

  (void)std::initializer_list<int>{(void(
    i == ints
    ? void(func(std::integral_constant<Int, ints>{}))
    : void()
  ), 1)...};
}

template<class Int, Int... ints, class I, class Func, class Default>
constexpr
void cswitch(std::integer_sequence<Int, ints...>, I i, Func && func, Default && default_func)
{
  class has_unique_int : std::integral_constant<Int, ints>... {};

  bool has_ints = false;
  (void)std::initializer_list<int>{(void(
    i == ints
    ? void((func(std::integral_constant<Int, ints>{}), void(has_ints = true)))
    : void()
  ), 1)...};
  if (!has_ints) {
    default_func(i);
  }
}

/// \brief shortcut for \c cswitch(\c ints, \c i, \c func, \c func)
template<class Ints, class I, class Func>
constexpr
void cswitch(Ints ints, I i, Func && func)
{ cswitch(ints, i, func, func); }


template<class T, class Ints, class I, class Func>
T rswitch(T default_value, Ints ints, I i, Func && func, nodefault)
{
  cswitch(
    ints, i,
    [&](auto ic) { default_value = std::forward<Func>(func)(ic); },
    nodefault{}
  );
  return default_value;
}

template<class T, class Ints, class I, class Func, class Default>
constexpr
T rswitch(T default_value, Ints ints, I i, Func && func, Default && default_func)
{
  cswitch(
    ints, i,
    [&](auto ic) { default_value = std::forward<Func>(func)(ic); },
    [&](auto i_) { default_value = std::forward<Default>(default_func)(i_); }
  );
  return default_value;
}

/// \brief shortcut for \c rswitch(\c default_value, \c ints, \c i, \c func, \c func)
template<class T, class Ints, class I, class Func>
constexpr
T rswitch(T default_value, Ints ints, I i, Func && func)
{ return rswitch(default_value, ints, i, std::forward<Func>(func), std::forward<Func>(func)); }

} }
