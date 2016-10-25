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

#include <falcon/cexpr.hpp>

class true_ {} true_;
class false_ {} false_;

inline std::true_type cbool(class true_) { return {}; }
inline std::false_type cbool(class false_) { return {}; }

template<class> class t_ {};

int main()
{
  using namespace falcon::cexpr;


  std::true_type y;
  std::false_type n;
  std::integral_constant<int, 1> a;
  std::integral_constant<int, 2> b;

  cbool(y) = y;
  cbool(a) = y;
  cbool(b) = y;
  cbool(true_) = y;
  cbool(n) = n;
  cbool(std::integral_constant<int, 0>{}) = n;
  cbool(false_) = n;

  select(y, a, b) = a;
  select(true_, a, b) = a;
  select(n, a, b) = b;
  select(false_, a, b) = b;


  auto ra = [a]{ return a; };
  auto rb = [b]{ return b; };

  cif(y, ra) = a;
  cif(true_, ra) = a;
  cif(y, ra, rb) = a;
  cif(true_, ra, rb) = a;
  t_<decltype(cif(n, ra))>{} = t_<void>{};
  t_<decltype(cif(false_, ra))>{} = t_<void>{};
  cif(n, ra, rb) = b;
  cif(false_, ra, rb) = b;


  std::integer_sequence<int, 0, 1, 2> ints;
  auto is = [](auto v) { return [v](auto i){ if (i != v) throw 1; return int(i); }; };
  auto xto4 = [](auto) { return 4; };

  is(0)(rswitch(5, ints, 0, is(0)));
  is(1)(rswitch(5, ints, 1, is(1)));
  is(2)(rswitch(5, ints, 2, is(2)));
  is(3)(rswitch(5, ints, 3, is(3)));

  is(0)(rswitch(5, ints, 0, is(0), xto4));
  is(1)(rswitch(5, ints, 1, is(1), xto4));
  is(2)(rswitch(5, ints, 2, is(2), xto4));
  is(4)(rswitch(5, ints, 3, is(3), xto4));

  is(0)(rswitch(5, ints, 0, is(0), nodefault));
  is(1)(rswitch(5, ints, 1, is(1), nodefault));
  is(2)(rswitch(5, ints, 2, is(2), nodefault));
  is(5)(rswitch(5, ints, 3, is(3), nodefault));

  is(0)(rswitch(ints, 0, is(0)));
  is(1)(rswitch(ints, 1, is(1)));
  is(2)(rswitch(ints, 2, is(2)));
  is(3)(rswitch(ints, 3, is(3)));

  is(0)(rswitch(ints, 0, is(0), xto4));
  is(1)(rswitch(ints, 1, is(1), xto4));
  is(2)(rswitch(ints, 2, is(2), xto4));
  is(4)(rswitch(ints, 3, is(3), xto4));

  is(0)(rswitch(ints, 0, is(0), nodefault));
  is(1)(rswitch(ints, 1, is(1), nodefault));
  is(2)(rswitch(ints, 2, is(2), nodefault));
  is(0)(rswitch(ints, 3, is(3), nodefault));
}
