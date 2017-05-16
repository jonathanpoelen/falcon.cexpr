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

#include <falcon/cstmt.hpp>

class Yes {} yes;
class No {} no;

namespace falcon { namespace cstmt {
  template<> struct cbool_trait< ::Yes> : std::true_type {};
  template<> struct cbool_trait< ::No> : std::false_type {};
} }

class true_ {} true_;
class false_ {} false_;

inline std::true_type cbool(class true_) { return {}; }
inline std::false_type cbool(class false_) { return {}; }

template<class> class t_ {};

template<class T> struct inherit : T {};

int main()
{
  using namespace falcon::cstmt;


  std::true_type y;
  std::false_type n;
  std::integral_constant<int, 1> a;
  std::integral_constant<int, 2> b;

  cbool(y) = y;
  cbool(yes) = y;
  cbool(true_) = y;
  cbool(inherit<std::true_type>{}) = y;
  cbool(n) = n;
  cbool(no) = n;
  cbool(false_) = n;
  cbool(inherit<std::false_type>{}) = n;

  select(y, a, b) = a;
  select(yes, a, b) = a;
  select(true_, a, b) = a;
  select(n, a, b) = b;
  select(no, a, b) = b;
  select(false_, a, b) = b;


  auto ra = [a]{ return a; };
  auto rb = [b]{ return b; };

  cif(y, ra) = a;
  cif(yes, ra) = a;
  cif(true_, ra) = a;
  cif(y, ra, rb) = a;
  cif(yes, ra, rb) = a;
  cif(true_, ra, rb) = a;
  t_<decltype(cif(n, ra))>{} = t_<void>{};
  t_<decltype(cif(no, ra))>{} = t_<void>{};
  t_<decltype(cif(false_, ra))>{} = t_<void>{};
  cif(n, ra, rb) = b;
  cif(false_, ra, rb) = b;


  std::integer_sequence<int, 0, 1, 2> ints;
  auto is = [](auto v) { return [v](auto i){ if (i != v) throw 1; return int(i); }; };
  auto xto4 = [](auto) { return 4; };
  auto throw_ = [](auto){ throw 2; };

  cswitch(ints, char(0), is(0), throw_);

  cswitch(ints, 0, is(0), throw_);
  cswitch(ints, 1, is(1), throw_);
  cswitch(ints, 2, is(2), throw_);
  cswitch(ints, 3, throw_, is(3));

  cswitch(ints, 0, is(0), nodefault);
  cswitch(ints, 1, is(1), nodefault);
  cswitch(ints, 2, is(2), nodefault);
  cswitch(ints, 3, throw_, nodefault);

  is(0)(rswitch_or(ints, 0, is(0), 5));
  is(1)(rswitch_or(ints, 1, is(1), 5));
  is(2)(rswitch_or(ints, 2, is(2), 5));
  is(5)(rswitch_or(ints, 3, is(3), 5));

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

  auto set = [](auto v) { return [v](auto i) mutable { v = i; }; };
  std::integral_constant<int, 3> c;

  cswitch(ints, a, set(a));
  cswitch(ints, b, set(b));
  cswitch(ints, c, set(c));

  is(1)(rswitch_or(ints, a, is(1), 5));
  is(2)(rswitch_or(ints, b, is(2), 5));
  is(5)(rswitch_or(ints, c, is(3), 5));

#ifndef _MSC_VER // (`x ? a : b` bug)
  auto rself = [](auto x) { return x; };

  rswitch(ints, a, rself) = a;
  rswitch(ints, b, rself) = b;
  rswitch(ints, c, rself) = c;

  rswitch(ints, a, rself, throw_) = a;
  rswitch(ints, b, rself, throw_) = b;
  rswitch(ints, c, throw_, rself) = c;

  rswitch(ints, a, rself, nodefault) = a;
  rswitch(ints, b, rself, nodefault) = b;
  is(0)(rswitch(ints, c, rself, nodefault));
#else
  is(1)(rswitch(ints, a, is(1)));
  is(2)(rswitch(ints, b, is(2)));
  is(3)(rswitch(ints, c, is(3)));

  is(1)(rswitch(ints, a, is(1), xto4));
  is(2)(rswitch(ints, b, is(2), xto4));
  is(3)(rswitch(ints, c, xto4, is(3)));

  is(1)(rswitch(ints, a, is(1), nodefault));
  is(2)(rswitch(ints, b, is(2), nodefault));
  is(0)(rswitch(ints, c, is(3), nodefault));
#endif

  auto ithrow = [](auto) -> int { throw 2; };
  rswitch(ints, 1, is(1), ithrow);
  rswitch(ints, 2, is(2), ithrow);
  rswitch(ints, 5, ithrow, is(5));
}
