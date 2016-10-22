g++-4.9, g++-5, clang-3.6: [![Travis Build Status](https://travis-ci.org/jonathanpoelen/falcon.cexpr.svg?branch=master)](https://travis-ci.org/jonathanpoelen/falcon.cexpr)

VS 2015: [![Appveyor Build Status](https://ci.appveyor.com/api/projects/status/github/jonathanpoelen/falcon.cexpr)](https://ci.appveyor.com/project/jonathanpoelen/falcon.cexpr)


# cexpr

```cpp
namespace falcon { namespace cexpr {

std::integral_constant<bool, bool(T::value)> cbool(T);

True select(std::true_type, yes, no);
False select(std::false_type, yes, no);
decltype(select(cbool(cond), yes, no)) select(cond, yes, no);

decltype(func_yes()) cif(std::true_type, func_yes);
void cif(std::false_type, func_yes);
decltype(cif(cbool(cond), func_yes)) cif(cond, func_yes);

decltype(func_yes()) cif(std::true_type, func_yes, func_no);
decltype(func_no()) cif(std::false_type, func_yes, func_no);
decltype(cif(cbool(cond), func_yes, func_no)) cif(cond, func_yes, func_no);

constexpr class nodefault_t {} nodefault;

void cswitch(std::integer_sequence ints, i, func); // = cswitch(ints, i, func, func)
void cswitch(std::integer_sequence ints, i, func, nodefault_t);
void cswitch(std::integer_sequence ints, i, func, default_func);

T rswitch(T, std::integer_sequence ints, i, func); // = rswitch(T, ints, i, func, func)
T rswitch(T, std::integer_sequence ints, i, func, nodefault_t);
T rswitch(T, std::integer_sequence ints, i, func, default_func);

auto rswitch(std::integer_sequence ints, i, func); // = rswitch(T, ints, i, func, func)
std::common_type_t<decltype(func(ic))..., decltype(func(i))>
  rswitch(std::integer_sequence<I, ic...>, i, func, nodefault_t);
std::common_type_t<decltype(func(ic))..., decltype(default_func(i))>
  rswitch(std::integer_sequence<I, ic...>, i, func, default_func);

} }
```
