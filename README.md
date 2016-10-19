g++-4.9, g++-5, clang-3.6: [![Travis Build Status](https://travis-ci.org/jonathanpoelen/falcon.cexpr.svg?branch=master)](https://travis-ci.org/jonathanpoelen/falcon.cexpr)

VS 2015: [![Appveyor Build Status](https://ci.appveyor.com/api/projects/status/github/jonathanpoelen/falcon.cexpr)](https://ci.appveyor.com/project/jonathanpoelen/falcon.cexpr)


# cexpr

```cpp
True select(std::true_type, True, False);
False select(std::false_type, True, False);

decltype(func_yes()) cif(std::true_type, func_yes);
void cif(std::false_type, func_yes);

decltype(func_yes()) cif(std::true_type, func_yes, func_no);
decltype(func_no()) cif(std::false_type, func_yes, func_no);

void cswitch(std::integer_sequence, i, func); // = cswitch(ints, i, func, func)
void cswitch(std::integer_sequence, i, func, nodefault);
void cswitch(std::integer_sequence, i, func, default_func);

T rswitch(T, std::integer_sequence, i, func); // = rswitch(T, ints, i, func, func)
T rswitch(T, std::integer_sequence, i, func, nodefault);
T rswitch(T, std::integer_sequence, i, func, default_func);
```
