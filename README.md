g++-4.9, g++-5, clang-3.6: [![Travis Build Status](https://travis-ci.org/jonathanpoelen/falcon.cstmt.svg?branch=master)](https://travis-ci.org/jonathanpoelen/falcon.cstmt)

VS 2015: [![Appveyor Build Status](https://ci.appveyor.com/api/projects/status/github/jonathanpoelen/falcon.cstmt)](https://ci.appveyor.com/project/jonathanpoelen/falcon-cstmt)


# falcon::cstmt

```cpp
namespace falcon { namespace cstmt {

template<T> struct cbool_trait;
template<> struct cbool_trait<std::true_type> : std::true_type {};
template<> struct cbool_trait<std::false_type> : std::false_type {};

template<T> using cbool_trait_t = typename cbool_trait<T>::type;

auto cbool(T) -> cbool_trait_t<T>;

auto select(std::true_type, yes, no) -> decltype(yes);
auto select(std::false_type, yes, no) -> decltype(no);
auto select(cond, yes, no) -> decltype(select(cbool(cond), yes, no));

auto cif(std::true_type, func_yes) -> decltype(func_yes());
auto cif(std::false_type, func_yes) -> void;
auto cif(cond, func_yes) -> decltype(cif(cbool(cond), func_yes));

auto cif(std::true_type, func_yes, func_no) -> decltype(func_yes());
auto cif(std::false_type, func_yes, func_no) -> decltype(func_no());
auto cif(cond, func_yes, func_no) -> decltype(cif(cbool(cond), func_yes, func_no));

constexpr class nodefault_t {} nodefault;

/// func(ic) if \p i equals \c ic, otherwise \p default_func(\p i)

void cswitch(std::integer_sequence<I, ic...> ints, i, func, nodefault_t);
void cswitch(std::integer_sequence<I, ic...> ints, i, func, default_func);
void cswitch(std::integer_sequence<I, ic...> ints, i, func); // = cswitch(ints, i, func, func);

T rswitch_or(std::integer_sequence<I, ic...> ints, i, func, T default_value);

auto rswitch(std::integer_sequence<I, ic...> ints, i, func, nodefault_t)
  -> std::common_type_t<decltype(func(ic))...>;
auto rswitch(std::integer_sequence<I, ic...> ints, i, func, default_func)
  -> std::common_type_t<decltype(func(ic))..., decltype(default_func(i))>;
auto rswitch(std::integer_sequence<I, ic...> ints, i, func) // = rswitch(ints, i, func, func)
  -> std::common_type_t<decltype(func(ic))..., decltype(func(i))>;

} }
```

# Boolean user type support

Specialize `cbool_trait` or define `cbool(T)` function in the same namespace as `T` ([ADL](http://en.cppreference.com/w/cpp/language/adl)).

`cbool_trait::type` and `cbool` result must be `std::false_type` or `std::true_type`.
