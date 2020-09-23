#ifndef MEMORY_FUNCTION_H_
#define MEMORY_FUNCTION_H_

#include "lazyparam_prover/memory/stack.h"

namespace memory {

template<typename F> struct _sig : _sig<decltype(&F::operator())> {};
template<typename F, typename Ret, typename ...Args> struct _sig<Ret(F::*)(Args...) const> {
  using value = Ret(Args...);
};

template<typename F> using sig = typename _sig<F>::value;

template<typename A, typename B> struct same;
template<typename A> struct same<A,A> { enum { value = true }; };

template<typename F, typename Sig> constexpr bool has_sig(){ return same<sig<F>,Sig>::value; }

template<typename Sig> struct function;
template<typename Ret, typename ...Args> struct function<Ret(Args...)> {
  template<typename F> explicit function(memory::Alloc &A, const F &f) {
    static_assert(has_sig<F,Ret(Args...)>());
    call = new(A.alloc_bytes(sizeof(Call<F>)))Call<F>{f};
  };
  INL Ret operator()(Args ...args) const { return (*call)(args...); }
private:
  struct BaseCall { virtual Ret operator()(Args...) const = 0; };
  template<typename F> struct Call : BaseCall {
    F f;
    Ret operator()(Args ...args) const { return f(args...); }
  };
  const BaseCall *call;
};

}  // namespace memory

#endif  // MEMORY_FUNCTION_H_
