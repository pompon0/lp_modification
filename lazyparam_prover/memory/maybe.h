#ifndef MEMORY_MAYBE_H_
#define MEMORY_MAYBE_H_

namespace tableau {

template<typename T> struct Maybe {
private:
  u8 data[sizeof(T)];
  bool present;
public:
  Maybe() : present(0) {}
  explicit Maybe(const T &v) : present(1) { new(data)T(v); }
  explicit operator bool() const { return present; } 
  T get() const {
    DEBUG if(!present) error("Maybe::get(): not present");
    return *(T*)data;
  }
  bool operator==(Maybe<T> m) const {
    return present == m.present && (!present || get()==m.get());
  }
};

}  // namespace tableau

#endif  // MEMORY_MAYBE_H_
