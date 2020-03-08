#ifndef CONSTRAINT_H_
#define CONSTRAINT_H_

namespace tableau {

struct Constraint;

struct Constraint0 {
  enum Type { NEQ, LT, LE, TRUE };
  struct Pair { Term0 l,r; }; 
private:
  using TYPE = Field<Type>;
  using VAR_END = Field<u64,TYPE>;
  using OR = ArrayField<Pair,VAR_END>;
  u8 *ptr;
  Constraint0(u8 *_ptr) : ptr(_ptr) {}
public:
  
  struct Builder {
  private:
    u8 *ptr;
  public:
    Builder(Type _type, size_t _or_count) : ptr(OR::alloc(or_count)) { TYPE::ref(ptr) = _type; }
    Builder& set_or(size_t i, Pair p){
      OR::ref(ptr,i) = p;
      util::maxi(VAR_END::ref(ptr),p.l.var_end());
      util::maxi(VAR_END::ref(ptr),p.r.var_end());
      return *this;
    }
    Constraint0 build(){ return Constraint0(ptr); }
  };
 
  // ignores sign
  static Constraint0 neq(Atom0 l, Atom0 r) {
    if(l.pred()!=r.pred()) return Builder(TRUE,0).build();
    DEBUG if(l.arg_count()!=r.arg_count()) error("l.arg_count() = %, r.arg_count() = %",show(l),show(r));
    Builder b(NEQ,l.arg_count());
    for(size_t i=l.arg_count(); i--;) b.set_or({l.arg(i),r.arg(i)});
    return b.build();
  }

  static Constraint0 neq(Term0 l, Term0 r) {
    return Builder(NEQ,1).set_or(0,{l,r}).build();
  }

  static Constraint0 lt(Term0 l, Term0 r) {
    return Builder(LT,1).set_or({l,r}).build();
  }

  static Constraint0 le(Term0 l, Term0 r) {
    return Builder(GT,1).set_or({l,r}).build();
  }

  size_t var_end() const { return VAR_END::ref(ptr); }
  Type type() const { return TYPE::ref(ptr); }
  size_t or_count() const { return OR::size(ptr); }
  Pair or_(size_t i) const { return OR::ref(ptr,i); }

  Constraint shift(size_t offset) const { return Constraint(offset,*this); }
};

// On the opposite to atoms and clauses, constraints are also build throughout tableau
// search and they may couple unrelated Atoms - they are not pure shifts, and the number
// of pairs may decrease over time.
struct Constraint {

private:
  Constraint(u64 _offset, ) : offset(_offset), constraint(_constraint) {}
  u64 offset;
  Constraint0 constraint;
};

}  // namespace tableau

#endif  // CONSTRAINT_H_
