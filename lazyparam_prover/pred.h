#ifndef PRED_H_
#define PRED_H_

namespace tableau {





static_assert(sizeof(u64*)==sizeof(u64));
static_assert(sizeof(Term)==2*sizeof(u64));
static_assert(sizeof(Var)==sizeof(Term));
static_assert(sizeof(Fun)==sizeof(Term));
static_assert(sizeof(Atom)<=sizeof(Term)+2*sizeof(u64));
static_assert(sizeof(OrClause)==sizeof(AndClause));

}  // namespace tableau

#endif // PRED_H_
