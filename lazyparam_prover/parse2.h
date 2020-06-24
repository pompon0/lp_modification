#ifndef PARSE2_H_
#define PARSE2_H_

#include "Forwards.hpp"
#include "Kernel/Clause.hpp"
#include "Kernel/Formula.hpp"
#include "Kernel/FormulaUnit.hpp"
#include "Kernel/Unit.hpp"
#include "Parse/TPTP.hpp"
#include "Shell/TPTPPrinter.hpp"
// Vampire defines __APPLE__ macro, which makes
// other libraries freak out.
#undef __APPLE__


#include "google/protobuf/repeated_field.h"

#include "tptp.pb.h"

#include "lazyparam_prover/util/string.h"
#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/clause.h"
#include "lazyparam_prover/log.h"

namespace tableau {

enum {
  CUSTOM_ARITY = -2,
  VARIADIC_ARITY = -1,
};

const std::map<tptp::Type,int> TYPE_ARITY = {
  {tptp::PRED_EQ,2},
  {tptp::PRED,CUSTOM_ARITY},
  {tptp::TERM_FUN,CUSTOM_ARITY},
  {tptp::TERM_VAR,0},
  {tptp::FORM_NEG,1},
  {tptp::FORM_OR,VARIADIC_ARITY},
  {tptp::FORM_AND,VARIADIC_ARITY},
  {tptp::FORM_IFF,2},
  {tptp::FORM_IMPL,2},
  {tptp::FORM_RIMPL,2},
  {tptp::FORM_XOR,2},
  {tptp::FORM_NOR,2},
  {tptp::FORM_NAND,2},
  {tptp::FORM_TRUE,0},
  {tptp::FORM_FALSE,0},
  {tptp::FORALL,2},
  {tptp::EXISTS,2},
};

using NodeId = int32_t;

struct NodeIndex {
private:
  std::map<tptp::Type,NodeId> standard_;
  std::map<unsigned,NodeId> funs_,preds_,vars_;
public:
  google::protobuf::RepeatedPtrField<tptp::Node> nodes;
  
  NodeId var(unsigned i) {
    if(vars_.count(i)) return vars_[i];
    NodeId id = nodes.size();
    vars_[i] = id;
    auto n = nodes.Add();
    n->set_type(tptp::TERM_VAR);
    n->set_id(id);
    return id;
  }

  NodeId fun(unsigned f, size_t arity) {
    if(funs_.count(f)) return funs_[f];
    NodeId id = nodes.size();
    funs_[f] = id;
    auto n = nodes.Add();
    n->set_type(tptp::TERM_FUN);
    n->set_id(id);
    n->set_arity(arity);
    n->set_name(Lib::env.signature->functionName(f).c_str());
    return id;
  }

  NodeId pred(unsigned p, size_t arity) {
    if(preds_.count(p)) return preds_[p];
    NodeId id = nodes.size();
    preds_[p] = id;
    auto n = nodes.Add();
    n->set_type(tptp::PRED);
    n->set_id(id);
    n->set_arity(arity);
    n->set_name(Lib::env.signature->predicateName(p).c_str());
    return id;
  }

  NodeId standard(tptp::Type t) {
    DEBUG if(TYPE_ARITY.at(t)==CUSTOM_ARITY) error("% is not a standard node type",t);
    if(standard_.count(t)) return standard_[t];
    NodeId id = nodes.size();
    standard_[t] = id;
    auto n = nodes.Add();
    n->set_type(t);
    n->set_id(id);
    return id;
  }
};

struct NodeStream {
  vec<int32_t> stream;
  void add(int32_t v){ stream.push_back(v); }
};

struct ParseCtx {
  NodeIndex idx;

  void parse_term(NodeStream &s, const Kernel::TermList *in) { FRAME("parse_term");
    //TODO: append content of env.signature to the proto, don't use names at all.
    if(in->isVar()) {
      s.add(idx.var(in->var()));
    } else if(in->isTerm()) {
      auto *t = in->term();
      s.add(idx.fun(t->functor(),t->arity()));
      for(size_t i=0; i<t->arity(); ++i) parse_term(s,t->nthArgument(i));
    } else {
      error("unsupported term");
    }
  }

  void parse_literal(NodeStream &s, const Kernel::Literal *in) { FRAME("parse_literal");
    if(in->isNegative()) s.add(idx.standard(tptp::FORM_NEG));
    if(in->isEquality()) s.add(idx.standard(tptp::PRED_EQ));
    else s.add(idx.pred(in->functor(),in->arity()));
    for(size_t i=0; i<in->arity(); ++i) parse_term(s,in->nthArgument(i));
  }

  void parse_formula(NodeStream &s, const Kernel::Formula *in) { FRAME("parse_formula");
    switch(in->connective()) {
      case Kernel::LITERAL: {
        auto a = static_cast<const Kernel::AtomicFormula*>(in);
        parse_literal(s,a->getLiteral());
        return;
      }
      case Kernel::AND:
      case Kernel::OR: {
        auto j = static_cast<const Kernel::JunctionFormula*>(in);
        switch(in->connective()) {
          case Kernel::AND: s.add(idx.standard(tptp::FORM_AND)); break;
          case Kernel::OR: s.add(idx.standard(tptp::FORM_OR)); break;
          default: error("WTF");
        }
        const Kernel::FormulaList *args = j->getArgs();
        s.add(Kernel::FormulaList::length(args));
        for(; Kernel::FormulaList::isNonEmpty(args); args = args->tail()) parse_formula(s,args->head());
        return;
      }
      case Kernel::IMP:
      case Kernel::IFF:
      case Kernel::XOR: {
        auto b = static_cast<const Kernel::BinaryFormula*>(in);
        switch(in->connective()) {
          case Kernel::IMP: s.add(idx.standard(tptp::FORM_IMPL)); break;
          case Kernel::IFF: s.add(idx.standard(tptp::FORM_IFF)); break;
          case Kernel::XOR: s.add(idx.standard(tptp::FORM_XOR)); break;
          default: error("WTF");
        }
        parse_formula(s,b->lhs());
        parse_formula(s,b->rhs());
        return;
      }
      case Kernel::NOT: {
        auto n = static_cast<const Kernel::NegatedFormula*>(in);
        s.add(idx.standard(tptp::FORM_NEG));
        parse_formula(s,n->subformula());
        return;
      }
      case Kernel::FORALL:
      case Kernel::EXISTS: {
        auto inq = static_cast<const Kernel::QuantifiedFormula*>(in);
        NodeId q;
        switch(in->connective()) {
          case Kernel::FORALL: q = idx.standard(tptp::FORALL); break;
          case Kernel::EXISTS: q = idx.standard(tptp::EXISTS); break;
          default: error("WTF");
        }
        for(const Kernel::Formula::VarList *vl = inq->varList(); Kernel::Formula::VarList::isNonEmpty(vl); vl = vl->tail()) {
          s.add(q);
          s.add(idx.var(vl->head()));
        }
        parse_formula(s, inq->subformula());
        return;
      }
      case Kernel::BOOL_TERM: error("unsupported BOOL_TERM");
      case Kernel::FALSE: s.add(idx.standard(tptp::FORM_FALSE)); return;
      case Kernel::TRUE: s.add(idx.standard(tptp::FORM_TRUE)); return;
      case Kernel::NAME: error("unsupported NAME");
      case Kernel::NOCONN: error("unsupported NOCONN");
    }
  }

  // TODO: add tests for clause parsing
  void parse_clause(NodeStream &s, const Kernel::Clause *cla) { FRAME("parse_clause(%)",cla->toString());
    s.add(idx.standard(tptp::FORM_OR));
    s.add(cla->size());
    for(size_t i=0; i<cla->size(); ++i) parse_literal(s,(*cla)[i]);
  }

  void parse_file(tptp::File *file, std::istream &is) { FRAME("parse_file");
    Lib::env.options->setOutputAxiomNames(true);
    UnitList *units;
    try {
      units = Parse::TPTP::parse(is);
    } catch(Parse::TPTP::ParseErrorException &e) {
      e.cry(std::cerr);
      error("%",e.msg());
    }
    for(; UnitList::isNonEmpty(units); units = units->tail()) {
      tptp::Input *input = file->mutable_input()->Add();
      Unit *unit = units->head();
      switch(unit->inputType()) {
        case Kernel::Unit::AXIOM:
        case Kernel::Unit::ASSUMPTION: {
          input->set_role(tptp::Input::AXIOM);
          vstring name;
          if(!Parse::TPTP::findAxiomName(unit,name)) error("findAxiomName() failed");
          input->set_name(util::fmt("%",name));
          break;
        }
        // vampire parser actually negates CONJECTUREs for uniformity
        case Kernel::Unit::CONJECTURE:
        case Kernel::Unit::NEGATED_CONJECTURE:
          input->set_role(tptp::Input::NEGATED_CONJECTURE);
          input->set_name("conjecture");
          break;
        default: error("unit->inputType() = %",unit->inputType());
      }
      if(unit->isClause()) {
        auto cla = static_cast<Kernel::Clause*>(unit);
        input->set_language(tptp::Input::CNF);
        NodeStream s;
        parse_clause(s,cla);
        input->mutable_formula()->Add(s.stream.begin(),s.stream.end());
      } else {
        auto fu = static_cast<Kernel::FormulaUnit*>(unit);
        input->set_language(tptp::Input::FOF);
        NodeStream s;
        parse_formula(s,fu->formula());
        input->mutable_formula()->Add(s.stream.begin(),s.stream.end());
      }
    }
    file->mutable_nodes()->CopyFrom(idx.nodes);
  }
};

// TODO: requires tuning: curently outputs tff with not shortened names
void inline_imports(std::ostream &os, std::istream &is) { FRAME("inline_imports");
  Lib::env.options->setOutputAxiomNames(true);
  UnitList *units;
  try {
    units = Parse::TPTP::parse(is);
  } catch(Parse::TPTP::ParseErrorException &e) {
    e.cry(std::cerr);
    error("%",e.msg());
  }
  Shell::TPTPPrinter p(&os);
  for(; UnitList::isNonEmpty(units); units = units->tail()) {
    p.print(units->head());
  }
}

bool has_equality(const tptp::File &f) {
  for(auto n : f.nodes()) {
    if(n.type()==tptp::PRED_EQ) return 1;
  }
  return 0;
}

} // namespace tableau

#endif  // PARSE2_H_
