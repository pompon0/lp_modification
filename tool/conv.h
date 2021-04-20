#ifndef TOOL_CONV_H_
#define TOOL_CONV_H_

#include "Forwards.hpp"
#include "Kernel/Clause.hpp"
#include "Kernel/Formula.hpp"
#include "Kernel/FormulaUnit.hpp"
#include "Kernel/Unit.hpp"
#include "Parse/TPTP.hpp"
#include "Shell/TPTPPrinter.hpp"

#include "utils/log.h"
#include "utils/string.h"
#include "tool/node.h"

namespace tool::conv {

struct TptpToProto {
  static void term(node::OutputStream &s, const Kernel::TermList *in) { FRAME("parse_term");
    //TODO: append content of env.signature to the proto, don't use names at all.
    if(in->isVar()) {
      s.add(s.idx.var(in->var()));
    } else if(in->isTerm()) {
      auto *t = in->term();
      s.add(s.idx.fun(
        t->functor(),t->arity(),
        Lib::env.signature->functionName(t->functor()).c_str()
      ));
      for(size_t i=0; i<t->arity(); ++i) term(s,t->nthArgument(i));
    } else {
      error("unsupported term");
    }
  }

  static void literal(node::OutputStream &s, const Kernel::Literal *in) { FRAME("parse_literal");
    if(in->isNegative()) s.add(s.idx.standard(tptp::FORM_NEG));
    if(in->isEquality()) s.add(s.idx.standard(tptp::PRED_EQ));
    else s.add(s.idx.pred(
      in->functor(),
      in->arity(),
      Lib::env.signature->predicateName(in->functor()).c_str()
    ));
    for(size_t i=0; i<in->arity(); ++i) term(s,in->nthArgument(i));
  }

  static void formula(node::OutputStream &s, const Kernel::Formula *in) { FRAME("parse_formula");
    switch(in->connective()) {
      case Kernel::LITERAL: {
        auto a = static_cast<const Kernel::AtomicFormula*>(in);
        literal(s,a->getLiteral());
        return;
      }
      case Kernel::AND:
      case Kernel::OR: {
        auto j = static_cast<const Kernel::JunctionFormula*>(in);
        switch(in->connective()) {
          case Kernel::AND: s.add(s.idx.standard(tptp::FORM_AND)); break;
          case Kernel::OR: s.add(s.idx.standard(tptp::FORM_OR)); break;
          default: error("WTF");
        }
        const Kernel::FormulaList *args = j->getArgs();
        s.add(Kernel::FormulaList::length(args));
        for(; Kernel::FormulaList::isNonEmpty(args); args = args->tail()) formula(s,args->head());
        return;
      }
      case Kernel::IMP:
      case Kernel::IFF:
      case Kernel::XOR: {
        auto b = static_cast<const Kernel::BinaryFormula*>(in);
        switch(in->connective()) {
          case Kernel::IMP: s.add(s.idx.standard(tptp::FORM_IMPL)); break;
          case Kernel::IFF: s.add(s.idx.standard(tptp::FORM_IFF)); break;
          case Kernel::XOR: s.add(s.idx.standard(tptp::FORM_XOR)); break;
          default: error("WTF");
        }
        formula(s,b->lhs());
        formula(s,b->rhs());
        return;
      }
      case Kernel::NOT: {
        auto n = static_cast<const Kernel::NegatedFormula*>(in);
        s.add(s.idx.standard(tptp::FORM_NEG));
        formula(s,n->subformula());
        return;
      }
      case Kernel::FORALL:
      case Kernel::EXISTS: {
        auto inq = static_cast<const Kernel::QuantifiedFormula*>(in);
        node::Id q;
        switch(in->connective()) {
          case Kernel::FORALL: q = s.idx.standard(tptp::FORALL); break;
          case Kernel::EXISTS: q = s.idx.standard(tptp::EXISTS); break;
          default: error("WTF");
        }
        for(const Kernel::Formula::VarList *vl = inq->varList(); Kernel::Formula::VarList::isNonEmpty(vl); vl = vl->tail()) {
          s.add(q);
          s.add(s.idx.var(vl->head()));
        }
        formula(s, inq->subformula());
        return;
      }
      case Kernel::BOOL_TERM: error("unsupported BOOL_TERM");
      case Kernel::FALSE: s.add(s.idx.standard(tptp::FORM_FALSE)); return;
      case Kernel::TRUE: s.add(s.idx.standard(tptp::FORM_TRUE)); return;
      case Kernel::NAME: error("unsupported NAME");
      case Kernel::NOCONN: error("unsupported NOCONN");
    }
  }

  // TODO: add tests for clause parsing
  static void clause(node::OutputStream &s, const Kernel::Clause *cla) { FRAME("parse_clause(%)",cla->toString());
    s.add(s.idx.standard(tptp::FORM_OR));
    s.add(cla->size());
    for(size_t i=0; i<cla->size(); ++i) literal(s,(*cla)[i]);
  }

  static void file(node::Index &idx, tptp::File *out, std::istream &in) { FRAME("parse_file");
    Lib::env.options->setOutputAxiomNames(true);
    UnitList *units;
    try {
      units = Parse::TPTP::parse(in);
    } catch(Parse::TPTP::ParseErrorException &e) {
      e.cry(std::cerr);
      error("%",e.msg());
    }
    for(; UnitList::isNonEmpty(units); units = units->tail()) {
      tptp::Input *input = out->mutable_input()->Add();
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
        node::OutputStream s(idx);
        clause(s,cla);
        input->mutable_formula()->Add(s.stream.begin(),s.stream.end());
      } else {
        auto fu = static_cast<Kernel::FormulaUnit*>(unit);
        input->set_language(tptp::Input::FOF);
        node::OutputStream s(idx);
        formula(s,fu->formula());
        input->mutable_formula()->Add(s.stream.begin(),s.stream.end());
      }
    }
    out->mutable_nodes()->CopyFrom(idx.get_nodes());
  }
};

} // namespace tool::conv

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

#endif  // TOOL_CONV_H_
