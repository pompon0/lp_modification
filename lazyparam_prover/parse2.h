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


#include "tptp.pb.h"

#include "lazyparam_prover/util/string.h"
#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/clause.h"
#include "lazyparam_prover/log.h"

namespace tableau {

void parse_term(tptp::Term *out, const Kernel::TermList *in) { FRAME("parse_term");
  if(in->isVar()) {
    out->set_type(tptp::Term::VAR);
    out->set_name(util::fmt("V%",in->var()));
  } else if(in->isTerm()) {
    auto *t = in->term();
    out->set_type(tptp::Term::EXP);
    out->set_name(util::fmt("f%",t->functor()));
    auto *args = out->mutable_args();
    for(size_t i=0; i<t->arity(); ++i) {
      parse_term(args->Add(),t->nthArgument(i));
    }
  } else {
    error("unsupported term");
  }
}

void parse_literal(tptp::Formula *out, const Kernel::Literal *in) { FRAME("parse_literal");
  if(in->isNegative()) {
    auto *op = out->mutable_op();
    op->set_type(tptp::Formula::Operator::NEG);
    out = op->mutable_args()->Add();
  }
  auto *pred = out->mutable_pred();
  if(in->isEquality()) {
    pred->set_type(tptp::Formula::Pred::EQ);
  } else {
    pred->set_type(tptp::Formula::Pred::CUSTOM);
    pred->set_name(util::fmt("p%",in->functor()));
  }
  auto *args = pred->mutable_args();
  for(size_t i=0; i<in->arity(); ++i) {
    parse_term(args->Add(),in->nthArgument(i));
  }
}

void parse_formula(tptp::Formula *out, const Kernel::Formula *in) { FRAME("parse_formula");
  switch(in->connective()) {
    case Kernel::LITERAL: {
      auto a = static_cast<const Kernel::AtomicFormula*>(in);
      parse_literal(out,a->getLiteral());
      return;
    }
    case Kernel::AND:
    case Kernel::OR: {
      auto j = static_cast<const Kernel::JunctionFormula*>(in);
      auto *op = out->mutable_op();
      switch(in->connective()) {
        case Kernel::AND: op->set_type(tptp::Formula::Operator::AND); break;
        case Kernel::OR: op->set_type(tptp::Formula::Operator::OR); break;
        default: error("WTF");
      }
      auto args = op->mutable_args();
      for(const Kernel::FormulaList *j_args = j->getArgs(); Kernel::FormulaList::isNonEmpty(j_args); j_args = j_args->tail()) {
        parse_formula(args->Add(),j_args->head());
      }
      return;
    }
    case Kernel::IMP:
    case Kernel::IFF:
    case Kernel::XOR: {
      auto b = static_cast<const Kernel::BinaryFormula*>(in);
      auto *op = out->mutable_op();
      switch(in->connective()) {
        case Kernel::IMP: op->set_type(tptp::Formula::Operator::IMPL); break;
        case Kernel::IFF: op->set_type(tptp::Formula::Operator::IFF); break;
        case Kernel::XOR: op->set_type(tptp::Formula::Operator::XOR); break;
        default: error("WTF");
      }
      parse_formula(op->mutable_args()->Add(),b->lhs());
      parse_formula(op->mutable_args()->Add(),b->rhs());
      return;
    }
    case Kernel::NOT: {
      auto n = static_cast<const Kernel::NegatedFormula*>(in);
      auto *op = out->mutable_op();
      op->set_type(tptp::Formula::Operator::NEG);
      parse_formula(op->mutable_args()->Add(),n->subformula());
      return;
    }
    case Kernel::FORALL:
    case Kernel::EXISTS: {
      auto inq = static_cast<const Kernel::QuantifiedFormula*>(in);
      auto *outq = out->mutable_quant();
      switch(in->connective()) {
        case Kernel::FORALL: outq->set_type(tptp::Formula::Quant::FORALL); break;
        case Kernel::EXISTS: outq->set_type(tptp::Formula::Quant::EXISTS); break;
        default: error("WTF");
      }
      for(const Kernel::Formula::VarList *vl = inq->varList(); Kernel::Formula::VarList::isNonEmpty(vl); vl = vl->tail()) {
        outq->mutable_var()->Add(util::fmt("V%",vl->head()));
      }
      parse_formula(outq->mutable_sub(), inq->subformula());
      return;
    }
    case Kernel::BOOL_TERM: error("unsupported BOOL_TERM");
    case Kernel::FALSE: out->mutable_op()->set_type(tptp::Formula::Operator::FALSE); return;
    case Kernel::TRUE: out->mutable_op()->set_type(tptp::Formula::Operator::TRUE); return;
    case Kernel::NAME: error("unsupported NAME");
    case Kernel::NOCONN: error("unsupported NOCONN");
  }
}

// TODO: add tests for clause parsing
void parse_clause(tptp::Formula *out, const Kernel::Clause *cla) { FRAME("parse_clause(%)",cla->toString());
  auto *op = out->mutable_op();
  op->set_type(tptp::Formula::Operator::OR);
  auto *args = op->mutable_args();
  for(size_t i=0; i<cla->size(); ++i) {
    parse_literal(args->Add(),(*cla)[i]);
  }
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
      parse_clause(input->mutable_formula(),cla);
    } else {
      auto fu = static_cast<Kernel::FormulaUnit*>(unit);
      input->set_language(tptp::Input::FOF);
      parse_formula(input->mutable_formula(),fu->formula());
    }
  }
}

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

bool has_equality(const tptp::Formula &f) {
  switch(f.formula_case()) {
    case tptp::Formula::kOp:
      for(const auto &a : f.op().args()) {
        if(has_equality(a)) return 1;
      }
      return 0;
    case tptp::Formula::kQuant:
      return has_equality(f.quant().sub());
    case tptp::Formula::kPred:
      return f.pred().type()==tptp::Formula::Pred::EQ;
    default: error("f.formula_case() = %",f.formula_case());
  }
}

bool has_equality(const tptp::File &f) {
  for(const auto &i : f.input()) {
    if(has_equality(i.formula())) return 1;
  }
  return 0;
}

} // namespace tableau

#endif  // PARSE2_H_
