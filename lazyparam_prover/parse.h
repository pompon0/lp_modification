#ifndef PARSE_H_
#define PARSE_H_

#include "tptp.pb.h"
#include "google/protobuf/text_format.h"
#include "lazyparam_prover/pred.h"

template<typename T> struct IntDict {
  size_t operator()(const T &v) {
    if(!m.count(v)){ auto x = m.size(); m[v] = x; }
    return m[v];
  }
  void clear(){ m.clear(); }
  size_t size(){ return m.size(); }

  std::map<size_t,T> rev() const {
    std::map<size_t,T> rm;
    for(const auto &p : m) rm[p.second] = p.first;
    return rm;
  }

private:
  std::map<T,size_t> m;
};

struct ParseCtx {
  IntDict<str> pred_names;
  IntDict<str> fun_names;
  IntDict<str> var_names;

  Term parse_term(const tptp::Term &t) {
    FRAME("parse_term(%)",t.DebugString());
    switch(t.type()){
      case tptp::Term::VAR: {
        return Term(Var::make(var_names(t.name())));
      }
      case tptp::Term::EXP: {
        size_t ac = t.args().size();
        Fun::Builder b(fun_names(t.name()),ac);
        for(size_t i=0; i<ac; ++i)
          b.set_arg(i,parse_term(t.args()[i]));
        return Term(b.build());
      }
      default:
        error("unexpected t.type() = %",t.type());
    }
  }

  Atom parse_atom(const tptp::Formula &f) {
    FRAME("parse_atom(%)",f.DebugString());
    switch(f.formula_case()) {
    case tptp::Formula::kOp: {
      if(f.op().type()!=tptp::Formula::Operator::NEG)
        error("f.op().type() = %, want %",f.op().type(),tptp::Formula::Operator::NEG);
      if(f.op().args().size()!=1)
        error("f.op.args().size() = %, want %",f.op().args().size(),1);
      return parse_atom(f.op().args()[0]).neg();
    }
    case tptp::Formula::kPred: {
      switch(f.pred().type()) {
      case tptp::Formula::Pred::CUSTOM: {
        size_t ac = f.pred().args().size();
        Atom::Builder b(true,pred_names(f.pred().name()),ac);
        for(size_t i=0; i<ac; ++i)
          b.set_arg(i,parse_term(f.pred().args()[i]));
        return b.build();
      }
      case tptp::Formula::Pred::EQ: {
        if(f.pred().args().size()!=2)
          error("f.pred().args().size() = %, want %",f.pred().args().size(),2);
        return Atom::eq(true, parse_term(f.pred().args()[0]), parse_term(f.pred().args()[1]));
      }
      default:
        error("unexpected f.pred().type() = %",f.pred().type());
      }
    }
    default:
      error("unexpected f.formula_case() = %",f.formula_case());
    }
  }

  OrClause parse_orClause(const tptp::Formula &f) {
    FRAME("parse_orClause(%)",f.DebugString());
    var_names.clear();
    vec<Atom> atoms;
    switch(f.formula_case()) {
    case tptp::Formula::kOp: {
      switch(f.op().type()) {
        case tptp::Formula::Operator::OR: {
          for(const auto &a : f.op().args())
            atoms.push_back(parse_atom(a));
          break;
        }
        case tptp::Formula::Operator::FALSE: break;
        default: {
          atoms.push_back(parse_atom(f));
          break;
        }
      }
      break;
    }
    case tptp::Formula::kPred: {
      atoms.push_back(parse_atom(f));
      break;
    }
    default:
      error("unexpected f.formula_case() = %",f.formula_case());
    }
    OrClause::Builder b(atoms.size(),var_names.size());
    for(size_t i=0; i<atoms.size(); ++i) b.set_atom(i,atoms[i]);
    return b.build();
  }

  NotAndForm parse_notAndForm(const tptp::File &file) {
    NotAndForm form;
    for(const tptp::Input &input : file.input()) {
      if(input.language()!=tptp::Input::CNF)
        error("input.language() = %, want CNF",input.language());
      switch(input.role()) {
      case tptp::Input::AXIOM:
      case tptp::Input::PLAIN:
      case tptp::Input::NEGATED_CONJECTURE: {
        OrClause cla = parse_orClause(input.formula());
        form.or_clauses.push_back(DerOrClause(cla.atom_count()>1,cla));
        break;
      }
      default:
        error("unexpected input.role() = %",input.role());
      }
    }
    return form;
  }

  NotAndForm parse_notAndForm(const str &file_raw) {
    tptp::File file;
    if(!file.ParseFromString(file_raw)) {
      error("failed to parse input");
    }
    return parse_notAndForm(file);
  }
};

struct ProtoCtx {
  ProtoCtx(const ParseCtx &pc) : pred_names(pc.pred_names.rev()), fun_names(pc.fun_names.rev()) {
    fun_names[Fun::EXTRA_CONST] = "c";
  }
  std::map<size_t,str> pred_names;
  std::map<size_t,str> fun_names;

  tptp::Term proto_term(Term t) const {
    tptp::Term pt;
    switch(t.type()) {
      case Term::VAR: {
        pt.set_type(tptp::Term::VAR);
        pt.set_name(show(t));
        break;
      }
      case Term::FUN: {
        Fun f(t);
        pt.set_type(tptp::Term::EXP);
        pt.set_name(fun_names.at(f.fun()));
        for(size_t i=0; i<f.arg_count(); ++i)
          *(pt.add_args()) = proto_term(f.arg(i));
        break;
      }
    }
    return pt;
  }

  tptp::Formula proto_atom(Atom a) const {
    tptp::Formula f;
    //TODO: error if a.pred() not in pred_names
    if(a.pred()==Atom::EQ) {
      f.mutable_pred()->set_type(tptp::Formula::Pred::EQ);
    } else {
      f.mutable_pred()->set_type(tptp::Formula::Pred::CUSTOM);
      f.mutable_pred()->set_name(pred_names.at(a.pred()));
    }
    for(size_t i=0; i<a.arg_count(); ++i)
      *(f.mutable_pred()->add_args()) = proto_term(a.arg(i));
    if(!a.sign()) {
      tptp::Formula nf;
      nf.mutable_op()->set_type(tptp::Formula::Operator::NEG);
      *(nf.mutable_op()->add_args()) = f;
      return nf;
    }
    return f;
  }

  tptp::Formula proto_orClause(const OrClause &cla) const {
    tptp::Formula f;
    f.mutable_op()->set_type(tptp::Formula::Operator::OR);
    for(size_t i=0; i<cla.atom_count(); ++i) *(f.mutable_op()->add_args()) = proto_atom(cla.atom(i));
    return f;
  }

  tptp::File proto_notAndForm(const NotAndForm &f) const {
    tptp::File file;
    for(const auto &cla : f.or_clauses) {
      auto input = file.add_input();
      input->set_role(tptp::Input::PLAIN);
      input->set_language(tptp::Input::CNF);
      *(input->mutable_formula()) = proto_orClause(cla.derived());
    }
    return file;
  }
};

#endif  // PARSE_H_

