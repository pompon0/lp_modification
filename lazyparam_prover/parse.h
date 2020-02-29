#ifndef PARSE_H_
#define PARSE_H_

#include "tptp.pb.h"
#include "solutions.pb.h"
#include "google/protobuf/io/coded_stream.h"
#include "lazyparam_prover/pred.h"
#include "lazyparam_prover/derived.h"
#include "lazyparam_prover/kbo.h"
#include "lazyparam_prover/ground.h"

namespace tableau {

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

  NotAndForm parse_notAndForm(const str &file_raw) { FRAME("parse_notAndForm()");
    tptp::File file;
    auto stream = new google::protobuf::io::CodedInputStream((const uint8_t*)(&file_raw[0]),file_raw.size());
    stream->SetRecursionLimit(100000000);
    if(!file.ParseFromCodedStream(stream)) {
      error("failed to parse input");
    }
    return parse_notAndForm(file);
  }
};

struct ProtoCtx {
  ProtoCtx(const ParseCtx &pc) : pred_names(pc.pred_names.rev()), fun_names(pc.fun_names.rev()) {
    fun_names[Fun::EXTRA_CONST] = "c";
    pred_names[Atom::EQ_TRANS_POS] = "eqt";
  }
  std::map<size_t,str> pred_names;
  std::map<size_t,str> fun_names;

  tptp::Term proto_term(Term t) const { FRAME("proto_term()");
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
        DEBUG if(!fun_names.count(f.fun())) error("fun_names.count(%) = 0",f.fun());
        pt.set_name(fun_names.at(f.fun()));
        for(size_t i=0; i<f.arg_count(); ++i)
          *(pt.add_args()) = proto_term(f.arg(i));
        break;
      }
    }
    return pt;
  }

  tptp::Formula proto_atom(Atom a) const { FRAME("proto_atom()");
    tptp::Formula f;
    //TODO: error if a.pred() not in pred_names
    if(a.pred()==Atom::EQ) {
      f.mutable_pred()->set_type(tptp::Formula::Pred::EQ);
    } else {
      f.mutable_pred()->set_type(tptp::Formula::Pred::CUSTOM);
      //DEBUG if(!pred_names.count(a.pred())) error("pred_names.count(%) = 0",a.pred());
      f.mutable_pred()->set_name(pred_names.count(a.pred()) ? pred_names.at(a.pred()) : util::fmt("unknownPred%",a.pred()));
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

  tptp::Formula proto_orClause(const OrClause &cla) const { FRAME("proto_orClause()");
    tptp::Formula f;
    f.mutable_op()->set_type(tptp::Formula::Operator::OR);
    for(size_t i=0; i<cla.atom_count(); ++i) *(f.mutable_op()->add_args()) = proto_atom(cla.atom(i));
    return f;
  }

  solutions::Derivation proto_derAndClause(const DerAndClause &cla, const KBO &val) const { FRAME("proto_derAndClause()");
    solutions::Derivation d;
    d.set_cost(cla.cost);
    auto derived = d.mutable_derived();
    derived->set_name("derived");
    derived->set_role(tptp::Input::PLAIN);
    derived->set_language(tptp::Input::CNF);
    *(derived->mutable_formula()) = proto_orClause(ground(val.eval(cla.derived.neg())));
    for(const auto &s : cla.source) {
      auto ps = d.add_sources();
      auto pg = ps->mutable_ground();
      pg->set_name("ground");
      pg->set_role(tptp::Input::PLAIN);
      pg->set_language(tptp::Input::CNF);
      *(pg->mutable_formula()) = proto_orClause(ground(val.eval(s.neg())));
      auto pss = ps->mutable_source();
      pss->set_name("source");
      pss->set_role(tptp::Input::PLAIN);
      pss->set_language(tptp::Input::CNF);
      *(pss->mutable_formula()) = proto_orClause(s.neg());
    }
    return d;
  }

  solutions::Proof proto_Proof(const OrForm &f, const KBO &val) { FRAME("proto_Proof");
    solutions::Proof proof;
    size_t i=0;
    for(const auto &cla : f.and_clauses) {
      auto pcla = proof.add_clauses();
      *pcla = proto_derAndClause(cla,val);
      pcla->mutable_derived()->set_name(util::fmt("a%",i++));
    }
    return proof;
  }

  tptp::File proto_notAndForm(const NotAndForm &f) const { FRAME("proto_notAndForm()");
    tptp::File file;
    size_t i = 0;
    for(const auto &cla : f.or_clauses) {
      auto input = file.add_input();
      input->set_name(util::fmt("a%",i++));
      input->set_role(tptp::Input::PLAIN);
      input->set_language(tptp::Input::CNF);
      *(input->mutable_formula()) = proto_orClause(cla.derived());
    }
    return file;
  }
};

}  // namespace tableau

#endif  // PARSE_H_

