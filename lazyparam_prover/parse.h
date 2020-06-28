#ifndef PARSE_H_
#define PARSE_H_

#include "tptp.pb.h"
#include "solutions.pb.h"
#include "google/protobuf/io/coded_stream.h"
#include "lazyparam_prover/syntax/term.h"
#include "lazyparam_prover/syntax/atom.h"
#include "lazyparam_prover/syntax/clause.h"
#include "lazyparam_prover/syntax/show.h"
#include "lazyparam_prover/derived.h"
#include "lazyparam_prover/kbo.h"
#include "lazyparam_prover/ground.h"
#include "lazyparam_prover/node.h"

namespace tableau {

struct ParseCtx {
  Term parse_term(NodeInputStream &s) {
    FRAME("parse_term()");
    auto n = s.node();
    switch(n.type()){
      case tptp::TERM_VAR: return Term(Var(n.id()));
      case tptp::TERM_FUN: {
        size_t ac = n.arity();
        Fun::Builder b(n.id(),ac);
        for(size_t i=0; i<ac; ++i) b.set_arg(i,parse_term(s));
        return Term(b.build());
      }
      default:
        error("unexpected n.type() = %",n.type());
    }
  }

  Atom parse_atom(NodeInputStream &s) {
    FRAME("parse_atom()");
    auto n = s.node();
    switch(n.type()) {
    case tptp::FORM_NEG: return parse_atom(s).neg();
    case tptp::PRED_EQ: {
      Term l = parse_term(s);
      Term r = parse_term(s);
      return Atom::eq(true,l,r);
    }
    case tptp::PRED: {
      size_t ac = n.arity();
      Atom::Builder b(true,n.id(),ac,false);
      for(size_t i=0; i<ac; ++i) b.set_arg(i,parse_term(s));
      return b.build();
    }
    default:
      error("unexpected n.type() = %",n.type());
    }
  }

  OrClause parse_orClause(NodeInputStream &s) {
    FRAME("parse_orClause(%)",show(s));
    size_t arity = 0;
    switch(s.node_peek().type()) {
      case tptp::FORM_OR: { s.node(); arity = s.arity(); break; }
      case tptp::FORM_FALSE: { s.node(); arity = 0; break; }
      default: { arity = 1; break; }
    }
    AndClause::Builder b(arity);
    for(size_t i=0; i<arity; ++i) b.set_atom(i,parse_atom(s).neg());
    return b.build().neg();
  }

  OrForm parse_orForm(const tptp::File &file) {
    OrForm form;
    NodeIndex idx(file.nodes());
    FRAME("parse_orForm() idx = %",show(idx));
    for(const tptp::Input &input : file.input()) {
      if(input.language()!=tptp::Input::CNF)
        error("input.language() = %, want CNF",input.language());
      switch(input.role()) {
      case tptp::Input::AXIOM:
      case tptp::Input::PLAIN:
      case tptp::Input::NEGATED_CONJECTURE: {
        NodeInputStream s(idx,input.formula());
        OrClause cla = parse_orClause(s);
        form.and_clauses.push_back(DerAndClause(cla.atom_count()>1,cla.neg()));
        break;
      }
      default:
        error("unexpected input.role() = %",input.role());
      }
    }
    return form;
  } 
};

inline static tptp::File file_from_raw(const str &file_raw) { FRAME("file_from_raw()");
  tptp::File file;
  auto stream = new google::protobuf::io::CodedInputStream((const uint8_t*)(&file_raw[0]),file_raw.size());
  stream->SetRecursionLimit(100000000);
  if(!file.ParseFromCodedStream(stream)) {
    error("failed to parse input");
  }
  return file;
}

struct ProtoCtx {
  RevNodeIndex idx;
  ProtoCtx(const RevNodeIndex &_idx) : idx(_idx) {
    //idx.add_fun(Fun::EXTRA_CONST,0);
    //idx.add_pred(Atom::EQ_TRANS_POS,2);
    //idx.add_pred(Atom::EQ_TRANS_NEG,2);
    //idx.add_pred(Atom::EQ_SYMM,2);
  }

  void proto_term(NodeStream &s, Term t) { FRAME("proto_term()");
    switch(t.type()) {
      case Term::VAR: {
        s.add(idx.add_var(Var(t).id()));
        break;
      }
      case Term::FUN: {
        Fun f(t);
        s.add(idx.add_fun(f.fun(),f.arg_count(),""));
        for(size_t i=0; i<f.arg_count(); ++i) proto_term(s,f.arg(i));
        break;
      }
    }
  }

  void proto_atom(NodeStream &s, Atom a) { FRAME("proto_atom()");
    if(!a.sign()) s.add(idx.add_standard(tptp::FORM_NEG));
    if(a.pred()==Atom::EQ) {
      s.add(idx.add_standard(tptp::PRED_EQ));
    } else {
      s.add(idx.add_pred(a.pred(),a.arg_count(),""));
    }
    for(size_t i=0; i<a.arg_count(); ++i) proto_term(s,a.arg(i));
  }

  void proto_orClause(NodeStream &s, OrClause cla) { FRAME("proto_orClause()");
    s.add(idx.add_standard(tptp::FORM_OR));
    s.add(cla.atom_count());
    for(size_t i=0; i<cla.atom_count(); ++i) proto_atom(s,cla.atom(i));
  }

  solutions::Derivation proto_derAndClause(const DerAndClause &cla, const Valuation &val) { FRAME("proto_derAndClause()");
    solutions::Derivation d;
    d.set_cost(cla.cost());
    auto derived = d.mutable_derived();
    derived->set_name("derived");
    derived->set_role(tptp::Input::PLAIN);
    derived->set_language(tptp::Input::CNF);
    NodeStream s;
    proto_orClause(s,ground(val.eval(cla.derived())).neg());
    derived->mutable_formula()->Add(s.stream.begin(),s.stream.end());
    for(size_t i=0; i<cla.source_count(); ++i) {
      auto ps = d.add_sources();
      auto pg = ps->mutable_ground();
      pg->set_name("ground");
      pg->set_role(tptp::Input::PLAIN);
      pg->set_language(tptp::Input::CNF);
      NodeStream gs;
      proto_orClause(gs,ground(val.eval(cla.source(i))).neg());
      pg->mutable_formula()->Add(gs.stream.begin(),gs.stream.end());
      auto pss = ps->mutable_source();
      NodeStream ss;
      pss->set_name("source");
      pss->set_role(tptp::Input::PLAIN);
      pss->set_language(tptp::Input::CNF);
      proto_orClause(ss,cla.source(i).neg());
      pss->mutable_formula()->Add(ss.stream.begin(),ss.stream.end());
    }
    return d;
  }

  solutions::Proof proto_Proof(const OrForm &f, const Valuation &val) { FRAME("proto_Proof");
    solutions::Proof proof;
    size_t i=0;
    for(const auto &cla : f.and_clauses) {
      auto pcla = proof.add_clauses();
      *pcla = proto_derAndClause(cla,val);
      pcla->mutable_derived()->set_name(util::fmt("a%",i++));
    }
    proof.mutable_nodes()->CopyFrom(idx.nodes);
    return proof;
  }

  tptp::File proto_orForm(const OrForm &f) { FRAME("proto_orForm()");
    tptp::File file;
    size_t i = 0;
    for(const auto &cla : f.and_clauses) {
      auto input = file.add_input();
      input->set_name(util::fmt("a%",i++));
      input->set_role(tptp::Input::PLAIN);
      input->set_language(tptp::Input::CNF);
      NodeStream s;
      proto_orClause(s,cla.derived().neg());
      input->mutable_formula()->Add(s.stream.begin(),s.stream.end());
    }
    file.mutable_nodes()->CopyFrom(idx.nodes);
    return file;
  }
};

}  // namespace tableau

#endif  // PARSE_H_

