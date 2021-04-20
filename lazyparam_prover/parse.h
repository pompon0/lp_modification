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
#include "tool/node.h"

namespace tableau {

struct ProtoToSyntax {
  static Term term(memory::Alloc &A, tool::node::InputStream &s) {
    FRAME("parse_term()");
    auto n = s.node();
    switch(n.type()){
      case tptp::TERM_VAR: return Term(Var(A,n.id()));
      case tptp::TERM_FUN: {
        size_t ac = n.arity();
        Fun::Builder b(A,n.id(),ac);
        for(size_t i=0; i<ac; ++i) b.set_arg(i,term(A,s));
        return Term(b.build());
      }
      default:
        error("unexpected n.type() = %",n.type());
    }
  }

  static Atom atom(memory::Alloc &A, tool::node::InputStream &s) {
    FRAME("parse_atom()");
    auto n = s.node();
    switch(n.type()) {
    case tptp::FORM_NEG: return atom(A,s).neg();
    case tptp::PRED_EQ: {
      Term l = term(A,s);
      Term r = term(A,s);
      return Atom::eq(A,true,l,r);
    }
    case tptp::PRED: {
      size_t ac = n.arity();
      Atom::Builder b(A,true,n.id(),ac,false);
      for(size_t i=0; i<ac; ++i) b.set_arg(i,term(A,s));
      return b.build();
    }
    default:
      error("unexpected n.type() = %",n.type());
    }
  }

  static OrClause orClause(memory::Alloc &A, tool::node::InputStream &s) {
    FRAME("parse_orClause(%)",show(s));
    size_t arity = 0;
    switch(s.node_peek().type()) {
      case tptp::FORM_OR: { s.node(); arity = s.arity(); break; }
      case tptp::FORM_FALSE: { s.node(); arity = 0; break; }
      default: { arity = 1; break; }
    }
    AndClause::Builder b(A,arity);
    for(size_t i=0; i<arity; ++i) b.set_atom(i,atom(A,s).neg());
    return b.build().neg();
  }

  static std::tuple<OrForm,tool::node::Index> orForm(memory::Alloc &A, const tptp::File &file) {
    FRAME("ProtoToSyntax::orForm()");
    tool::node::Index idx(file.nodes());
    OrForm form;
    for(const tptp::Input &input : file.input()) {
      if(input.language()!=tptp::Input::CNF)
        error("input.language() = %, want CNF",input.language());
      switch(input.role()) {
      case tptp::Input::AXIOM:
      case tptp::Input::PLAIN:
      case tptp::Input::NEGATED_CONJECTURE: {
        tool::node::InputStream s(idx,input.formula());
        OrClause cla = orClause(A,s);
        form.and_clauses.push_back(DerAndClause(A,cla.atom_count()>1,cla.neg()));
        break;
      }
      default:
        error("unexpected input.role() = %",input.role());
      }
    }
    return {form,idx};
  } 
};

struct SyntaxToProto {
  static void term(tool::node::OutputStream &s, Term t) { FRAME("proto_term()");
    switch(t.type()) {
      case Term::VAR: {
        s.add(s.idx.var(Var(t).id()));
        break;
      }
      case Term::FUN: {
        Fun f(t);
        s.add(s.idx.fun(f.fun(),f.arg_count(),""));
        for(size_t i=0; i<f.arg_count(); ++i) term(s,f.arg(i));
        break;
      }
    }
  }

  static void atom(tool::node::OutputStream &s, Atom a) { FRAME("proto_atom()");
    if(!a.sign()) s.add(s.idx.standard(tptp::FORM_NEG));
    if(a.pred()==Atom::EQ) {
      s.add(s.idx.standard(tptp::PRED_EQ));
    } else {
      s.add(s.idx.pred(a.pred(),a.arg_count(),""));
    }
    for(size_t i=0; i<a.arg_count(); ++i) term(s,a.arg(i));
  }

  static void orClause(tool::node::OutputStream &s, OrClause cla) { FRAME("proto_orClause()");
    s.add(s.idx.standard(tptp::FORM_OR));
    s.add(cla.atom_count());
    for(size_t i=0; i<cla.atom_count(); ++i) atom(s,cla.atom(i));
  }

  static solutions::Derivation derAndClause(memory::Alloc &A, tool::node::Index &idx, const DerAndClause &cla, const Valuation &val) { FRAME("proto_derAndClause()");
    solutions::Derivation d;
    d.set_cost(cla.cost());
    auto derived = d.mutable_derived();
    derived->set_name("derived");
    derived->set_role(tptp::Input::PLAIN);
    derived->set_language(tptp::Input::CNF);
    tool::node::OutputStream s(idx);
    orClause(s,ground(A,val.eval(A,cla.derived())).neg());
    derived->mutable_formula()->Add(s.stream.begin(),s.stream.end());
    for(size_t i=0; i<cla.source_count(); ++i) {
      auto ps = d.add_sources();
      auto pg = ps->mutable_ground();
      pg->set_name("ground");
      pg->set_role(tptp::Input::PLAIN);
      pg->set_language(tptp::Input::CNF);
      tool::node::OutputStream gs(idx);
      orClause(gs,ground(A,val.eval(A,cla.source(i))).neg());
      pg->mutable_formula()->Add(gs.stream.begin(),gs.stream.end());
      auto pss = ps->mutable_source();
      tool::node::OutputStream ss(idx);
      pss->set_name("source");
      pss->set_role(tptp::Input::PLAIN);
      pss->set_language(tptp::Input::CNF);
      orClause(ss,cla.source(i).neg());
      pss->mutable_formula()->Add(ss.stream.begin(),ss.stream.end());
    }
    return d;
  }

  static solutions::Proof proof(memory::Alloc &A, tool::node::Index &idx, const OrForm &f, const Valuation &val) { FRAME("proto_Proof");
    solutions::Proof proof;
    size_t i=0;
    for(const auto &cla : f.and_clauses) {
      auto pcla = proof.add_clauses();
      *pcla = derAndClause(A,idx,cla,val);
      pcla->mutable_derived()->set_name(util::fmt("a%",i++));
    }
    proof.mutable_nodes()->CopyFrom(idx.get_nodes());
    return proof;
  }

  static tptp::File orForm(tool::node::Index &idx, const OrForm &f) { FRAME("proto_orForm()");
    tptp::File file;
    size_t i = 0;
    for(const auto &cla : f.and_clauses) {
      auto input = file.add_input();
      input->set_name(util::fmt("a%",i++));
      input->set_role(tptp::Input::PLAIN);
      input->set_language(tptp::Input::CNF);
      tool::node::OutputStream s(idx); 
      orClause(s,cla.derived().neg());
      input->mutable_formula()->Add(s.stream.begin(),s.stream.end());
    }
    file.mutable_nodes()->CopyFrom(idx.get_nodes());
    return file;
  }
};

}  // namespace tableau

#endif  // PARSE_H_

