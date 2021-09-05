#ifndef FFPROVER_TREE_H_
#define FFPROVER_TREE_H_

#include "lazyparam_prover/memory/stack.h"
#include "lazyparam_prover/memory/maybe.h"
#include "lazyparam_prover/memory/array.h"

namespace ff {

struct Tree {
private:
  struct Node { 
    Node *parent;
    double priority;
    double rewards; 
    size_t visits;

    memory::Maybe<size_t> won_depth;
    size_t children_lost;
    memory::Maybe<memory::Array<Node>> children;
  };
  memory::Alloc A;
  Node *root_;
  Tree() : root_(A.alloc_init<Node>({})) {}
public:
  struct Ptr { 
    INL void visit(double reward) {
      //info("visit(%)",reward);
      for(Node *x = node; x; x = x->parent) {
        x->visits++;
        x->rewards += reward;
      }
    }

    INL size_t visits() const { return node->visits; }
    INL double rewards() const { return node->rewards; }
    INL double priority() const { return node->priority; }

    INL size_t child_count() const { FRAME("child_count");
      DEBUG if(!node->children) error("unexpanded node");
      return node->children.get().size();
    }

    INL Ptr child(size_t i) const { FRAME("child(%)",i);
      DEBUG if(!node->children) error("unexpanded node");
      return Ptr(tree,&node->children.get()[i]);
    }

    INL bool has_parent() const { return node->parent; }
    INL Ptr parent() const {
      DEBUG if(!node->parent) error("root has no parent");
      return Ptr(tree,node->parent);
    }

    INL bool expanded() const { return bool(node->children); }

    INL void expand(const vec<double> &priorities) { FRAME("expand");
      DEBUG if(node->children) error("re-expanding a node");
      node->children = memory::just(memory::Array<Node>(tree->A,priorities.size()));
      double total = 0; for(auto p : priorities){
        DEBUG if(p<0) error("negative priority");
        total += p;
      }
      for(size_t i=0; i<priorities.size(); i++)
        node->children.get()[i] = Node{.parent=node,.priority=priorities[i]/total};
      // if child_count is 0, then this is a lost branch.
      for(Node *x = node; x->parent && x->children_lost==x->children.get().size(); x = x->parent) {
        x->parent->children_lost++;
      }
    }

    INL bool lost() const {
      return !bool(node->won_depth)
        && bool(node->children)
        && node->children.get().size()==node->children_lost;
    }

    INL void set_won() {
      size_t depth = 0;
      if(!tree->root().node->won_depth) info("WON");
      for(auto *x = node; x && (!bool(x->won_depth) || x->won_depth.get()>depth); x = x->parent)
        x->won_depth = memory::just(depth++);
    }

    INL memory::Maybe<size_t> won_depth() const { return node->won_depth; }

    INL size_t child_id(Ptr t) const {
      DEBUG if(!node->children) error("unexpanded node");
      for(size_t i=node->children.get().size(); i--;) 
        if(child(i).node==t.node) return i;
      error("not a valid child");
    }
  private:
    friend struct Tree;
    Ptr(Tree *_tree, Node *_node) : tree(_tree), node(_node) {}
    Tree *tree;
    Node *node;
  };

  INL static ptr<Tree> New() { return own(new Tree()); }
  INL Ptr root() { return Ptr(this,root_); }
};

}  // namespace ff

#endif  // FFPROVER_TREE_H_
