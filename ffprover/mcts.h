struct Tree {
private:
  struct Node { 
    Node *parent;
    double priority;
    double rewards; // W = wins
    size_t visits; // N = visit count
    memory::Maybe<size_t> win_depth;

    memory::Maybe<memory::Array<Node>> children;
    size_t children_lost;
  };
  memory::Allocator A;
  Node *root;
  Tree() : root(A.alloc<Node>()) { *root = Node{}; }
public:
  struct Ptr { 
    void visit(double reward) {
      for(Node *x = node; x; x = x->parent) {
        x->visits++;
        x->rewards += reward;
      }
    }

    INL Ptr child(size_t i) { FRAME("child(%)",i);
      DEBUG if(!node->expanded) error("unexpanded node");
      return Ptr{.tree=tree, .node=node->children[i] };
    }

    INL bool expanded(){ return node->children; }

    void expand(const vec<double> &priorities) {
      DEBUG if(node->expanded) error("re-expanding a node");
      node->expanded = true;
      node->children = memory::Array<Node>(tree->A,priorites.size());
      double total = 0; for(auto p : priorities) total += p;
      for(size_t i=0; i<priorities.size(); i++)
        node->children[i] = Node{.parent=node,.priorities=priorities[i]/total};
      // if child_count is 0, then this is a lost branch.
      for(Node *x = node; x->parent && x->children_lost==x->children.size(); x = x->parent) {
        x->parent->children_lost++;
      }
    }

    INL bool lost() {
      return node->expanded && node->children.size()==node->children_lost;
    }
  private:
    Tree *tree;
    Node *node;
  };

  INL static ptr<Tree> New() { return own(new Tree()); }
  INL Ptr root() { return {.tree=this,.root=root }; }
};


  double ucb(double sum_visits) {
    double visits = max(1.0,tree_visits);
    double sum_visits = max(1.0,sum_visits);
    if(do_ucb) {
      switch(ucb_mode) {
      case 1: factor = sqrt(sum_visits/visits); break; // UCB no logarithm *)
      case 2: factor = sqrt(sum_visits)/visits; break; // PUCB from Alpha Zero *)
      default: factor = sqrt(log(sum_visits)/visits); break; // Original Csaba Szepesvari *)
      }
      return (tree_reward/visits) + ucb_const*prior*factor;
    }
    return Random.float(1.) * prior;;
  }

  size_t select_branch() {
    size_t max_i = 0;
    double max_u = -1;
    for(size_t i=0; i<branches.size(); i++) {
      if(branches[i].kind==Lost) continue;
      auto u = branches[i].ucb(visits);
      if(u>max_u){ max_u = u; max_i = i; }
    }
    return max_i;
  }

