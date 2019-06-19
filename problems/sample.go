package problems

var trivial = []byte("fof(trivial, conjecture, ($true)).")
var simple = []byte("fof(simple, conjecture, (?[X]:(![Y]: (p(X) => p(Y))))).")

var eqAxiom1 = []byte("fof(eqAxiom1, conjecture, ((a1=a2) & (b1=b2) & (c1=c2)) => (f(a1,b1,c1)=f(a2,b2,c2))).")
var eqAxiom2 = []byte("fof(eqAxiom2, conjecture, (a1=a2) => (f(a1,b1,c1)=f(a2,b1,c1))).")
var eqAxiom3 = []byte("fof(eqAxiom3, conjecture, ((a=b) & (b=c) & (c=d)) => (a=d)).")
var eqAxiom4 = []byte("fof(eqAxiom4, conjecture, ((a=b)  => (b=a))).")

var barber = []byte("fof(simple, conjecture, ~(?[B]:(![X]:(shaves(B,X) <=> ~shaves(X,X))))).")

var pelletier20 = []byte(`
fof(a1, axiom, (![X]: (![Y]: (?[Z]: (![W]: ((p(X) & q(Y)) => (r(Z) & u(W)))))))).
fof(c, conjecture, (?[X]: (?[Y]: ((p(X) & q(Y)) => (?[Z]: r(Z)))))).
`)

var pelletier24 = []byte(`
fof(a1, axiom, ~(?[X] : (u(X) & q(X)))).
fof(a2, axiom, (![X] : (p(X) => (q(X) | r(X))))).
fof(a3, axiom, ~(?[X] : (p(X) => (?[X]: q(X))))).
fof(a4, axiom, (![X] : ((q(X) & r(X)) => u(X)))).
fof(c, conjecture, (?[X] : (p(X) & r(X)))).
`)

var SampleProblems = map[string][]byte {
  "trivial": trivial,
  "simple": simple,
  "eqAxiom1": eqAxiom1,
  "eqAxiom2": eqAxiom2,
  "eqAxiom3": eqAxiom3,
  "barber": barber,
  "pelletier20": pelletier20,
  "pelletier24": pelletier24,
}
