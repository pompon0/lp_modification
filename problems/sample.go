package problems

var Trivial = []byte("fof(trivial, conjecture, ($true)).")
var Simple = []byte("fof(simple, conjecture, (?[X]:(![Y]: (p(X) => p(Y))))).")

var EqAxiom1 = []byte("fof(eqAxiom1, conjecture, ((a1=a2) & (b1=b2) & (c1=c2)) => (f(a1,b1,c1)=f(a2,b2,c2))).")
var EqAxiom2 = []byte("fof(eqAxiom2, conjecture, (a1=a2) => (f(a1,b1,c1)=f(a2,b1,c1))).")
var EqAxiom3 = []byte("fof(eqAxiom3, conjecture, ((a=b) & (b=c) & (c=d)) => (a=d)).")
var EqAxiom4 = []byte("fof(eqAxiom4, conjecture, ((a=b)  => (b=a))).")

var Barber = []byte("fof(barber, conjecture, ~(?[B]:(![X]:(shaves(B,X) <=> ~shaves(X,X))))).")

var Pelletier20 = []byte(`
fof(a1, axiom, (![X]: (![Y]: (?[Z]: (![W]: ((p(X) & q(Y)) => (r(Z) & u(W)))))))).
fof(c, conjecture, (?[X]: (?[Y]: ((p(X) & q(Y)) => (?[Z]: r(Z)))))).
`)

var Pelletier24 = []byte(`
fof(a1, axiom, ~(?[X] : (u(X) & q(X)))).
fof(a2, axiom, (![X] : (p(X) => (q(X) | r(X))))).
fof(a3, axiom, ~(?[X] : (p(X) => (?[X]: q(X))))).
fof(a4, axiom, (![X] : ((q(X) & r(X)) => u(X)))).
fof(c, conjecture, (?[X] : (p(X) & r(X)))).
`)

var L143_zfmisc_1 = []byte(`
fof(l143_zfmisc_1, conjecture,  (! [A] :  (! [B] :  (! [C] :  ~ ( ( ~ (r2_hidden(A, C))  &  ( ~ (r2_hidden(B, C))  &  ~ (r1_xboole_0(k2_tarski(A, B), C)) ) ) ) ) ) ) ).
fof(t51_zfmisc_1, axiom,  (! [A] :  (! [B] :  (! [C] :  ~ ( ( ~ (r2_hidden(A, B))  &  ( ~ (r2_hidden(C, B))  &  ~ (r1_xboole_0(k2_tarski(A, C), B)) ) ) ) ) ) ) ).
`)

var L40_tex_2_reduced = []byte(`
fof(a0, axiom, (d=c)).
fof(a1, axiom, (~q)).
fof(a2, axiom, (d=c <=> q)).
`)

var SampleProblems = map[string][]byte {
  "trivial": Trivial,
  "simple": Simple,
  "eqAxiom1": EqAxiom1,
  "eqAxiom2": EqAxiom2,
  "eqAxiom3": EqAxiom3,
  "eqAxiom4": EqAxiom4,
  "barber": Barber,
  "pelletier20": Pelletier20,
  "pelletier24": Pelletier24,
  "l143_zfmics_1": L143_zfmisc_1,
  "l40_tex_2_reduced": L40_tex_2_reduced,
}
