OVERVIEW

=====================


Utilities for a standard while language. It includes:

- a parser for standard while programs annotated with assume and
  assert statements and optionally with loop invariants (adapted from
  https://github.com/davnils/while-lang-parser);

- loop unwinding based on bounded model checking;

- multiple transformations into single-assignment form:
    
    - based on the boogie/havoc method, in which loops are removed by
      inserting assume and assert statements (see [1] for more details)

    - loops are converted into special for loops (see [2,3] for more
      details)

- multiple methods for generating verification conditions (VCs) (see
  [4] for more details)

- interaction with Why3 for discharging VCs


See documentation/images/tool_workfow.pdf for a full overview of the
tool.


[1] Michael Barnett, K. Rustan M. Leino: Weakest-precondition of
    unstructured programs. PASTE 2005: 82-87

[2] Cláudio Belo Lourençoo, Maria João Frade, and Jorge Sousa Pinto.
    Formalizing single-assignment program verification: An adaptation-
    complete approach. In Proceedings of the 25th European Symposium
    on Programming, volume 9632 of LNCS, 2016.

[3] Cláudio Belo Lourenço, Maria João Frade, Jorge Sousa Pinto: A
    Single-Assignment Translation for Annotated Programs. CoRR
    abs/1601.00584 (2016)

[4] Cláudio Belo Lourenço, Si-Mohamed Lamraoui, Shin Nakajima, and
    Jorge Sousa Pinto. Studying verification conditions for imperative
    programs. ECEASST, 72, 2015.


INSTALLATION

=====================

1. git clone git@bitbucket.org:belolourenco/while-lang.git

2. cd while-lang

3. cabal install


Multiples executables are going to be installed:

- while-lang-parser: parse a while program and pretty print it

- unwind: simply unwind loops

- transSAFor: translation based on [3]

- transSAHavoc: translation based on [1]

- while-lang-vcgen: performs the complete workflow - unwind loops
  (optionally), translate the program into SA, generate verification
  conditions, pretty print or discharge VCs with Why3


VCGens

=====================

The following VCGens are available

VCGen = psp       : Strongest Postcondition with PARTIAL contexts (assert NOT in context)

      | pspplus   : Strongest Postcondition with PARTIAL contexts (assert in context)

      | gsp       : Strongest Postcondition with GLOBAL contexts  (assert NOT in context

      | gspplus   : Strongest Postcondition with GLOBAL contexts  (assert in context)

      | pcnf      : Conditional Normal Form with PARTIAL contexts (assert NOT in context)

      | pcnfplus  : Conditional Normal Form with PARTIAL contexts (assert in context)

      | gcnf      : Conditional Normal Form with GLOBAL contexts  (assert NOT in context

      | gcnfplus  : Conditional Normal Form with GLOBAL contexts  (assert in context)

      | plin      : Linear with PARTIAL contexts                  (assert NOT in context)

      | plinplus  : Linear with PARTIAL contexts                  (assert in context)

      | glin      : Linear with GLOBAL contexts                   (assert NOT in context

      | glinplus  : Linear with GLOBAL contexts                   (assert in context)


USAGE EXAMPLES

=====================

While-language parser: $ while-lang-parser file_name.wl

SA translation to SA language with for statements: $ transSAFor file_name.wl

SA translation to SA language based on Boogie/Havoc: $ transSAHavoc file_name.wl

Loop UNWIND: $ unwind file.wl annotation bound

annotation  = assume | assert
bound       = int

VCGENs: $ vcgen file.wl VCGen
