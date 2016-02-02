While language parser
=====================

Parser for the While language described in 
Extended with integer division and try-catch clauses.

Useful for the laborations in the KTH course DD2457 

Available on hackage. Written by me and Patrik Berggren.


INSTALL: $ cabal install

While-language parser: $ while-lang-parser file_name.wl

SA translation to SA language with for statements: $ transSAFor file_name.wl

SA translation to SA language based on Boogie/Havoc: $ transSAHavoc file_name.wl

Loop UNWIND: $ unwind file.wl annotation bound

annotation  = assume | assert
bound       = int

VCGENs: $ vcgen file.wl VCGen

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