While language parser
=====================

Parser for the While language described in "Semantics with Applications" by Nielson and Nielson.
Extended with integer division and try-catch clauses.

Useful for the laborations in the KTH course DD2457 "Program Semantics and Analysis".

Available on hackage. Written by me and Patrik Berggren.


INSTALL: $ cabal install

RUN While-language parser: $ while-lang-parser file_name.wl

RUN SA translation to SA language with for statements: $ transSAFor file_name.wl

RUN SA translation to SA language based on Boogie/Havoc: $ transSAHavoc file_name.wl