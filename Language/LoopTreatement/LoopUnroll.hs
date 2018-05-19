module Language.LoopTreatement.LoopUnroll where

import Language.While.Types

data UnwindAnnotation = AssumeAnn | AssertAnn
     deriving Show

loop_unroll :: UnwindAnnotation -> Int -> Stm -> Stm
loop_unroll a _ s@(Sass v x)      = s
loop_unroll a _ s@Sskip           = s
loop_unroll a _ s@(Sassume e)     = s
loop_unroll a _ s@(Sassert e)     = s
loop_unroll a k (Scomp s1 s2)     = Scomp (loop_unroll a k s1) (loop_unroll a k s2)
loop_unroll a k (Sif b st sf)     = Sif b (loop_unroll a k st) (loop_unroll a k sf)
loop_unroll a k (Swhile b s)      = let s' = loop_unroll a k s
                                    in unroll a k b s'
loop_unroll a k (SwhileInv b i s) = let assrt = Sassert i
                                        s' = loop_unroll a k s
                                        s'' = Scomp s' assrt
                                    in Scomp assrt (unroll a k b s'')
loop_unroll a k (Stry s1 s2)      = Stry (loop_unroll a k s1) (loop_unroll a k s2)

unroll :: UnwindAnnotation -> Int -> Bexp -> Stm -> Stm
unroll AssumeAnn 0 b _ = Sassume $ Bneg b
unroll AssertAnn 0 b _ = Sassert $ Bneg b
unroll a k b s = Sif b (Scomp s $ unroll a (k - 1) b s) Sskip
