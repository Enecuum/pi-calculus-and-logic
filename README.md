```
pi-calculus-and-logic$ ghci Catarotoid/SimpleTests01.hs 
GHCi, version 8.4.3: http://www.haskell.org/ghc/  :? for help
[1 of 9] Compiling Catarotoid.MiscTools ( Catarotoid/MiscTools.hs, interpreted )
[2 of 9] Compiling Catarotoid.Pattern ( Catarotoid/Pattern.hs, interpreted )
[3 of 9] Compiling Catarotoid.UniqueShare ( Catarotoid/UniqueShare.hs, interpreted )
[4 of 9] Compiling Catarotoid.Term  ( Catarotoid/Term.hs, interpreted )
[5 of 9] Compiling Catarotoid.Reductions ( Catarotoid/Reductions.hs, interpreted )
[6 of 9] Compiling Catarotoid.Morphisms ( Catarotoid/Morphisms.hs, interpreted )
[7 of 9] Compiling Catarotoid.FocusUp ( Catarotoid/FocusUp.hs, interpreted )
[8 of 9] Compiling Catarotoid.FocusRotation ( Catarotoid/FocusRotation.hs, interpreted )
[9 of 9] Compiling Catarotoid.SimpleTests01 ( Catarotoid/SimpleTests01.hs, interpreted )
Ok, 9 modules loaded.
*Catarotoid.SimpleTests01> rotateFocus $ focup01  test02
[(Atom :w,(INVRT (Atom :q) `CMCNJ` (Atom :r `CMCNJ` Atom :t)) `CMCNJ` INVRT (Share #61)),(Atom :e,Share #61)]
*Catarotoid.SimpleTests01> test02 
[(Atom :q `CMCNJ` (Focus (Atom :w) `CMCNJ` Focus (Atom :e)),Atom :r `CMCNJ` Atom :t)]
*Catarotoid.SimpleTests01>
```
