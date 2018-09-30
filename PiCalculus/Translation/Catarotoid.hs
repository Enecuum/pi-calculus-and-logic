module PiCalculus.Translation.Catarotoid where


{-


sec:a1^-1 . b1^-1 . ( body * p ) . b2^-1 . sec:a2^-1
body = ! sec:$n1    . c1    . c2    . sec:$n2
     * ! sec:$n1^-1 . c1^-1 . c2^-1 . sec:$n2^-1

sec:$n1   . b1    . ( body * q ) . b2    . sec:$n2
body = ! k1    . sec:$n1    . sec:$n2    . k2
     * ! k1^-1 . sec:$n1^-1 . sec:$n2^-1 . k2^-1






sec:a^-1 . b^-1 . ( * p ) . b . sec:a .

sec:$n . b . ( body * q ) . b^-1 . sec:$n^-1 .
 body = k . sec:$n . sec:$n^-1 . k^-1



a<d> . p

sec:c . a^-1 . sec:c^-1 .
     ! ( b . ( sec:c * d    ) . e )
   * ! ( g . ( sec:c * d^-1 ) . h )
   * p


a(k) . q

sec:$n . a . sec:$n^-1 .
    ! k    . b^-1 . sec:$n^-1 . e^-1
  * ! k^-1 . g^-1 . sec:$n^-1 . h^-1
  * q


  * ! ( ! f . k    . b^-1 . sec:$n^-1 . e^-1 . f^-1
      * ! f . k^-1 . g^-1 . sec:$n^-1 . h^-1 . f^-1
      )
  * f^-1
  * f



-}


