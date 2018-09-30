module PiCalculus.TranslationToCatarotoid where


{-



a<g>

a1^-1 . sec:b . sec:c . sec:d . sec:e . a2^-1 .
  (@ u)
  (   ! u . ( ! sec:b^-1 . g1    . g2    . sec:c^-1 ) . u^-1
    * ! u . ( ! sec:d^-1 . g1^-1 . g2^-1 . sec:e^-1 ) . u^-1
    * u^-1
    * u
    * p
  )

a(h)

a1 . sec:$b^-1 . sec:$c^-1 . sec:$d^-1 . sec:$e^-1 . a2 .
  (@ u)
  (   ! u . ( ! h1^-1 . sec:$b . sec:$c . h2^-1 ) . u^-1
    * ! u . ( ! h1    . sec:$d . sec:$e . h2    ) . u^-1
    * u^-1
    * u
    * q
  )



( ! a . b ) * ( ! c . b ) * b^-1





























a1 . sec:$b^-1 . sec:$c^-1 . sec:$d^-1 . sec:$e^-1 . a2 .
  (   ! u . ( v^-1 * ! v^-1 ) . ( ! v . h1^-1 . sec:$b . sec:$c . h2^-1 ) . u^-1
    * ! u . ( v^-1 * ! v^-1 ) . ( ! v . h1    . sec:$d . sec:$e . h2    ) . u^-1
    * u^-1
    * q . u
  )



a1 . sec:$b^-1 . sec:$c^-1 . sec:$d^-1 . sec:$e^-1 . a2 .
  (   ! h1^-1 . sec:$b . sec:$c . h2^-1
    * ! h1    . sec:$d . sec:$e . h2
    * q
  )


sec:a1^-1 . b1^-1 . ( body * p ) . b2^-1 . sec:a2^-1
body = ! sec:a1    . c1    . c2    . sec:a2
     * ! sec:a1^-1 . c1^-1 . c2^-1 . sec:a2^-1

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


