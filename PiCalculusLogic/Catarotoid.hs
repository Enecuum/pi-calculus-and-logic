module PiCalculusLogic.Catarotoid where


{-


a<g>

( ( a2^-1 . a1^-1 ) * sec:b^-1 )

( (@ u)
  (   ! u . ( ! r1^-1 . ( sec:b * ( g1    . g2    ) ) . r2^-1 ) . u^-1
    * ! u . ( ! s1^-1 . ( sec:b * ( g2^-1 . g1^-1 ) ) . s2^-1 ) . u^-1
    * u^-1
    * u
  )
)

a(h)

( ( a1 . a2 ) * sec:$b )

( (@ u)
  (   ! u . ( ! h1^-1 . r1 . sec:$b^-1 . r2 . h2^-1 ) . u^-1
    * ! u . ( ! h2    . s1 . sec:$b^-1 . s2 . h1    ) . u^-1
    * u^-1
    * u
  )
)




disjunction

(@ b)
(   ! a * b
  * ! c * b
  * ! d * b
  . b^-1
)


(@ b )
! ( ( ! a * b ) * ( ! c * b ) )

(@ b)
( ( ( ! r * b ) * ( ! u * b ) ) * t )

( ( ! ( r * t * b ) * ( ! u * b ) )






   




a<g>

( a1^-1 . sec:b^-1 . a2^-1 ) in formula

( (@ u)
  (   ! u . ( ! r1^-1 . sec:b . g1    . g2    . r2^-1 ) . u^-1
    * ! u . ( ! s1^-1 . sec:b . g1^-1 . g2^-1 . s2^-1 ) . u^-1
    * u^-1
    * u
  )
) in global multiplication

a(h)

( a2 . sec:$b . a1 ) in formula

( (@ u)
  (   ! u . ( ! h1^-1 . r1 . sec:$b^-1 . r2 . h2^-1 ) . u^-1
    * ! u . ( ! h1    . s1 . sec:$b^-1 . s2 . h2    ) . u^-1
    * u^-1
    * u
  )
) in global multiplication
   



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


