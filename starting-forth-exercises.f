: card 20 > if ." You get alcohol! " else ." You don't get any alcohol! " then ;

: sign.test
    dup 0 > if ." Positive " else 0 < if ." Negative " then then ;

: difference - abs ;

: commission 10 / 50 min ;

: 3swap >r swap r> ;

: decade 10 0 do i . loop ;

: r% 10 */ 5 + 10 / ;
: compound
    swap 21 1 do ." year " i . 3 spaces
	2dup R% + dup ." balance " . cr loop 2drop ;

: rectangle 256 0 do i 16 mod 0= if
	    cr then ." *" loop ;

: fizzbuzz ( n x -- )
     do	i 15 mod 0= if ." FizzBuzz " cr else
	    i 5 mod 0= if ." Fizz " cr else
		i 3 mod 0= if ." Buzz " cr else i . cr
		then then then loop ;

: normal.fizzbuzz
    101 0 fizzbuzz ;

