! Copyright (C) 2018 Jack Lucas
! See http://factorcode.org/license.txt for BSD license.
! These answers will probably take some time to turn
! into greek statues, but lumps of clay work for now.

USING: kernel math sequences lists ;
IN: 99-problems

: 99-last ( lst -- elt )
    dup cdr nil? [ car ] [ cdr 99-last ] if ;

: 99-pair-last? ( lst -- bool )
    dup cdr cdr nil? [ drop t ] [ drop f ] if ;

! Needs to be updated to error on too small of
! a list.
: 99-but-last ( lst -- elt )
    dup llength 2 >= [
        dup 99-pair-last? [ car ] [ cdr 99-but-last ] if
    ]
    [ ] if ;

