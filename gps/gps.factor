! Copyright (C) 2018 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: kernel sequences accessors sets ;
IN: gps

TUPLE: op action preconds add-list del-list ;

: make-op ( a b c d -- x )
    op boa ;

! Tests if the goal is a part of the current
! add list.
: appropriate? ( goal op -- bool )
    add-list>> member? ;

DEFER: achieve


: apply-op ( state op -- state )
    dup preconds>> [ 3dup rot achieve ] all?
    [ "Executing: " print 
            
: achieve ( ops goal state -- bool )
    [ over swap member? ] [ swap [ ] ] ;

: every-achieve


                                
