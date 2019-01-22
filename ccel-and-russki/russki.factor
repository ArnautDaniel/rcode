! Copyright (C) 2019 Jack Lucas
! See http://factorcode.org/license.txt for BSD license.
USING: html.parser http.client kernel sequences html.parser.analyzer continuations prettyprint io io.pathnames io.directories namespaces io.files.info io.encodings.utf8 io.files io.encodings.8-bit  ;
IN: russki

: get-title ( tag -- tag' )
    "title" attribute ;
: get-href ( tag -- tag' )
    "href" attribute ;

: russki-make-directory ( -- )
    current-directory get "/russki-books" append exists?
    [  ]
    [ "russki-books" make-directory ] if ;

: russki-get ( url -- response )
    http-get  parse-html swap drop "a" find-html-objects  ;

: only-djvu-links ( response -- response' )
    [ first "href" attribute file-extension "djvu" equal? ] filter
    [ first get-href 0 2 rot subseq ".." equal? ] reject ;

: find-russian-hyperlinks ( response -- response' )
    [ first "title" attribute? ] filter ;

: russki-single-print ( tag -- )
    dup get-title swap get-href
    write "    -    " write write flush ;

: and-print-them ( response -- )
    [ russki-single-print ] each ;

: make-proper-url ( url -- url' )
    dup 0 4 rot subseq "http" equal?
    [ ] [ "khazarzar.skeptik.net/books/" swap append ] if ;

: get-info ( tag -- title url )
    dup get-title ".djvu" append swap get-href ;

: download-an-item ( title url -- )
    make-proper-url
    swap download-to ;

: download-them-all ( response -- )
    [ first dup get-info rot russki-single-print
      download-an-item " -- OK!" print flush ] each ;
    
: russki-print ( -- )
    russki-make-directory
    current-directory get "/russki-books" append
    set-current-directory
    "khazarzar.skeptik.net/books"
    russki-get find-russian-hyperlinks only-djvu-links
    download-them-all ;
    
MAIN: russki-print
