
fun square (x : real) = x * x;

fun title (name) = "The Duke of " ^ name;

fun digit i = chr (i + ord #"0");

fun isLower c = #"a" <= c andalso c <= #"z";

fun lengthvec (x,y) = Math.sqrt(x*x + y*y);

type vec = real * real;

fun average (x, y) = (x+y) / 2.0;

fun addvec ((x1,y1) , (x2,y2)) : vec = (x1+x2, y1+y2);

fun subvec (v1,v2) = addvec (v1, negvec v2);

fun distance pairv = lengthvec (subvec pairv);

fun scalevec (r, (x,y)) : vec = (r*x, r*y);
