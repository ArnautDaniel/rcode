// Copyright (C) 2018 Jack Lucas
// Insertion Sort in C;  Iterative version

#include <stdio.h>

void insertion_sort (int* arr, int len);

int main () {

  printf("\nCalling insertion sort\n");

  int array[6] = {5,2,4,6,1,3};

  int len = sizeof array / sizeof array[0];

  insertion_sort(array, len);

  printf ("\nResult starting at 5,2,4,6,1,3\n");
  for (int i = 0; i < len; i++) {
    printf("%d " , array[i]);
  }

  return 0;
}

void insertion_sort (int* arr, int len){
  for (int j = 1; j < len; j++){
    int key = arr[j];
    int i = j - 1;

    while (i >= 0 && arr[i] > key){
      arr[i+1] = arr[i];
      i = i - 1;
    }
    arr[i+1] = key;
  }
  return;
}

/* It feels pretty gross having to deal with side effects like this.
   Just calling insertion_sort on an array and suddenly it's changed?
   I'm sure the speed is much greater than my destructive sort in Scheme
   though.

   It's almost like every language is just a long list of design choices
   between being explict or implicit, and I can't get over the fact that
   C is so damn implicit when it comes to side-effects.  Scheme's method of
   appending a "!" to destructive functions is a much safer feeling.

   Now that I've finished the sort in both Scheme and C we turn to the
   horror of thinking about how to do this in Forth.

   I'm fair sure it's masochistic to use 3 languages in different *-fix's.
   Post-fix, pre-fix and in-fix.  I'm an addict for crazy mind-bending and
   this is the only way I get my fix.

*/

