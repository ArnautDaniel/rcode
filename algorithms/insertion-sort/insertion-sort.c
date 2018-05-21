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
