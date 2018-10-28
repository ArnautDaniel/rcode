#include<stdio.h>
#include<stdlib.h>
#include<fts.h>
#include<string.h>
#include<sys/stat.h>
#include<time.h>
#define LEN 256
int compare (const FTSENT**, const FTSENT**);

char* get_extension(char* node){
  int leng = strlen(node);
  leng -= 4;
  return node+leng;
}

int proper_extension (FTSENT* node, char* k){
  char* extension = get_extension(node->fts_name);
  if (!(strcmp(extension, k))){
    return 1;
  }
  return 0;
}

/* Add a string to the 2d array in memory
mall = the pointer to an array of pointers to char || char**
ft_name is a pointer to a string
size is a pointer to the current size of the mall array
pos is the position in the array currently
*/
void add_to_database(char** mall, char* ft_name, int* size, int* pos){
  if (pos == size){
    *size += 50;
    for (int i = *size - 50; i < *size; i++){
      mall[i] = malloc((LEN+1) * sizeof(char));
    }
  }
  if (strlen(ft_name) <= LEN){
    strcpy( mall[*pos], ft_name);
  } else {
    mall[*pos] = realloc (mall[*pos], strlen(ft_name)+1);
    strcpy( mall[*pos], ft_name);
  }
  *pos += 1;
  return;
}

  
int main (int argc, char* const argv[]){
  if (argc < 3){
    printf("usage: %s *directory* *filter0*|*filtern*\n", argv[0]);
    exit(-1);
  }

  FTS* file_system = fts_open (argv+1, FTS_COMFOLLOW|FTS_NOCHDIR,&compare);
  FTSENT* node = NULL;
  int size_l = 100;
  int* size = &size_l;
  int pos_l = 0;
  int* pos = &pos_l;
  char** store;
  
  store = malloc(100 * sizeof(char*));
  
  for (int i = 0; i < *size; i++){
    store[i] =  malloc((LEN+1) * sizeof(char));
  }
  
  if (file_system != NULL){
    while ((node=fts_read(file_system)) != NULL) {
      switch (node->fts_info) {
      case FTS_D:
	break;
      case FTS_F:
	for (int i = 2; i<argc; i++){
	  if (proper_extension(node, argv[i])){
	    add_to_database(store, node->fts_name, size, pos);
	  }
	}
	break;
      default:
	break;
      }
    }
    fts_close(file_system);
  }
  time_t t;
  srand((unsigned) time(&t));
  int r = rand () % (*size-1);
  char *file_pointer  = store[r];
  
  while(*file_pointer == '\0'){
    r = rand () % (*size-1);
    file_pointer = store[r];
  }
  
  printf("%s\n", file_pointer);

  return 0;
}


int compare(const FTSENT** one, const FTSENT** two) {
  return (strcmp((*one)->fts_name, (*two)->fts_name));
}

