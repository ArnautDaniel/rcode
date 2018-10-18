/* Looking into writing Crow in C */
#include<stdlib.h>
#include<stdio.h>
#include<sys/types.h>
#include<fts.h>
#include<string.h>
#include<errno.h>
#include<sys/stat.h>

int compare (const FTSENT**, const FTSENT**);
void indent (int i);
int count_files (const FTSENT* children, short parent_depth);

int main (int argc, char* const argv[]) {
  
  FTS* file_system = fts_open (argv + 1, FTS_COMFOLLOW|FTS_NOCHDIR,&compare);
  FTSENT *node = NULL;
  struct stat* sb = NULL;

  if (argc<2) {
    printf("Usage: %s <path-spec>\n", argv[0]);
    exit(255);
  }

  int count = 0;

  if (file_system != NULL) {
    while ((node=fts_read(file_system)) != NULL) {
      switch (node->fts_info) {

      case FTS_D :
	printf ("\n|-- %s - DIR - DEPTH %d - FILES %d --|\n",
		node->fts_name, node->fts_level,
		count_files(fts_children(file_system, 0), node->fts_level));
	break;

      case FTS_F :
	count++;
	sb = node->fts_statp;
	printf("  --> %s - %lld bytes \n", node->fts_name,
	       (long long) sb->st_size);
	break;

      default:
	break;
      }
    }
    fts_close(file_system);
  }
  printf("\n--- %d files found --- \n", count);
  return 0;
}

int compare(const FTSENT** one, const FTSENT** two) {
  return (strcmp((*one)->fts_name, (*two)->fts_name));
}

int count_files (const FTSENT* children, short parent_depth) {
  if (children != NULL) {
    if (children->fts_level > parent_depth+1) {
      return 0 + count_files(children->fts_link, parent_depth);
    }
    return 1 + count_files(children->fts_link, parent_depth);
  }
  return 0;
}

void indent(int i) {
  for (; i > 0; i--) 
    printf("--");
  printf("> ");
}
