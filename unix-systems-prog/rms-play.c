#include<stdio.h>
#include<stdlib.h>
#include<fts.h>
#include<string.h>
#include<sys/stat.h>
#include <sys/wait.h>
#include <errno.h>
#include <unistd.h>
#include <time.h>
#define LEN 1024
#define SLEEP_LEN 1

int calculate_seek_of (int** index, int r){
  int result = 0;
  if (r == 2)
    return 0;
  for (int i = 2; i<r; i++){
    result = result +  *index[i] + 1;
  }
  return result;
}
  
int get_count(FILE* file){
  int count = 0;
  for (char c = fgetc (file); c != EOF; c = fgetc(file)){
    if (c == '\n')
      count += 1;
  }
  return count;
}

void index_count (FILE* file, int ** index){
  
  if (*index[1] == 0){
    return;
  }
  
  int indexer = 0;
  for (char c = fgetc(file); c != '\n'; c = fgetc(file)){
    indexer += 1;
    printf("%c", c);
  }
  
  int i = (2 + *index[0]) - *index[1]; // Thats just bad
  *index[i] = indexer;
  (*index[1])--;
  
  return index_count(file, index);
}

char* read_the_line(FILE* file, int count_down){
  
  char* pp = (char*) malloc (sizeof (char) * count_down);
  
  for (int i = 0; i < count_down; i++){
    char temp = fgetc(file);
    *(pp + i) = temp;
  }
  
  pp[count_down] = '\0';
  
  return pp;
}

char* read_random_line(FILE* file, int** index){
  
  time_t t;
  srand((unsigned) time(&t));
  int r = 0;
  
  while (r <= 1){
      r = rand () % *index[1];
  }
  
  int count_down = *index[r];
  sleep(SLEEP_LEN);
  
  int seek_position = calculate_seek_of(index, r);
  fseek(file, seek_position, 0);
  
  return read_the_line(file, count_down+1);
}
  
  
void play_til_zero (FILE* file, int** index){
   if (*index[0] == 0) {
    printf("end of music files!");
    return;
  }
   char* k = read_random_line(file, index);
   char* b = "mpv ";

   int xm = strlen(b) + strlen(k) + 1;
   char* buf = malloc(sizeof(char) * xm);
   
   for (int i = 0; i < xm; i++){
     buf[i] = '\0';
   }
   strcpy(buf, b);
   strcat(buf, k);
   system(buf);
   
   free(k);
   free(buf);
   *index[0] -= 1;
   
   return play_til_zero(file, index);
}

void play_random(FILE* file){
  int count = get_count (file);
  rewind(file);

  int** index = malloc((2 + count) * sizeof(int*));
  
  for (int i = 0; i < count+2; i++){
    index[i] = malloc(sizeof(int));
  }
  
  *index[0] = count;
  *index[1] = count;
  index_count(file, index);
  *index[1] = *index[0];
  play_til_zero(file,  index);
  
  return;
  
}

int main (int argc, char* const argv[]){

  if (argc != 2) {
    printf ("usage: %s *directory*\n", argv[0]);
    exit(-1);
  }

  char* command =  "./rms-get ";
  char* directives = " .mp3 .flac .ogg .wav > .rms-file";
  
  int x = strlen(argv[1]) + strlen(command) + strlen(directives);
  char buff[x+1];
  char* buffp = buff;

  strcpy(buffp, command);
  strcat(buffp, argv[1]);
  strcat(buffp, directives);
  
  int status;
  if ((status = system(buffp)) < 0) {
    exit(status);
  }

  FILE* file;
  file = fopen(".rms-file", "r");
  if (file == NULL){
    printf("NULL FILE!\n");
    exit(-1);
  }

  play_random(file);

  printf("\n\nThanks for playing!\n");
  return 0;
}
