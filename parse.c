// read all characters from a file, outputting only the letter 'b's
// it finds in the file

#include <stdio.h>

int main(int argc, char** argv) {
FILE *fp;
int c;
int state = 0;
unsigned int size, len;
unsigned long offset;

  fp = fopen(argv[1], "r"); // error check this!

  while((c = fgetc(fp)) != EOF) {
    switch (state) {
      case 0:  // get command
        switch(c) {
          case 0:  // complete
            printf("END\n");
            fclose(fp);
            return 0;
          case 1:  // erase all
            printf("ERASE ALL\n");
            break;
          case 2:  // erase block
            printf("ERASE BLOCK\n");
            state = 2;  // read sector
            break;
          case 3:  // program
            printf("PROGRAM ");
            state = 1;  // read size
            break;
          default:
            printf("Unknown Command!\n");
            return -1;
        }
        break;
      case 1:  // read size;
        size = c & 0xff;
        state = 3;  // read second byte of size;
        break;
      case 3:  // read size;
        size |= (unsigned int)((c & 0xff) << 8) ;
        printf("%6.6d bytes", size);
        state = 4;  // read first byte of offset;
        break;
      case 4:  // read offset;
        offset = (unsigned long)(c & 0xff) ;
        state = 5;  // read next byte of offset;
        break;
      case 5:  // read offset;
        offset |= (unsigned long)((c & 0xff) << 8) ;
        state = 6;  // read next byte of offset;
        break;
      case 6:  // read offset;
        offset |= (unsigned long)((c & 0xff) << 16) ;
        printf(" at offset $%6.6x\n", offset);
        state = 7; // read data;
        len = 0;
        break;
      case 7:  // read data
        len++;
        if(len == size)
          state = 0;  // start over
        break;
      default:
        printf("error!\n");
        return -1;
    }

  }

  fclose(fp);

  return 0;
}

