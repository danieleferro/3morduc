#ifndef __OPENGL
#define __OPENGL

#include <GL/glut.h>    // Header File For The GLUT Library 
#include <GL/gl.h>	// Header File For The OpenGL32 Library
#include <GL/glu.h>	// Header File For The GLu32 Library
#include <stdio.h>      // Header file for standard file i/o.
#include <stdlib.h>     // Header file for malloc/free.

#endif

#ifndef __IMAGE_LOADER
#define __IMAGE_LOADER

/* Image type - contains height, width, and data */
struct Image {
  unsigned long sizeX;
  unsigned long sizeY;
  char *data;
};

typedef struct Image Image;

int ImageLoad(char * filename, Image * image);
void LoadGLTextures(GLuint * texture);

#endif
