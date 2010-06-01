#ifndef __OPENGL
#define __OPENGL

#include <GL/glut.h>    // Header File For The GLUT Library 
#include <GL/gl.h>	// Header File For The OpenGL32 Library
#include <GL/glu.h>	// Header File For The GLu32 Library

#endif

#ifndef __STD_LIB_INCLUDE
#define __STD_LIB_INCLUDE

#include <stdio.h>      // Header file for standard file i/o.
#include <stdlib.h>     // Header file for malloc/free.

#endif

#ifndef __TEXTURE_HANDLER
#define __TEXTURE_HANDLER

#include <string>


/* Image type - contains height, width, and data */
struct Image {
  unsigned long sizeX;
  unsigned long sizeY;
  char *data;
};

typedef struct Image Image;

void DrawTexture();

/* textures manager functions */
int ImageLoad(std::string filename, Image * image);
void LoadGLTextures(GLuint * texture, std::string filename);


#endif