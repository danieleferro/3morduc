#ifndef __PNG_LOADER_OPENGL__
#define __PNG_LOADER_OPENGL__

#include <cstdio>
#include <cstdlib>
#include <png.h>
#include <GL/gl.h>

void DrawTexture();
int GetTextureInfo(int ColourType);
GLuint loadImage(const char *filename);

#endif

