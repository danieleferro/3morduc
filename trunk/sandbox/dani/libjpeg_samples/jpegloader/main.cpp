#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>
#include <stdio.h>
#include <jpeglib.h>

GLOBAL(int)
read_JPEG_file (char * filename, unsigned int * textureID);

unsigned int uiv1Texture[1];	

void draw(void)
{
  double a=.5, z=0.;

  glClear(GL_COLOR_BUFFER_BIT);

  glBegin(GL_QUADS);
  glTexCoord2f(0., 0.);
  glVertex3f(-a, a, z);
  glTexCoord2f(0., 1.);
  glVertex3f(-a, -a, z);
  glTexCoord2f(1., 1.);
  glVertex3f(a, -a, z);
  glTexCoord2f(1., 0.);
  glVertex3f(a, a, z);
  glEnd();

  glFlush();
}

int main(int argc, char **argv)
{
  glutInitWindowSize(800,600);
  glutInitWindowPosition(200,200);
  glutInitDisplayMode(GLUT_RGB | GLUT_SINGLE);
  glutInit(&argc,argv);
  glutCreateWindow("JPEG TEXTURE");

  glutDisplayFunc(draw);

  glMatrixMode(GL_PROJECTION);
  gluPerspective(45.,4./3.,1.,20.);
  gluLookAt(.0,.0,2., .0,.0,.0, .0,1.,.0);
  glMatrixMode(GL_MODELVIEW);

  glEnable(GL_CULL_FACE);
  glDisable(GL_DEPTH_TEST);

  glEnable(GL_TEXTURE_2D);
  read_JPEG_file("texture/image1.jpg", uiv1Texture);
  glBindTexture(GL_TEXTURE_2D, uiv1Texture[0]);
  glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR_MIPMAP_LINEAR);

  glutMainLoop();

  return 0;
}
