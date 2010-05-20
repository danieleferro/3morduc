
/* Copyright (c) Mark J. Kilgard, 1994. */

/* This program is freely distributable without licensing fees 
   and is provided without guarantee or warrantee expressed or 
   implied. This program is -not- in the public domain. */

#include <stdlib.h>
#include <stdio.h>
#include <GL/glut.h>
#include "gl_log.h"
#include "interlace_stencil.h"

#define glImageLenght  1442400 /* 800x601x3 , 600+1 = to be sure we have some reserves */

// Microsoft OpenGL implmentation dos not support AUX buffers
static GLubyte glEmulAUX[glImageLenght]; // field for emulation of AUX buffer
static GLint gliWindowWidth,gliWindowHeight;
static int bStencilGenerated=0;
// for debuging
char	szBuffer[256];

void
display(void)
{
	
	GLint gliStencilBits;
// diagnostic
	GLenum errCode;
	const GLubyte *errString;

	if ((errCode = glGetError()) != GL_NO_ERROR)
	{
		errString = gluErrorString (errCode);
		sprintf(szBuffer,"OpenGL Error: %s\n",errString); 
		GLLogMsg(szBuffer);
	}
	else
	{
		GLLogMsg("No OpenGL error\n");
	}

	glGetIntegerv(GL_STENCIL_BITS,&gliStencilBits);
  	sprintf(szBuffer,"Number of stencil buffer bits %d\n",gliStencilBits); 
	GLLogMsg(szBuffer);
	gliWindowWidth=glutGet(GLUT_WINDOW_WIDTH);
	gliWindowHeight=glutGet(GLUT_WINDOW_HEIGHT);
	sprintf(szBuffer,"Window width, height %d %d\n",gliWindowWidth,gliWindowHeight); 
	GLLogMsg(szBuffer);
// generating the pattern in stencil buffer
	if(bStencilGenerated==0)
	{
		interlace_stencil(gliWindowWidth,gliWindowHeight);
		bStencilGenerated=1;
	}
	
//	glDrawBuffer(GL_BACK_LEFT);
  // following comand replace glDrawBuffer(GL_BACK_LEFT);
  glStencilFunc(GL_NOTEQUAL,1,1); // draws if stencil <> 1
 
	glDrawBuffer(GL_BACK);
	glClearColor(0.0, 0.0, 0.0, 1.0); /* black */
// the clearing trick des not work becouse clearing does not 
// care about stencil buffer content
	glClear(GL_COLOR_BUFFER_BIT); 
// we must realy draw something
	glColor4f(1.0, 0.0, 0.0, 1.0); /* red */
	glBegin(GL_QUADS);
		glVertex2f(100,100);
		glVertex2f(100,300);
		glVertex2f(300,300);
		glVertex2f(300,100);
	glEnd();

//	glDrawBuffer(GL_BACK_RIGHT);
// following comand replace glDrawBuffer(GL_BACK_RIGHT);
  glStencilFunc(GL_EQUAL,1,1); // draws if stencil <> 0
 
// we can not do clearing (erasing previous image)
//	glClearColor(0.0, 0.0, 1.0, 1.0); /* blue */
//	glClear(GL_COLOR_BUFFER_BIT);
	// writing something to test the stencil operation
	glColor4f(0,0,1,1); // blue
	glBegin(GL_QUADS);
		glVertex2f(50,50);
		glVertex2f(50,350);
		glVertex2f(350,350);
		glVertex2f(350,50);
	glEnd();
	// cretaing the stereo interlaced image
	// getting into screen coordinates

	// end of red buffer restoring
	glutSwapBuffers();
}
void reshape(int w,int h)
{
	glViewport(0,0,w,h);
	interlace_stencil(w,h);
}   

int
main(int argc, char **argv)
{
  GLLogMsg("Experimental Stereo Renderer\n");
  glutInit(&argc, argv);
//  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_STEREO);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_STENCIL);
  glutInitWindowSize(400,400); 
  glutCreateWindow("stereo example");
  glutDisplayFunc(display);
  glutReshapeFunc(reshape);
  glutMainLoop();
  return 0;             /* ANSI C requires main to return int. */
}
