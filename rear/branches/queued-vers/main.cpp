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

#include <unistd.h>     // needed to sleep.
#include <iostream>
#include "robot.h"

// why does include compile ?! 
#include "DataManager.h"

#include "key_mapping.h"
#include "texture_png.h"

/* step for forward direction */
#define STEP 0.1f

/* step for rotation */
#define ANGLE 0.1f

/* The number of our GLUT window */
int window; 

/* storage for one texture  */
//GLuint texture[1];

/* robot declaration */
Robot * rob = NULL;

/* Data Manager declaration */
DataManager * manager = NULL;


void setMaterial ( GLfloat ambientR, GLfloat ambientG, GLfloat ambientB, 
		   GLfloat diffuseR, GLfloat diffuseG, GLfloat diffuseB, 
		   GLfloat specularR, GLfloat specularG, GLfloat specularB,
		   GLfloat shininess) {
  
  GLfloat ambient[] = { ambientR, ambientG, ambientB,    };
  GLfloat diffuse[] = { diffuseR, diffuseG, diffuseB,    };
  GLfloat specular[] = { specularR, specularG, specularB };
  
  glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT,ambient);
  glMaterialfv(GL_FRONT_AND_BACK,GL_DIFFUSE,diffuse);
  glMaterialfv(GL_FRONT_AND_BACK,GL_SPECULAR,specular);
  glMaterialf(GL_FRONT_AND_BACK,GL_SHININESS,shininess);
}

// --------------------------------------------------------------------
//                          GLUT CALLBACKS
// --------------------------------------------------------------------
void display () {

  /* clear window */
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  /* future matrix manipulations should affect the modelview matrix */
  
  /* draw the texture */
  DrawTexture();
  
  /* draw the robot */
  rob->DrawRobot();
  
  // last set material is for the textures
  setMaterial(1.0, 1.0, 1.0,
	      1.0, 1.0, 1.0,
	      1.0, 1.0, 1.0,
	      20);

  /* flush drawing routines to the window */
  glFlush();
}

void reshape ( int width, int height ) {

  /* define the viewport transformation */
  glViewport(0, 0, width, height);
}

/* The function called whenever a key is pressed. */
void keyPressed(unsigned char key, int x, int y) 
{
  /* avoid thrashing this procedure */
  usleep(100);

  GLfloat _oldX = rob->GetX();
  GLfloat _oldY = rob->GetY();
  GLfloat _oldTheta = rob->GetTheta();

  GLfloat temp;
  
  switch (key) {

  case ESCAPE :
   
    /* If escape is pressed, kill everything. */
    
    /* shut down our window */
    glutDestroyWindow(window); 
    
    /* exit the program...normal termination. */
    exit(0);
    
  case q:
    temp = _oldTheta + ANGLE;
    rob->Place(_oldX, _oldY, temp);
    break;

  case w:
    temp = _oldTheta - ANGLE;
    rob->Place(_oldX, _oldY, temp);
    break;

  case s:
    
    manager->NextStep();
    break;

    
    // moving camera
  case o:
    glTranslatef(0.f, 0.f, STEP *5 );
    break;

    // moving camera
  case p:
    glTranslatef(0.f, 0.f, - STEP * 5);
    break;

    // moving camera
  case k:
    glRotatef(STEP * 5,
	      0.0,
	      1.f,
	      0.0);
    break;

    // moving camera
  case l:
    glRotatef(- STEP * 5,
	      0.0f,
	      1.f,
	      0.0f);
    break;


  }

  glutPostRedisplay();


}

void specialKeyPressed(int key, int x, int y) 
{
  /* avoid thrashing this procedure */
  usleep(100);

  GLfloat _oldX = rob->GetX();
  GLfloat _oldY = rob->GetY();
  GLfloat _oldTheta = rob->GetTheta();

  GLfloat temp;


  switch (key) {

  case GLUT_KEY_UP :

    temp = _oldY - STEP;
    rob->Place(_oldX, temp, _oldTheta);
    break;

  case GLUT_KEY_DOWN :

    temp = _oldY + STEP;
    rob->Place(_oldX, temp, _oldTheta);
    break;

  case GLUT_KEY_RIGHT :

    temp = _oldX + STEP;
    rob->Place(temp, _oldY, _oldTheta);
    break;    

  case GLUT_KEY_LEFT :

    temp = _oldX - STEP;
    rob->Place(temp, _oldY, _oldTheta);
    break;

  }

  glutPostRedisplay();
}

void animate () {

  /* update state variables */

  /* refresh screen */
  glutPostRedisplay();
}


/*
 * getGLPos is intended to be passed as a callback function to the
 * glutPassiveMotionFunc((void)(int, int))
 * it is triggered every time GLUT perceives that the mouse is moving
 */
void GetGLPos(int x, int y) {


  // printf(">> X: %d\t Y: %d\n", x, y);
  GLint viewport[4];
  GLdouble modelview[16];
  GLdouble projection[16];
  GLfloat winX, winY, winZ;
  GLdouble posX, posY, posZ;
  
  glGetDoublev( GL_MODELVIEW_MATRIX, modelview );
  glGetDoublev( GL_PROJECTION_MATRIX, projection );
  glGetIntegerv( GL_VIEWPORT, viewport );
  
  winX = (float)x;
  winY = (float)viewport[3] - (float)y;
  glReadPixels( x, int(winY), 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, &winZ );
  
  gluUnProject( winX, winY, winZ, modelview, projection, viewport, &posX, &posY, &posZ);

  printf(">> Win -> ( %4.4f, %4.4f, %4.4f ) \n>> GL  -> ( %4.4f, %4.4f, %4.4f ) \n\n",
	 winX, (float)viewport[3] - winY, winZ, posX, posY, posZ);
}

void init()
{

  /* callback registering */
  glutDisplayFunc(display);
  glutReshapeFunc(reshape);
  // glutPassiveMotionFunc(getGLPos);
  glutKeyboardFunc(keyPressed);
  glutSpecialFunc(specialKeyPressed);
  //glutIdleFunc(animate);

  /* set up depth-buffering */
  glEnable(GL_DEPTH_TEST);
    
  /* load image for texturing  */
  //LoadGLTextures(texture, "screenshot.bmp");

  /* set the color for glClear() */
  glClearColor(0.0f, 0.0f, 0.0f, 0.0f);

  /* a few other things */
  glClearDepth(1.0);		    // Enables Clearing Of The Depth Buffer
  glDepthFunc(GL_LESS);		    // The Type Of Depth Test To Do
  glShadeModel(GL_SMOOTH);	    // Enables Smooth Color Shadingx

  /* set up lighting */
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  
  GLfloat lightpos[] = { 0.0f, 0.0f, 1.0f, 0.0f };
  GLfloat lightcolor[] = { 1.0f, 1.0f, 1.0f };
  GLfloat ambcolor[] = { 1.0f, 1.0f, 1.0f };
 
  glEnable(GL_LIGHTING);
  glLightModelfv(GL_LIGHT_MODEL_AMBIENT, ambcolor);
  
  glEnable(GL_LIGHT0);                          // set up LIGHT0
  glLightfv(GL_LIGHT0, GL_POSITION, lightpos);  // configure LIGHT0
  glLightfv(GL_LIGHT0, GL_AMBIENT, lightcolor);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, lightcolor);
  glLightfv(GL_LIGHT0, GL_SPECULAR, lightcolor);
  glLightf(GL_LIGHT0,  GL_SPOT_CUTOFF, 45.0f);

}

int main ( int argc, char * argv[] ) {

  /* initialize GLUT, using any commandline parameters 
     passed to the program */
  glutInit(&argc,argv);

  /* setup the size, position, and display mode for 
     new windows */
  glutInitWindowSize(624, 442);
  glutInitWindowPosition(0, 0);
  glutInitDisplayMode( GLUT_RGB | GLUT_DEPTH);

  /* create and set up a window */
  window = glutCreateWindow("robot");

  /* robot instantiation
     do not instantiate the robot before 
     having initialized OpenGL since calls
     OpenGL functions */
  rob = new Robot();

  /* data manager instatiation */
  manager = new DataManager(rob, 22 );
  
  init();

  /* define the projection transformation */
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(60, 624/442, 0.001, 100000);
  
  /* define the viewing transformation */
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(0.0, 0.0, 10.0,
	    0.0, 0.0, 0.0,
	    0.0, 1.0, 0.0);

 
  /* tell GLUT to wait for events */
  glutMainLoop();
}
