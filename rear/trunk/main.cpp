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
//#include "camera.h"

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

/* Data Logic declaration */
DataLogic * logic = NULL;

/* Data Manager declaration */
DataManager * manager = NULL;

/* Camera declaration */
CCamera * camera = NULL; 

/* x, y, theta (PROVA) */
float x, y, theta;

void DrawNet(GLfloat size, GLint LinesX, GLint LinesZ)
{
  glBegin(GL_LINES);
  for (int xc = 0; xc < LinesX; xc++)
    {
      glVertex3f(-size / 2.0 + xc / (GLfloat)(LinesX-1)*size,
		 0.0,
		 size / 2.0);

      glVertex3f(-size / 2.0 + xc / (GLfloat)(LinesX-1)*size,
		 0.0,
		 size / -2.0);
    }

  for (int zc = 0; zc < LinesX; zc++)
    {
      glVertex3f(size / 2.0,
		0.0,
		-size / 2.0 + zc / (GLfloat)(LinesZ-1)*size);

      glVertex3f(size / -2.0,
		 0.0,
		 -size / 2.0 + zc / (GLfloat)(LinesZ-1)*size);
    }
  
  glEnd();
  
}


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
  //glClear(GL_COLOR_BUFFER_BIT);

  glLoadIdentity();
  camera->Render();

  
  /*
  glTranslatef(0.0,0.8,0.0);

  glScalef(3.0,1.0,3.0);
       
  GLfloat size = 2.0;
  GLint LinesX = 30;   
  GLint LinesZ = 30;


  GLfloat halfsize = size / 2.0;
  glColor3f(1.0,1.0,1.0);

  glPushMatrix();
  {
    glTranslatef(0.0,-halfsize ,0.0);
    DrawNet(size,LinesX,LinesZ);
    glTranslatef(0.0,size,0.0);
    DrawNet(size,LinesX,LinesZ);
  }
  glPopMatrix();

  glColor3f(0.0,0.0,1.0);
  glPushMatrix();
  {
    glTranslatef(-halfsize,0.0,0.0);	
    glRotatef(90.0,0.0,0.0,halfsize);
    DrawNet(size,LinesX,LinesZ);
    glTranslatef(0.0,-size,0.0);
    DrawNet(size,LinesX,LinesZ);
  }
  glPopMatrix();

  glColor3f(1.0,0.0,0.0);
  glPushMatrix();
  {
    glTranslatef(0.0,0.0,-halfsize);
    glRotatef(90.0,halfsize,0.0,0.0);
    DrawNet(size,LinesX,LinesZ);
    glTranslatef(0.0,size,0.0);
    DrawNet(size,LinesX,LinesZ);
  }
  glPopMatrix();

  glScalef(1/3.0,1.0,1/3.0);

  */


  /* draw the texture */
  DrawTexture();

	
  /* draw the robot */
  //rob->Place(0.0f, -70.0f, 0.0f);
  rob->DrawRobot();


  // last set material is for the textures
  setMaterial(1.0, 1.0, 1.0,
	      1.0, 1.0, 1.0,
	      1.0, 1.0, 1.0,
	      20);


  printf("X Y THETA nel main: %4.4f %4.4f %4.4f\n", x, y, theta);
	  
  glFlush();
  glutSwapBuffers();
  
}

void reshape ( int x, int y ) {
  
  if (y == 0 || x == 0)
    //Nothing is visible then, so return
    return;

  //Set a new projection matrix
  glMatrixMode(GL_PROJECTION);  
  glLoadIdentity();

  //Angle of view:40 degrees
  //Near clipping plane distance: 0.5
  //Far clipping plane distance: 20.0
  gluPerspective(40.0, (GLdouble)x/(GLdouble)y, 0.0001, 10000.0);

  glMatrixMode(GL_MODELVIEW);
  //Use the whole window for rendering
  glViewport(0, 0, x, y);

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
    camera->MoveForwards( - STEP *5 );
    break;

    // moving camera
  case p:
    camera->MoveForwards( STEP *5 );
    break;

    // moving camera
  case k:
    camera->RotateY( STEP*5 );
    break;

    // moving camera
  case l:
    camera->RotateY( - STEP*5 );
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

  display();
  glutPostRedisplay();
}

void animate () {

  /* update state variables */

  /* refresh screen */
  glutPostRedisplay();
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

  if (argc != 2)
    {
      
      printf("Usage: main <session_number>\n");
      exit(0);

    }

  /* initialize GLUT, using any commandline parameters 
     passed to the program */
  glutInit(&argc,argv);

  /* setup the size, position, and display mode for 
     new windows */
  glutInitWindowSize(624, 442);
  glutInitWindowPosition(0, 0);
  // glutInitDisplayMode(GLUT_RGB | GLUT_DEPTH);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);

  /* create and set up a window */
  window = glutCreateWindow("robot");


  /* robot instantiation
     do not instantiate the robot before 
     having initialized OpenGL since calls
     OpenGL functions */
  rob = new Robot();
  logic = new DataLogic(atoi(argv[1]));

  /* camera instatiation */
  camera = new CCamera();

  /* data manager instatiation */
  manager = new DataManager(rob, logic, camera);

  
  init();

  camera->Move( F3dVector(0.0, 0.0, 0.0 ));
  camera->MoveForwards( 1.0 );



  /* define the projection transformation
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(60, 624/442, 0.001, 100000);
  
  /* define the viewing transformation 
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(0.0f, 0.0f, 10.0f,
	    0.0f, 0.0f, 0.0f,
	    0.0f, 1.0f, 0.0f);

  */
 
  /* tell GLUT to wait for events */
  glutMainLoop();
}
