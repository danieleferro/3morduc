#ifndef __OPENGL
#define __OPENGL

#include <GL/glut.h>    // Header File For The GLUT Library 
#include <GL/gl.h>	// Header File For The OpenGL32 Library
#include <GL/glu.h>	// Header File For The GLu32 Library

#endif

#ifndef __ROBOT__
#define __ROBOT__

#include <math.h>
#include <iostream>
#include <stdio.h>

class Robot
{
 private:
  /* position */
  GLfloat x ;
  GLfloat y ;					
  GLfloat theta ;
  
  /* start point */
  GLfloat x_zero;
  GLfloat y_zero;					
  GLfloat theta_zero;
  
  
  /* scale factor (for drawing) */
  GLfloat radius; 

 public:
  /* default value for
     y is equal to the robot radius in order to put the robot
     outside of the camera sight at beginning of the program
  */
  Robot(float x = 0.f, float y = 4.f, float theta = - M_PI / 2);
  void Place(GLfloat x, GLfloat y, GLfloat theta);
  void DrawRobot();
  void SetInit(float x, float y, float theta);


  GLfloat GetX();
  GLfloat GetY();
  GLfloat GetTheta();    
};

#endif
