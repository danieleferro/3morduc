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
 protected:
  /* position */
  GLfloat x ;
  GLfloat y ;					
  GLfloat theta ;  
  
 public:
  /* default value for
     y is equal to the robot radius in order to put the robot
     outside of the camera sight at beginning of the program
  */
  Robot();
  void Place(GLfloat x, GLfloat y, GLfloat theta);
  virtual void DrawRobot() = 0;

  GLfloat GetX();
  GLfloat GetY();
  GLfloat GetTheta();    
};

class Morduc : public Robot
{
 private:
  /* scale factor (for drawing) */
  GLfloat radius;

 public:
  Morduc(float radius = 4.0f);
  void DrawRobot();
};
#endif
