#ifndef __OPENGL
#define __OPENGL

#include <GL/glut.h>    // Header File For The GLUT Library 
#include <GL/gl.h>	// Header File For The OpenGL32 Library
#include <GL/glu.h>	// Header File For The GLu32 Library

#endif

#ifndef __ROBOT__
#define __ROBOT__

#include <math.h>

class Robot
{
 private:
  /* position */
  GLfloat x ;
  GLfloat y ;					
  GLfloat theta ;
  
  /* scale factor (for drawing) */
  GLfloat radius; 

 public:
  Robot(float x, float y, float theta);
  void Move(GLfloat x, GLfloat y, GLfloat theta);
  void DrawRobot();  

  GLfloat GetX();
  GLfloat GetY();
  GLfloat GetTheta();    
};

#endif
