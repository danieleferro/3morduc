#ifndef __OPENGL
#define __OPENGL

#include <GL/glut.h>    // Header File For The GLUT Library 
#include <GL/gl.h>	// Header File For The OpenGL32 Library
#include <GL/glu.h>	// Header File For The GLu32 Library

#endif

#ifndef __CAMERA__
#define __CAMERA__

#include "math.h"
#include <iostream>

/////////////////////////////////
//Note: All angles in degrees  //
/////////////////////////////////


//Float 3d-vect, normally used
struct SF3dVector  
{
  GLfloat x,y,z;
};

struct SF2dVector
{
  GLfloat x,y;
};


class Camera
{
 private:
  SF3dVector Position;
  GLfloat RotatedX, RotatedY, RotatedZ;	

 public:
  Camera();             //inits the values
  void Render ( void );	//executes some glRotates and a glTranslate command
  //Note: You should call glLoadIdentity before using Render
  void Move ( SF3dVector Direction );
  float GetX();
  float GetY();
  float GetZ();
  void SetPosition(GLfloat x, GLfloat y, GLfloat z);
  void SetYAngle( GLfloat Angle );
  void RotateX ( GLfloat Angle );
  void RotateY ( GLfloat Angle );
  void RotateZ ( GLfloat Angle );
  void RotateXYZ ( SF3dVector Angles );
};


SF3dVector F3dVector ( GLfloat x, GLfloat y, GLfloat z );
SF3dVector AddF3dVectors ( SF3dVector * u, SF3dVector * v);
void AddF3dVectorToVector ( SF3dVector * Dst, SF3dVector * V2);

#endif
