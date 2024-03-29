#ifndef __OPENGL
#define __OPENGL

#include <GL/glut.h>    // Header File For The GLUT Library 
#include <GL/gl.h>	// Header File For The OpenGL32 Library
#include <GL/glu.h>	// Header File For The GLu32 Library

#endif

#ifndef __CAMERA__
#define __CAMERA__


#include "math.h"

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
  SF3dVector ViewDir;	        /*Not used for rendering the camera, but for "moveforwards"
				  So it is not necessary to "actualize" it always. It is only
				  actualized when ViewDirChanged is true and moveforwards is called*/
  bool ViewDirChanged;
  GLfloat RotatedX, RotatedY, RotatedZ;	
  void GetViewDir ( void );

 public:
  Camera();				//inits the values (Position: (0|0|0) Target: (0|0|-1) )
  void Render ( void );	//executes some glRotates and a glTranslate command
  //Note: You should call glLoadIdentity before using Render
  void Move ( SF3dVector Direction );
  void SetPosition(GLfloat x, GLfloat y, GLfloat z);
  void SetViewDir(GLfloat x, GLfloat y, GLfloat z);
  void RotateX ( GLfloat Angle );
  void RotateY ( GLfloat Angle );
  void RotateZ ( GLfloat Angle );
  void RotateXYZ ( SF3dVector Angles );
  void MoveForwards ( GLfloat Distance );
  void StrafeRight ( GLfloat Distance );
};


SF3dVector F3dVector ( GLfloat x, GLfloat y, GLfloat z );
SF3dVector AddF3dVectors ( SF3dVector * u, SF3dVector * v);
void AddF3dVectorToVector ( SF3dVector * Dst, SF3dVector * V2);

#endif
