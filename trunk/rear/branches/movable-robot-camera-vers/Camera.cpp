#include "Camera.h"
#include <iostream>

#define TO_DEGREES(X) X * 180 / M_PI
#define TO_RADIANS(X) X / 180 * M_PI

SF3dVector F3dVector ( GLfloat x, GLfloat y, GLfloat z )
{
  SF3dVector tmp;
  tmp.x = x;
  tmp.y = y;
  tmp.z = z;
  return tmp;
}

SF3dVector AddF3dVectors (SF3dVector* u, SF3dVector* v)
{
  SF3dVector result;
  result.x = u->x + v->x;
  result.y = u->y + v->y;
  result.z = u->z + v->z;
  return result;
}

void AddF3dVectorToVector ( SF3dVector * Dst, SF3dVector * V2)
{
  Dst->x += V2->x;
  Dst->y += V2->y;
  Dst->z += V2->z;
}


/***************************************************************************************/

Camera::Camera()
{
  //Init with standard OGL values:
  Position = F3dVector ( 0.0,
			 0.0,
			 0.0);

  // Viewdir is a unit vector
  // indicating the camera starting direction
  // it does not set the camera direction!
  ViewDir = F3dVector( 0.f,
		       0.f,
		       0.f);

  ViewDirChanged = false;
  //Only to be sure:
  RotatedX = RotatedY = RotatedZ = 0.0;
}

void Camera::GetViewDir( void )
{
  SF3dVector Step1, Step2;
  //Rotate around Y-axis:
  Step1.x = cos( (RotatedY + 90.0) * M_PI / 180);
  Step1.z = -sin( (RotatedY + 90.0) * M_PI / 180);

  //Rotate around X-axis:
  double cosX = cos (RotatedX * M_PI / 180);
  Step2.x = Step1.x * cosX;
  Step2.z = Step1.z * cosX;
  Step2.y = sin(RotatedX * M_PI / 180);

  std::cout << Step2.x << std::endl;
  std::cout << Step2.y << std::endl;
  std::cout << Step2.z << std::endl;

  //Rotation around Z-axis not yet implemented, so:
  ViewDir = Step2;
}

void Camera::Move (SF3dVector Direction)
{
  AddF3dVectorToVector(&Position, &Direction );
}

void Camera::RotateY (GLfloat Angle)
{
  std::cout << "RotateY called!" << std::endl;
  RotatedY += Angle;
  ViewDirChanged = true;
}

void Camera::RotateX (GLfloat Angle)
{
  std::cout << "RotateX called!" << std::endl;
  RotatedX += Angle;
  ViewDirChanged = true;
}

void Camera::SetPosition(GLfloat x, GLfloat y, GLfloat z)
{
  Position.x = x;
  Position.y = y;
  Position.z = z;
}

void Camera::SetViewDir(GLfloat x, GLfloat y, GLfloat z)
{
  ViewDir.x = x;
  ViewDir.y = y;
  ViewDir.z = z;
}

void Camera::Render( void )
{
  std::cout << "Camera position is:" 
	    << Position.x << ","
	    << Position.y << ","
	    << Position.z << std::endl;

  GetViewDir();

//   std::cout << "Camera orientation is:" 
// 	    << TO_DEGREES(ViewDir.x) << ","
// 	    << TO_DEGREES(ViewDir.y) << ","
// 	    << TO_DEGREES(ViewDir.z) << std::endl;

  std::cout << "Camera orientation is:" 
	    << ViewDir.x << ","
	    << ViewDir.y << ","
	    << ViewDir.z << std::endl;

//   glMatrixMode(GL_MODELVIEW);
//   glPushMatrix();
//   glLoadIdentity();

  glRotatef(-RotatedX , 1.0, 0.0, 0.0);
  glRotatef(-RotatedY , 0.0, 1.0, 0.0);
  glRotatef(-RotatedZ , 0.0, 0.0, 1.0);
  glTranslatef( -Position.x, -Position.y, -Position.z );

//   glPopMatrix();
}

void Camera::MoveForwards( GLfloat Distance )
{
  if (ViewDirChanged) GetViewDir();
  SF3dVector MoveVector;
  MoveVector.x = ViewDir.x * -Distance;
  MoveVector.y = ViewDir.y * -Distance;
  MoveVector.z = ViewDir.z * -Distance;
  AddF3dVectorToVector(&Position, &MoveVector );
}

void Camera::StrafeRight ( GLfloat Distance )
{
  if (ViewDirChanged) GetViewDir();
  SF3dVector MoveVector;
  MoveVector.z = -ViewDir.x * -Distance;
  MoveVector.y = 0.0;
  MoveVector.x = ViewDir.z * -Distance;
  AddF3dVectorToVector(&Position, &MoveVector );
}
