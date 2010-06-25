#include "Camera.h"

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

  //Only to be sure:
  RotatedX = RotatedY = RotatedZ = 0.0;
}

void Camera::Move (SF3dVector Direction)
{
  AddF3dVectorToVector(&Position, &Direction );
}

void Camera::SetYAngle (GLfloat Angle)
{
  RotatedY = Angle;
  std::cout << "Angle: **** " << Angle << std::endl;
}

void Camera::RotateY (GLfloat Angle)
{
  RotatedY += Angle;
}

void Camera::RotateX (GLfloat Angle)
{
  RotatedX += Angle;
}

void Camera::SetPosition(GLfloat x, GLfloat y, GLfloat z)
{
  Position.x = x;
  Position.y = y;
  Position.z = z;
}

void Camera::Render( void )
{
  std::cout << "Camera position is:" 
	    << Position.x << ","
	    << Position.y << ","
	    << Position.z << std::endl;
  
  glRotatef(-RotatedX , 1.0, 0.0, 0.0);
  glRotatef(-RotatedY , 0.0, 1.0, 0.0);
  glRotatef(-RotatedZ , 0.0, 0.0, 1.0);
  glTranslatef( -Position.x, -Position.y, -Position.z );
}
