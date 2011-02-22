/*
 * Camera.cpp
 *
 * This file is part of REAR.
 * Copyright (C) 2010 Daniele Ferro (daniele.ferro86@gmail.com) 
 *                    Loris Fichera (loris.fichera@gmail.com)
 *
 * REAR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * REAR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with REAR.  If not, see <http://www.gnu.org/licenses/>.
 */

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
  _theta = Angle;
}

void Camera::RotateY (GLfloat Angle)
{
  RotatedY += Angle;
  _theta += Angle;
}

void Camera::RotateX (GLfloat Angle)
{
  RotatedX += Angle;
}

GLfloat Camera::GetX ()
{
  return Position.x;
}

GLfloat Camera::GetY ()
{
  return Position.y;
}

GLfloat Camera::GetZ ()
{
  return Position.z;
}

GLfloat Camera::GetTheta()
{
  return _theta;
}

void Camera::SetPosition(GLfloat x, GLfloat y, GLfloat z)
{
  Position.x = x;
  Position.y = y;
  Position.z = z;
}

void Camera::Render( void )
{
  if (__CAMERA__DBG)
    std::cout << "Camera position is:     " 
	      << Position.x << ","
	      << Position.z << ","
	      << _theta + 90 << std::endl;
  
  glRotatef(-RotatedX , 1.0, 0.0, 0.0);
  glRotatef(-RotatedY , 0.0, 1.0, 0.0);
  glRotatef(-RotatedZ , 0.0, 0.0, 1.0);
  glTranslatef( -Position.x, -Position.y, -Position.z );
}
