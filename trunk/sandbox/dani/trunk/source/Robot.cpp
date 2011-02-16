/*
 * Robot.cpp
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

#include "Robot.h"

// theta in deegres
Robot::Robot()
{
//   this->radius = radius;
  this -> x = 0;
  this -> y = 0;
  this -> theta = 0;

}

// theta in deegres
void Robot::Place(GLfloat new_x, GLfloat new_y, 
		  GLfloat new_theta)
{
  this -> x = new_x;
  this -> y = new_y;
  this -> theta = new_theta;
}


GLfloat Robot::GetX()
{
  return this -> x;
}

GLfloat Robot::GetY()
{

  return this -> y;
}

GLfloat Robot::GetTheta()
{

  return this -> theta;
}
