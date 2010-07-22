/*
 * Robot.h    
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

#endif
