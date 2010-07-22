/*
 * DataManager.h    
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

#ifndef __STD_LIB_INCLUDE
#define __STD_LIB_INCLUDE

#include <stdio.h>
#include <cstdlib> 
#include <sstream>
#include <vector>
#include <iostream>

#endif

#ifndef __DATA_INTERFACE
#define __DATA_INTERFACE

#include "IImageSelector.h"
#include "Robot.h"
#include "texture_png.h"
#include "DataLogic.h"
#include "Camera.h"
#include "data_types.h"


class DataManager
{
 private:

  GLuint _texture[1];
  Robot * _rob;
  DataLogic * _logic;
  Camera * _camera;
  IImageSelector * _calculator;

  robot_data * _robot_status;
  image_data * _bg_image_data;

  /* bind the specified image to a texture */
  void LoadGLTextures(GLuint * texture, const char* filename);

  /* move camera */
  /* void MoveCamera(); */

 public:
  /* data manager conversion constructor */
  /* first parameter is a robot instance */
  /* second parameter is the simulation session number 
     it is only needed for offline testing */
  DataManager(Robot *, DataLogic *, Camera *, 
	      IImageSelector *); 
  
  ~DataManager();
  void NextStep(int command = 0);
};

#endif
