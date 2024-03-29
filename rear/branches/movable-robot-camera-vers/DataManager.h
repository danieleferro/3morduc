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

#include "DistanceCalcInterface.h"
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
  DistanceCalcInterface * _calculator;

  robot_data * _robot_status;
  image_data * _bg_image_data;

  float prev_x;
  float prev_y;
  float prev_theta;

  /* bind the specified image to a texture */
  void LoadGLTextures(GLuint * texture, const char* filename);

  /* move camera */
  void MoveCamera();

 public:
  /* data manager conversion constructor */
  /* first parameter is a robot instance */
  /* second parameter is the simulation session number 
     it is only needed for offline testing */
  DataManager(Robot *, DataLogic *, Camera *, 
	      DistanceCalcInterface *); 
  
  ~DataManager();
  void NextStep(bool first_time = false);
};

#endif
