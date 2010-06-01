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

#endif

#ifndef __DATA_INTERFACE
#define __DATA_INTERFACE

#include <string>
#include <sstream>
#include <vector>
#include <iostream>
#include "robot.h"

struct actual_robot_status {

  float x;
  float y;
  float theta;
  float time;

};



class DataManager
{
 private:
  actual_robot_status mem;
  Robot * rob;
  int index;

  actual_robot_status GetNewPosition(int index);


 public:
  DataManager(Robot * robot); 
  void NextStep();
  
  
};

#endif
