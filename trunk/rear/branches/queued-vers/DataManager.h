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
#include "texture_png.h"

#define STACK_SIZE 5
#define TRIGGER    0.001f

struct actual_robot_status {

  float x;
  float y;
  float theta;
  float time;

};

struct robot_data {

  actual_robot_status position;
  std::string image_path;

};



class DataManager
{
 private:
  actual_robot_status mem;
  std::string actual_image;

  Robot * rob;
  int index;
  int session;
  GLuint texture[1];

  std::vector<robot_data> queue;

  uint CountTranslation(std::vector<robot_data> queue);
  bool IsTranslation(robot_data data);



  /* fetch data position and camera image
     from robot */
  robot_data GetNewData(int index);

  /* bind the specified image to a texture */
  void LoadGLTextures(GLuint * texture, std::string filename);

  /* move camera */
  void MoveCamera(robot_data delta);

 public:
  /* data manager conversion constructor */
  /* first parameter is a robot instance */
  /* second parameter is the simulation session number 
     it is only needed for offline testing */
  DataManager(Robot * robot, int session); 
  void NextStep();
  
  
};

#endif
