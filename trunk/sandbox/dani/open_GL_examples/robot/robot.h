#ifndef __ROBOT__
#define __ROBOT__

//#include "stdafx.h"
//#include <stdio.h>
//#include "CCronometer.h"
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>
#include <math.h>



class Robot
{

  // posizione
  GLfloat xr;
  GLfloat yr;					
  GLfloat thetar;

  // v max
  GLfloat vlim;

  // w max
  GLfloat wlim;

  // ear ratio
  int n ;                         
  
  // isoluzione encoder
  int res ;

  //diametro della ruota
  GLfloat d ;
  // meter / step ratio
  GLfloat msr ;

  GLfloat dxr, dyr, dthetar;
  float vrx,vry;

  // distanza tra ruote
  GLfloat l;
  
  GLfloat v;
  GLfloat w;

  // max dimension
  GLfloat radius; 
  int collisions;

 public:

  // constructor
  Robot(float x1, float x2, float ang3);
  
  void move(GLfloat xr, GLfloat yr, GLfloat thetar);


  GLfloat getX();
  GLfloat getY();
  GLfloat getTheta();
    
  void DrawRobot();  
  
};

#endif
