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
 public:

  // posizione
  GLfloat xr;
  GLfloat yr;					
  GLfloat thetar;

  // v max
  GLfloat vlim;

  // w max
  GLfloat wlim;

  /*
  // base dei tempi		
  CCronometer ck;
  */

  /* odometer data */

  // ear ratio
  int n ;                         
  
  // isoluzione encoder
  int res ;

  //diametro della ruota
  GLfloat d ;
  // meter / step ratio
  GLfloat msr ;

  /*
  // tempo
  float Time;
  */

  GLfloat dxr,dyr,dthetar;
  float vrx,vry;

  // distanza tra ruote
  GLfloat l;
  
  GLfloat v;
  GLfloat w;

  //float dnr = (2*v+w*l)/(2*cm);       //passi encoder destro
  //float dnl = (2*v-w*l)/(2*cm);       //passi encoder sinistro

  // max dimension
  GLfloat radius; 
  // FILE *fp;
  int collisions;

  // constructor
  Robot();
  Robot(float x1, float x2, float ang3);
  
  void move(GLfloat xr, GLfloat yr, GLfloat thetar);
  
  //void PaintRobot();
  
  void PaintRobot2();
  
  void SetW(float set);
  
  void SetV(float set);
  
  void movetank(float l, float r);
  
  //bool LoadSPFromFile(char *string,int kk);
  
  /*
  int RobColl(OBJECT *coll, bool activecontrol);
  
  int RobColl2(OBJECT *coll, bool activecontrol);
  */
};

#endif
