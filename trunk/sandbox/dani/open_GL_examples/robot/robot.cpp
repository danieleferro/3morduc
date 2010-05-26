//#include "stdafx.h"
//#include "util.h"
#include <sys/types.h>  /// se non si mette prima dei prototipi prende quelli come definizione!
#include "robot.h"
#include <stdio.h>
#include <iostream>

void paintCylinder(GLfloat radius,GLfloat height);
void paintDisk(GLfloat radius);
void DrawPar3(GLfloat x0,GLfloat y0,GLfloat z0,GLfloat l,GLfloat h,GLfloat d );

extern int wi;
extern int he;
extern char *dest;

void DrawPar3(GLfloat x0,GLfloat y0,GLfloat z0,GLfloat l,GLfloat h,GLfloat d )
/// xyz0 uk centro e il resto le dimensioni
{
  // glTranslatef(x0,y0,z0);
  // glBindTexture(GL_TEXTURE_2D, texture[0]);
  
  glBegin(GL_QUADS);
  
  GLfloat direction=1.0f;

  glNormal3f(0.0f,0.0f,direction);
  glTexCoord2f(0.0f, 10.0f);glVertex3f(x0+l/2,y0+d/2,z0+h/2); //6
  glTexCoord2f(0.0f, 0.0f);glVertex3f(x0-l/2,y0+d/2,z0+h/2);
  glTexCoord2f(1.0f, 0.0f);glVertex3f(x0-l/2,y0-d/2,z0+h/2);
  glTexCoord2f(1.0f, 10.0f);glVertex3f(x0+l/2,y0-d/2,z0+h/2);

  glNormal3f(0,0,-direction);
  glTexCoord2f(0.0f, 10.0f);glVertex3f(x0+l/2,y0+d/2,z0-h/2);  //1
  glTexCoord2f(0.0f, 0.0f);glVertex3f(x0+l/2,y0-d/2,z0-h/2);
  glTexCoord2f(1.0f, 0.0f);glVertex3f(x0-l/2,y0-d/2,z0-h/2);
  glTexCoord2f(1.0f, 10.0f);glVertex3f(x0-l/2,y0+d/2,z0-h/2);

  glNormal3f(direction,0,0);
  glTexCoord2f(0.0f, 10.0f);glVertex3f(x0+l/2,y0+d/2,z0+h/2); //2

  glTexCoord2f(1.0f, 10.0f);glVertex3f(x0+l/2,y0-d/2,z0+h/2);
  glTexCoord2f(1.0f, 0.0f);glVertex3f(x0+l/2,y0-d/2,z0-h/2);
  glTexCoord2f(0.0f, 0.0f);glVertex3f(x0+l/2,y0+d/2,z0-h/2);

  glNormal3f(-direction,0,0);
  glTexCoord2f(0.0f, 10.0f);glVertex3f(x0-l/2,y0+d/2,z0+h/2); //5
  glTexCoord2f(1.0f, 10.0f);glVertex3f(x0-l/2,y0+d/2,z0-h/2);
  glTexCoord2f(1.0f, 0.0f);glVertex3f(x0-l/2,y0-d/2,z0-h/2);
  glTexCoord2f(0.0f, 0.0f);glVertex3f(x0-l/2,y0-d/2,z0+h/2);

  glNormal3f(0,-direction,0);
  glVertex3f(x0+l/2,y0+d/2,z0+h/2); //3
  glVertex3f(x0+l/2,y0+d/2,z0-h/2);
  glVertex3f(x0-l/2,y0+d/2,z0-h/2);
  glVertex3f(x0-l/2,y0+d/2,z0+h/2);

  /*
    glNormal3f(0,-direction,0);
    glVertex3f(x0+l/2,y0-d/2,z0+h/2); //4
    glVertex3f(x0+l/2,y0-d/2,z0-h/2);
    glVertex3f(x0-l/2,y0-d/2,z0-h/2);
    glVertex3f(x0-l/2,y0-d/2,z0+h/2);
  */

  glEnd();
}



Robot::Robot(float x1 = 0.f, float y2 = 0.f, float ang3 = 0.f)
{
  // meter / step ratio
  msr = 3.1412*d/n/res;

  // gear ratio
  n = 1;

  // risoluzione encoder
  res = 2000;

  // diametro della ruota
  d = 0.063f*2.0f;

  // distanza tra ruote
  l = 0.28f;
  vlim = 7;
  wlim = 2;

  radius = 4.f;

  dxr = 0;
  dyr = 0;
  dthetar = 0;
  collisions = 0;

  xr = x1;
  yr = y2;
  thetar = ang3;
}

void Robot::move(GLfloat xr, GLfloat yr, GLfloat thetar)
{

  this->xr = xr;
  this->yr = yr;
  this->thetar = thetar;

}

void Robot::DrawRobot()
{

  GLfloat reflectance_black[] = { 0.2f, 0.2f, 0.2f};
  GLfloat reflectance_white[] = { 0.8f, 0.8f, 0.8f};
  
  GLfloat cosine, sine;

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();

  // set robot height
  glTranslatef(0.0f, -12.0f, 0.0f);

  // set robot reflectance (it is black)
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, reflectance_black);

  // set robot position
  glTranslatef(xr, 0.0f, yr);
  glRotatef(-thetar * 180 / M_PI, 0.0f, 1.0f, 0.0f);

  // compute theta's cosine and sine value
  cosine = cos(thetar);
  sine = sin(thetar);
  
  // translate on z axis
  glTranslatef(0.0f,0.08f,0.0f);

  glScalef(radius, radius, radius);
  
  // draw robot
  paintCylinder(1.0f, 0.1);
  paintDisk(-1.0f);
  glTranslatef(0.0f, 0.1f, 0.0f);
  paintDisk(1.0f);
  
  glTranslatef(0.0f, 0.6f, 0.0f);

  paintCylinder(1.0f, 0.1f);
  paintDisk(-1.0f);
  glTranslatef(0.0f, 0.1f, 0.0f);
  paintDisk(1.0f);

  glTranslatef(0.8f, 0.0f, 0.0f);
  glColor3f(0.5f, 0.5f, 0.5f);
  paintCylinder(0.2f, 0.3f);
  glTranslatef(0.0f, 0.3f, 0.0f);
  paintDisk(0.2f);

  glTranslatef(0,0.401,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, reflectance_white);
  paintDisk(0.1f);
  glTranslatef(0,-0.701,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, reflectance_black);

  glTranslatef(-0.8f, 0.0f, 0.0f);
  glColor3f(0.1f, 0.1f, 0.1f);
  glTranslatef(0.0f ,0.6f, 0.0f);
  paintCylinder(1.0f, 0.1f);
  paintDisk(-1.0f);
  glTranslatef(0.0f, 0.1f, 0.0f); 
  paintDisk(1.0f);

  glTranslatef(0.0f, -1.5f, 0.0f);
  glTranslatef(0.0f, 0.0f, 0.8f);
  paintCylinder(0.1f, 1.5f);
  glTranslatef(0.0f, 0.0f, -1.60f);
  paintCylinder(0.1f, 1.5f);
  glTranslatef(-0.8f, 0.0f, 0.8f);

  paintCylinder(0.1f, 1.5f);
  glTranslatef(0.8f, 0.0f, 0.0f);

  glScalef(1/radius, 1/radius, 1/radius);

  glPopMatrix();
}


void paintDisk(GLfloat radius)
{

  glBegin(GL_POLYGON);
  float pos = -1;
  if (radius <= 0)
    pos = pos*pos;
  
  glNormal3f(0,pos,0);
  
  // glVertex3f(0,0,0);
  
  for (int k = 0; k <= 360; k++)
    glVertex3f(radius * cos(M_PI/180*k), 0, radius * sin(M_PI/180*k*pos));

  glEnd();

}

void paintCylinder(GLfloat radius, GLfloat height)
{

  GLfloat c[361], s[361];

  glBegin(GL_QUAD_STRIP);

  for (int k=0;k<=360;k++) {

    c[k]=cos(M_PI/180*k);
    s[k]=sin(M_PI/180*k);

    glNormal3f(c[k], s[k], 0);
    glVertex3f(radius * c[k], 0, radius * s[k]);
    glVertex3f(radius * c[k], height, radius * s[k]);

  }

  for (int k=0; k<=360; k++) {

      glNormal3f(c[k], s[k], 0);
      glVertex3f(radius*c[k], height, radius*s[k]);

  }

  glEnd();

}

GLfloat Robot::getX()
{

  return xr;
}

GLfloat Robot::getY() {

  return yr;
}

GLfloat  Robot::getTheta() {

  return thetar;
}

