#include "robot.h"

void PaintCylinder(GLfloat radius,GLfloat height);
void PaintDisk(GLfloat radius);

Robot::Robot(float x = 0.f, float y = 0.f, float theta = 0.f)
{
  radius = 4.f;
  this->x = x;
  this->y = y;
  this->theta = theta;
}

void Robot::Move(GLfloat new_x, GLfloat new_y, 
		 GLfloat new_theta)
{
  this->x = new_x;
  this->y = new_y;
  this->theta = new_theta;
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
  glTranslatef(this->x, 0.0f, this->y);
  glRotatef(-(this->theta) * 180 / M_PI, 0.0f, 1.0f, 0.0f);

  // compute theta's cosine and sine value
  cosine = cos(this->theta);
  sine = sin(this->theta);
  
  // translate on z axis
  glTranslatef(0.0f,0.08f,0.0f);

  glScalef(radius, radius, radius);
  
  // draw robot
  PaintCylinder(1.0f, 0.1);
  PaintDisk(-1.0f);
  glTranslatef(0.0f, 0.1f, 0.0f);
  PaintDisk(1.0f);
  
  glTranslatef(0.0f, 0.6f, 0.0f);

  PaintCylinder(1.0f, 0.1f);
  PaintDisk(-1.0f);
  glTranslatef(0.0f, 0.1f, 0.0f);
  PaintDisk(1.0f);

  glTranslatef(0.8f, 0.0f, 0.0f);
  glColor3f(0.5f, 0.5f, 0.5f);
  PaintCylinder(0.2f, 0.3f);
  glTranslatef(0.0f, 0.3f, 0.0f);
  PaintDisk(0.2f);

  glTranslatef(0,0.401,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, reflectance_white);
  PaintDisk(0.1f);
  glTranslatef(0,-0.701,0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, reflectance_black);

  glTranslatef(-0.8f, 0.0f, 0.0f);
  glColor3f(0.1f, 0.1f, 0.1f);
  glTranslatef(0.0f ,0.6f, 0.0f);
  PaintCylinder(1.0f, 0.1f);
  PaintDisk(-1.0f);
  glTranslatef(0.0f, 0.1f, 0.0f); 
  PaintDisk(1.0f);

  glTranslatef(0.0f, -1.5f, 0.0f);
  glTranslatef(0.0f, 0.0f, 0.8f);
  PaintCylinder(0.1f, 1.5f);
  glTranslatef(0.0f, 0.0f, -1.60f);
  PaintCylinder(0.1f, 1.5f);
  glTranslatef(-0.8f, 0.0f, 0.8f);

  PaintCylinder(0.1f, 1.5f);
  glTranslatef(0.8f, 0.0f, 0.0f);

  glScalef(1/radius, 1/radius, 1/radius);

  glPopMatrix();
}


void PaintDisk(GLfloat radius)
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

void PaintCylinder(GLfloat radius, GLfloat height)
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

GLfloat Robot::GetX()
{
  return this->x;
}

GLfloat Robot::GetY() {

  return this->y;
}

GLfloat  Robot::GetTheta() {

  return this->theta;
}

