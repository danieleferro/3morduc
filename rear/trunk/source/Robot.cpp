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
