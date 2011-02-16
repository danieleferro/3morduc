#ifndef __MORDUC
#define __MORDUC

#include "Robot.h"


class Morduc : public Robot
{
 private:
  /* scale factor (for drawing) */
  GLfloat radius;

  void PaintCylinder(GLfloat radius,GLfloat height);
  void PaintDisk(GLfloat radius);


 public:
  Morduc(float radius = 4.0f);
  void DrawRobot();
};

#endif
