#include <math.h>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define TO_RADIANS(X) X * M_PI / 180

#define IMAGE_NOT_VALID -1

#define X0 0
#define Y0 1
#define M0 2

#define SWEEP_ANGLE 45

using namespace std;

struct image_data {
  float x;
  float y;
  float theta;
  float time;
  char path[100];
};

struct robot_data {
  float x;
  float y;
  float theta;
  float time;
};


float PointAlgorithm(robot_data * robot_status, image_data * bg_image_data) {

  // robot_status->theta is always between [-180; 180]

  float line_a[3];
  float line_b[3];
  float line_c[3];
  float line_d[3];

  float rhs;
  int sign = 1;

  /* calcolo la retta "a" */
  line_a[X0] = robot_status->x;
  line_a[Y0] = robot_status->y;
  line_a[M0] = tan( TO_RADIANS( (robot_status->theta - 90) ) );

  /* calcolo la retta "b" */
  line_b[X0] = robot_status->x;
  line_b[Y0] = robot_status->y;
  line_b[M0] = -1 / line_a[M0];

  /* calcolo la retta "c" */
  line_c[X0] = robot_status->x;
  line_c[Y0] = robot_status->y;
  line_c[M0] = tan( atan( line_b[M0] ) + TO_RADIANS( SWEEP_ANGLE ) );

  /* calcolo la retta "d" */
  line_d[X0] = robot_status->x;
  line_d[Y0] = robot_status->y;
  line_d[M0] = tan( atan( line_b[M0] ) - TO_RADIANS( SWEEP_ANGLE ) );

  cout << "Line A: " << line_a[M0] << endl;
  cout << "Line B: " << line_b[M0] << endl;
  cout << "Line C: " << line_c[M0] << endl;
  cout << "Line D: " << line_d[M0] << endl;

  cout << "Point ( "
       << bg_image_data->x << "; "
       << bg_image_data->y << "; " 
       << bg_image_data->theta << " )";

  /* first of all, let's exclude images ahead the robot
  rhs = line_a[M0] * bg_image_data->x + ( line_a[Y0] - line_a[M0] * line_a[X0] );

  
  if ( bg_image_data->y >= rhs )
    {

      cout << " is EXcluded. (control 1)" << endl;
      
      return IMAGE_NOT_VALID;

    }
  */

  if ( (robot_status->theta > -90 && robot_status->theta < 90  ) ) 
    sign = 1;
  else
    sign = -1;

  /* let's exclude images are outside the plan identified 
     by the sweep angle: line C
  
  rhs = line_c[M0] * bg_image_data->x + ( line_c[Y0] - line_c[M0] * line_c[X0] );


  if ( bg_image_data->y*sign <= rhs*sign )
    {

      cout << " is EXcluded. (control 2)" << endl;
      
      return IMAGE_NOT_VALID;

    }

  */

  /* let's exclude images are outside the plan identified 
     by the sweep angle: line D
  */
  
  rhs = line_d[M0] * bg_image_data->x + ( line_d[Y0] - line_d[M0] * line_d[X0] );

  if ( bg_image_data->y*sign <= rhs*sign )
    {

      cout << " is EXcluded. (control 2)" << endl;
      
      return IMAGE_NOT_VALID;

    }
  
  */
  
 
  cout << " is INcluded." << endl;
  return 0;

}



int main(int argc, char** argv) {

  srand(time(NULL));
  int out = rand() % 10 + 1;

  robot_data robot_status;
  image_data bg_image_data;

  robot_status.x = 0;
  robot_status.y = 0;
  robot_status.theta = -180;


  bg_image_data.x =  1.5;
  bg_image_data.y =  1;
  bg_image_data.theta = 0;
  
  
  PointAlgorithm(&robot_status, &bg_image_data);

  return 0;
}
