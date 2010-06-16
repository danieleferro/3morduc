#include <math.h>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define TO_RADIANS(X) X * M_PI / 180
#define TO_DEGREES(X) X * 180 / M_PI

#define IMAGE_NOT_VALID -1

#define X0 0
#define Y0 1
#define M0 2

#define SWEEP_ANGLE 45

#define DEBUG 1

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

float Normalize360(float angle)
{
  if (angle > 360)
    return Normalize360(angle - 360);

  if (angle < -360)
    return Normalize360(angle + 360);

  return angle;
}

float Normalize180(float angle)
{
  if (angle > 180)
    return Normalize180(angle - 360);

  if (angle < -180)
    return Normalize180(angle + 360);

  return angle;
}


float PointAlgorithm(robot_data * robot_status, image_data * bg_image_data) {

  // robot_status->theta is always between [-180; 180]

  float line_a[3];
  float line_b[3];
  float line_c[3];
  float line_d[3];

  float gamma;
  float delta;

  float rhs_c;
  float rhs_d;
  int sign = 1;

  /* calcolo la retta "b" */
  line_b[X0] = robot_status->x;
  line_b[Y0] = robot_status->y;
  line_b[M0] = tan (TO_RADIANS( robot_status -> theta ));

  /* calcolo la retta "a" */
  line_a[X0] = robot_status->x;
  line_a[Y0] = robot_status->y;
  line_a[M0] = - 1 / line_b[M0];

  /* calcolo la retta "c" */
  gamma =  Normalize180(robot_status -> theta - ( 360 - SWEEP_ANGLE ));
  line_c[X0] = robot_status->x;
  line_c[Y0] = robot_status->y;
  line_c[M0] = tan( gamma );

  if (DEBUG)
    cout << "DEBUG: coefficiente angolare retta c: " <<
      tan ( TO_RADIANS( gamma )) <<
      endl;

  /* calcolo la retta "d" */
  delta = Normalize180(robot_status -> theta + ( 360 - SWEEP_ANGLE ));
  line_d[X0] = robot_status->x;
  line_d[Y0] = robot_status->y;
  line_d[M0] = tan( delta );

  if (DEBUG) 
    cout << "DEBUG: coefficiente angolare d: " <<
      tan ( TO_RADIANS( delta )) <<
      endl;

  cout << "Gamma: " << gamma << endl;
  cout << "Delta: " << delta << endl;

  cout << "Point ( "
       << bg_image_data->x << "; "
       << bg_image_data->y << "; " 
       << bg_image_data->theta << " )"
       << endl;
  
  /* let's exclude images that do not fall within the area
   * identified by the SWEEP_ANGLE 
   */
  rhs_c = line_c[M0] * bg_image_data->x + ( line_c[Y0] - line_c[M0] * line_c[X0] );
  rhs_d = line_d[M0] * bg_image_data->x + ( line_d[Y0] - line_d[M0] * line_d[X0] );

  if ( gamma >= -90 && gamma <= 90 )
    {
      cout << " y <= rhs_c is being evaluated " << endl;
      
      if ( bg_image_data->y <= rhs_c )
	{
	  cout << " is EXcluded." << endl;
	  
	  return IMAGE_NOT_VALID;	  
	}
    }

  else
    {
      cout << " y >= rhs_c is being evaluated " << endl;

      if ( bg_image_data->y >= rhs_c )
	
	{
	  cout << " is EXcluded." << endl;
	  
	  return IMAGE_NOT_VALID;	  
	}

    }

  if ( delta >= -90 && delta <= 90 )
    {
      cout << " y >= rhs_d is being evaluated " << endl;
      
      if ( bg_image_data->y >= rhs_d )
	{
	  cout << " is EXcluded." << endl;
	  
	  return IMAGE_NOT_VALID;
	  
	}
    }

  else
    {
      cout << " y <= rhs_d is being evaluated " << endl;

      if ( bg_image_data->y <= rhs_d )
	
	{
	  cout << " is EXcluded." << endl;
	  
	  return IMAGE_NOT_VALID;	  
	}
    }
 
  cout << " is INcluded." << endl;
  return 0;

}

int main(int argc, char** argv) {

  srand(time(NULL));
  int out = rand() % 10 + 1;

  robot_data robot_status;
  image_data bg_image_data;
  
  float robot_theta;
  float x_im, y_im;

  for (;;)
    {
      cout << endl;
      cout << "Insert robot orientation (-180, 180): " << endl;
      cin >> robot_theta;
      cout << "Insert image x coordinate: " << endl;
      cin >> x_im;
      cout << "Insert image y coordinate: " << endl;
      cin >> y_im;
      
      robot_status.x = 0;
      robot_status.y = 0;
      robot_status.theta = robot_theta;
      
      
      bg_image_data.x =  x_im;
      bg_image_data.y =  y_im;
      bg_image_data.theta = 0;
   
      PointAlgorithm(&robot_status, &bg_image_data);
    }

  return 0;
}
