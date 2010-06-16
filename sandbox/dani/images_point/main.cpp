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

#define SWEEP_ANGLE 20
#define RADIUS      100
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


// A floating point
struct fPoint
{
  float x;
  float y;
};


float AssignPoints(float distance)
{
  /* TODO */

  return distance;
}

float Sign(fPoint p1, fPoint p2, fPoint p3)
{
  return (p1.x - p3.x) * (p2.y - p3.y) - (p2.x - p3.x) * (p1.y - p3.y);
}

bool IsPointInTri(fPoint pt, fPoint v1, fPoint v2, fPoint v3)
{
  bool b1, b2, b3;

  b1 = Sign(pt, v1, v2) < 0.0f;
  b2 = Sign(pt, v2, v3) < 0.0f;
  b3 = Sign(pt, v3, v1) < 0.0f;

  return ((b1 == b2) && (b2 == b3));
}

float NormalizeAngle(float angle_deegres) {


  if (angle_deegres >= 0 && angle_deegres <= 180)
    return angle_deegres;

  if (angle_deegres >= -180 && angle_deegres <= 0)
    return angle_deegres;
    
 
  return NormalizeAngle(angle_deegres-360);

}


float PointAlgorithm(robot_data * robot_status, image_data * bg_image_data) {

  /* translate all system in robot_status->x and 
     robot_status->y; at the end the system is translated again */
  float x_zero = robot_status->x;
  float y_zero = robot_status->y;

  robot_status->x = 0;
  robot_status->y = 0;
  
  bg_image_data->x = bg_image_data->x - x_zero;
  bg_image_data->y = bg_image_data->y - y_zero;


  // theta must be always between [-180; 180]
  robot_status->theta = NormalizeAngle(robot_status->theta);
  bg_image_data->theta = NormalizeAngle(bg_image_data->theta);


  float ang_coeff_line_a;
  float ang_coeff_line_b;
  float ang_coeff_line_c;
  float ang_coeff_line_d;

  float rhs;
  int sign = 1;

  /* angular coefficient for line "a" */
  ang_coeff_line_a = tan( TO_RADIANS(robot_status->theta - 90) );

  /* angular coefficient for line "b" */
  ang_coeff_line_b = tan( TO_RADIANS(robot_status->theta) );

  /* angular coefficient for line "c" */
  ang_coeff_line_c = tan( atan( ang_coeff_line_b ) + TO_RADIANS( SWEEP_ANGLE ) );

  /* angular coefficient for line "d" */
  ang_coeff_line_d = tan( atan( ang_coeff_line_b ) - TO_RADIANS( SWEEP_ANGLE ) );

  /*
    cout << "Angular coefficient line A: " << ang_coeff_line_a << endl;
    cout << "Angular coefficient line B: " << ang_coeff_line_b << endl;
    cout << "Angular coefficient line C: " << ang_coeff_line_c << endl;
    cout << "Angular coefficient line D: " << ang_coeff_line_d << endl;
  */

  /* find poit A and B to form AOB triangle */
  // A point (retrieved from line C)
  float A_x1 = sqrt( pow(RADIUS, 2) / ( pow( ang_coeff_line_c , 2) + 1 ) );
  float A_y1 = ang_coeff_line_c * A_x1;
  
  float A_x2 = -1 * A_x1;
  float A_y2 = ang_coeff_line_c * A_x2;
  
  /*
    cout << "A_x1: " << A_x1 << "; "
    << "A_y1: " << A_y1
    << endl << endl;
    
    cout << "A_x2: " << A_x2 << "; "
    << "A_y2: " << A_y2
    << endl << endl;
  */
    

  /* find poit A and B to form AOB triangle */
  // B point (retrieved from line D)
  float B_x1 = sqrt( pow(RADIUS, 2) / ( pow( ang_coeff_line_d , 2) + 1 ) );
  float B_y1 = ang_coeff_line_d * B_x1;

  float B_x2 = -1 * B_x1;
  float B_y2 = ang_coeff_line_d * B_x2;

  /*
    cout << "B_x1: " << B_x1 << "; "
    << "B_y1: " << B_y1
    << endl << endl;
    
    cout << "B_x2: " << B_x2 << "; "
    << "B_y2: " << B_y2
    << endl << endl;
  */

  if ( robot_status->theta >= 0 && robot_status->theta <= 180 )
    sign = -1;

  if ( robot_status->theta >= -180 && robot_status->theta < 0 )
    sign = 1;
   

  // find out which couple values are right (point A)
  float A_x;
  float A_y;

  rhs = ang_coeff_line_a * A_x1;
  // cout << sign << " * " << A_y1 << " >= " << rhs << " * " << sign << endl;

  if ( A_y1*sign >= rhs*sign )
    {

      A_x = A_x1;
      A_y = A_y1;

    }
  else
    {

      A_x = A_x2;
      A_y = A_y2;

    }
    
  cout << "point A chosen: " << A_x << " ; " << A_y << endl;

  // find out which couple values are right (point B)
  float B_x;
  float B_y;
  float B_z = 0;

  rhs = ang_coeff_line_a * B_x1;  
  // cout << sign << " * " << B_y1 << " >= " << rhs << " * " << sign << endl;

  if ( B_y1*sign >= rhs*sign )
    {

      B_x = B_x1;
      B_y = B_y1;

    }
  else
    {

      B_x = B_x2;
      B_y = B_y2;

    }
    
  cout << "point B chosen: " << B_x << " ; " << B_y << endl;

  fPoint pt;
  pt.x = bg_image_data->x;
  pt.y = bg_image_data->y;

  fPoint v1;
  v1.x = 0;
  v1.y = 0;
  
  fPoint v2;
  v2.x = A_x;
  v2.y = A_y;
  
  fPoint v3;
  v3.x = B_x;
  v3.y = B_y;
  
  bool flag = IsPointInTri(pt, v1, v2, v3);


  /* retraslate system */
  robot_status->x = x_zero;
  robot_status->y = y_zero;
  
  bg_image_data->x = bg_image_data->x + x_zero;
  bg_image_data->y = bg_image_data->y + y_zero;


  if (flag) 
    {
      cout << "Point ( "
	   << bg_image_data->x << "; "
	   << bg_image_data->y << "; " 
	   << bg_image_data->theta << " )";
      
      cout << " is included." << endl;
      
      float distance;
      float angle_gap;
      distance = sqrt (
		       pow( ( bg_image_data->x - robot_status->x), 2 ) +
		       pow( ( bg_image_data->y - robot_status->y), 2 ) );

      angle_gap = fabs( bg_image_data->theta - robot_status->theta);

      cout << "Distance from robot values: " << distance << endl;
      cout << "Angle gap: " << angle_gap << endl;
      


    }
  else
    {
      cout << "Point ( "
	   << bg_image_data->x << "; "
	   << bg_image_data->y << "; " 
	   << bg_image_data->theta << " )";
      
      cout << " is NOT included." << endl;
    }

  cout << "Robot angle: " << robot_status->theta << " deegres." << endl;


  return 0;

}



int main(int argc, char** argv) {

  srand(time(NULL));
  int out = rand() % 10 + 1;

  /*
  if (argc < 2) 
    {
      cout << "Usage: ./main <robot_angle_in_deegres>" << endl << endl;
      exit(0);
    }
  */

  robot_data robot_status;
  image_data bg_image_data;

  robot_status.x = 0;
  robot_status.y = 0;
  robot_status.theta = 0;


  bg_image_data.x =  -2;
  bg_image_data.y =  0.73;
  bg_image_data.theta = 27;
  
  
  PointAlgorithm(&robot_status, &bg_image_data);

  return 0;
}
