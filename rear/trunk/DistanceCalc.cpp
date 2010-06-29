#include "DistanceCalc.h"

float SpacialMetricCalc::Calculate(robot_data * robot_status,
				   image_data * bg_image_data)
{
  float distance;
  float score;
  float optimal_distance = 20;

  distance = 
    sqrt( pow((robot_status -> x ) - (bg_image_data -> x), 2) +
	  pow((robot_status -> y ) - (bg_image_data -> y), 2) +
	  pow((robot_status -> theta ) - (bg_image_data -> theta), 2));


  if ( distance <= optimal_distance )
    score = distance / optimal_distance;
 else
    score = - ( distance - 2 * optimal_distance) / optimal_distance;

  return (- score);
}

SweepMetricCalc::SweepMetricCalc( float sweep_angle,
				  float angle_offset,
				  float mu_distance,
				  float sigma_distance,
				  float mu_angle,
				  float sigma_angle
				  )
{
  _sweep_angle = sweep_angle;
  _angle_offset = angle_offset;

  _mu_distance = mu_distance;
  _sigma_distance = sigma_distance;
  _mu_angle = mu_angle;
  _sigma_angle = sigma_angle;
}

float SweepMetricCalc::Calculate( robot_data * robot_status,
				  image_data * bg_image_data)
{
  if (DEBUG)
    {
      std::cout << std::endl;
      std::cout << "*** Calculates starts with: " << std::endl;
      std::cout << "Robot position: (" 
		<< robot_status -> x << ", "
		<< robot_status -> y << ", "
		<< robot_status -> theta << ")"
		<< std::endl;

      std::cout << "Camera position: (" 
		<< bg_image_data -> x << ", "
		<< bg_image_data -> y << ", "
		<< bg_image_data -> theta << ")"
		<< std::endl;
    }

  if ((robot_status -> x == bg_image_data -> x) &&
      (robot_status -> y == bg_image_data -> y) &&
      (robot_status -> theta == bg_image_data -> theta))
    return EGO_IMAGE;
  
  if (DEBUG) std::cout << "With Boundaries test in progress.." 
		       << std::endl;

  if ( !WithinBoundaries(robot_status, bg_image_data) )
    return IMAGE_NOT_VALID;
  
  if (DEBUG) std::cout << "Rightly Oriented test in progress.." 
		       << std::endl;

  if ( !RightlyOriented(robot_status, bg_image_data) )
    return IMAGE_NOT_VALID;
  
  // now calculates score for the image
  return - PointAlgorithm(robot_status, bg_image_data);
}

float SweepMetricCalc::PointAlgorithm( robot_data * robot_status,
				       image_data * bg_image_data)
{
  float angle;
  float score_angle;

  float distance;
  float score_distance;

  angle    = fabs ( robot_status->theta - bg_image_data->theta );
  distance = sqrt (
		   pow( ( bg_image_data->x - robot_status->x), 2 ) +
		   pow( ( bg_image_data->y - robot_status->y), 2 ) );

  /* calculate distance score with gaussian function */
  score_distance = ( 1 / ( _sigma_distance * sqrt ( 2 * M_PI ) ) );
  score_distance = score_distance * exp( - pow( ( distance - _mu_distance ), 2) / ( 2 * pow ( _sigma_distance, 2 ) ) );


  /* calculate angle  score with gaussian function */
  score_angle = ( 1 / ( _sigma_angle * sqrt ( 2 * M_PI ) ) );
  score_angle = score_angle * exp( - pow( ( angle - _mu_angle ), 2) / ( 2 * pow ( _sigma_angle, 2 ) ) );


  //  return ( score_distance + score_angle );
  return score_distance;
}

bool SweepMetricCalc::WithinBoundaries( robot_data * robot_status, 
					image_data * bg_image_data)
{
  float _radius = 100;

  /* translate all system in robot_status->x and 
     robot_status->y; at the end the system is translated again */
  float x_zero = robot_status->x;
  float y_zero = robot_status->y;

  std::cout << "a" << std::endl;
  robot_status->x = 0;
  robot_status->y = 0;
  std::cout << "b" << std::endl;
    
  bg_image_data->x = bg_image_data->x - x_zero;
  bg_image_data->y = bg_image_data->y - y_zero;

//   robot_status->theta = robot_status->theta + 90;
//   bg_image_data->theta = bg_image_data->theta + 90;
  


  // theta must be always between [-180; 180]
  robot_status->theta = Normalize180(robot_status->theta);
  bg_image_data->theta = Normalize180(bg_image_data->theta);


  float ang_coeff_line_a;
  float ang_coeff_line_b;
  float ang_coeff_line_c;
  float ang_coeff_line_d;

  float rhs;
  int sign = 1;

  std::cout << "Calcolo coeff a .. " << std::endl;
  /* angular coefficient for line "a" */
  ang_coeff_line_a = tan( TO_RADIANS(robot_status->theta + 90) );

  /* angular coefficient for line "b" */
  ang_coeff_line_b = tan( TO_RADIANS(robot_status->theta) );

  /* angular coefficient for line "c" */
  ang_coeff_line_c = tan( atan( ang_coeff_line_b ) + TO_RADIANS( _sweep_angle ) );

  /* angular coefficient for line "d" */
  ang_coeff_line_d = tan( atan( ang_coeff_line_b ) - TO_RADIANS( _sweep_angle ) );

  if (DEBUG)
    {
      std::cout << "Angular coefficient line A: " << ang_coeff_line_a << std::endl;
      std::cout << "Angular coefficient line B: " << ang_coeff_line_b << std::endl;
      std::cout << "Angular coefficient line C: " << ang_coeff_line_c << std::endl;
      std::cout << "Angular coefficient line D: " << ang_coeff_line_d << std::endl;
    }

  /* find poit A and B to form AOB triangle */
  // A point (retrieved from line C)
  float A_x1 = sqrt( pow(_radius, 2) / ( pow( ang_coeff_line_c , 2) + 1 ) );
  float A_y1 = ang_coeff_line_c * A_x1;
  
  float A_x2 = -1 * A_x1;
  float A_y2 = ang_coeff_line_c * A_x2;
  
  if (DEBUG)
    {
      std::cout << "A_x1: " << A_x1 << "; "
		<< "A_y1: " << A_y1
		<< std::endl << std::endl;
      
      std::cout << "A_x2: " << A_x2 << "; "
		<< "A_y2: " << A_y2
		<< std::endl << std::endl;
    }
    

  /* find poit A and B to form AOB triangle */
  // B point (retrieved from line D)
  float B_x1 = sqrt( pow(_radius, 2) / ( pow( ang_coeff_line_d , 2) + 1 ) );
  float B_y1 = ang_coeff_line_d * B_x1;

  float B_x2 = -1 * B_x1;
  float B_y2 = ang_coeff_line_d * B_x2;

  if (DEBUG)
    {
      std::cout << "B_x1: " << B_x1 << "; "
		<< "B_y1: " << B_y1
		<< std::endl << std::endl;
      
      std::cout << "B_x2: " << B_x2 << "; "
		<< "B_y2: " << B_y2
		<< std::endl << std::endl;
    }

  if ( robot_status->theta >= 0 && robot_status->theta <= 180 )
    sign = -1;

  if ( robot_status->theta >= -180 && robot_status->theta < 0 )
    sign = 1;
   

  // find out which couple values are right (point A)
  float A_x;
  float A_y;

  rhs = ang_coeff_line_b * A_x1;
  
  if (DEBUG)
    std::cout << sign << " * " << A_y1 << " >= " << rhs << " * " << sign << std::endl;

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
    

  // find out which couple values are right (point B)
  if ( robot_status->theta == 0)
    sign = 1;
  else {
    
    if ( robot_status->theta > 0 && robot_status->theta <= 180 )
      {
	sign = 1;
	std::cout << "Robot theta in ]0; 180] " << std::endl;
      }
    
    else 
      {
    
	if ( robot_status->theta >= -180 && robot_status->theta < 0 )
	  sign = -1;
      }

  }
  
  float B_x;
  float B_y;
  float B_z = 0;
  
  
  rhs = ang_coeff_line_b * B_x1;  
  
  if (DEBUG)
    std::cout << sign << " * " << B_y1 << " <= " << rhs << " * " << sign << std::endl;

  if ( B_y1*sign <= rhs*sign )
    {

      B_x = B_x1;
      B_y = B_y1;

    }
  else
    {

      B_x = B_x2;
      B_y = B_y2;

    }
    
  std::cout << "point A chosen: " << A_x << " ; " << A_y << std::endl;
  std::cout << "point B chosen: " << B_x << " ; " << B_y << std::endl;

  fPoint pt;
  pt.y = bg_image_data->x;
  pt.x = bg_image_data->y;

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


//   robot_status->theta = robot_status->theta - 90;
//   bg_image_data->theta = bg_image_data->theta - 90;
  

  if (flag) 
    {
      std::cout << "Point ( "
	   << bg_image_data->x - x_zero << "; "
	   << bg_image_data->y - y_zero << "; " 
	   << bg_image_data->theta << " )";
      
      std::cout << " is included." << std::endl;
           
    }
  else
    {
      std::cout << "Point ( "
	   << bg_image_data->x - x_zero << "; "
	   << bg_image_data->y - y_zero << "; " 
	   << bg_image_data->theta << " )";
      
      std::cout << " is NOT included." << std::endl;
    }

  std::cout << "Robot angle: " << robot_status->theta << " deegres." << std::endl;


  return flag;


  /*
  float line_a[3];
  float line_b[3];
  float line_c[3];
  float line_d[3];

  float gamma;
  float delta;

  float rhs_c;
  float rhs_d;

  // calculate parameters for line "b"
  line_b[X0] = robot_status->x;
  line_b[Y0] = robot_status->y;
  line_b[M0] = tan (TO_RADIANS( - robot_status -> theta ));

  // calculate parameters for line "a" 
  line_a[X0] = robot_status->x;
  line_a[Y0] = robot_status->y;
  line_a[M0] = - 1 / line_b[M0];

  // calculate parameters for line "c" 
  gamma =  Normalize180( - robot_status -> theta - ( 360 - _sweep_angle ));
  line_c[X0] = robot_status->x;
  line_c[Y0] = robot_status->y;
  line_c[M0] = tan( TO_RADIANS( gamma ));

  if (DEBUG) std::cout << "DEBUG: coefficiente angolare retta c: " 
		       << tan ( TO_RADIANS( gamma )) 
		       << std::endl;

  // calculate parameters for line "d"
  delta = Normalize180(- robot_status -> theta + ( 360 - _sweep_angle ));
  line_d[X0] = robot_status->x;
  line_d[Y0] = robot_status->y;
  line_d[M0] = tan( TO_RADIANS( delta ));

  if (DEBUG) std::cout << "DEBUG: coefficiente angolare d: " 
		       << tan ( TO_RADIANS( delta )) 
		       << std::endl;

  if (DEBUG)
    {
      std::cout << "Gamma: " << gamma << std::endl;
      std::cout << "Delta: " << delta << std::endl;
    }

  if (DEBUG)
    {
      std::cout << "Point ( "
		<< bg_image_data->x << "; "
		<< bg_image_data->y << "; " 
		<< bg_image_data->theta << " )"
		<< std::endl;
    }
  
//   let's exclude images that do not fall within the area
//    * identified by the _sweep_angle
  rhs_d = line_d[M0] * bg_image_data->y + ( line_d[Y0] - line_d[M0] * line_d[X0] );
  rhs_c = line_c[M0] * bg_image_data->x + ( line_c[Y0] - line_c[M0] * line_c[X0] );

  if (DEBUG) std::cout << "rhs_c : " << rhs_c << std::endl;
  if (DEBUG) std::cout << "y : " << bg_image_data -> x << std::endl;
  if ( gamma >= -90 && gamma <= 90 )
    {
      if (DEBUG) std::cout << " y <= rhs_c is being evaluated " 
			   << std::endl;
      if ( bg_image_data->y <= rhs_c )
	{
	  if (DEBUG) std::cout << " is EXcluded." << std::endl;
	  return false;
	}
    }

  else
    {
      if (DEBUG) std::cout << " y >= rhs_c is being evaluated " 
			   << std::endl;
      if ( bg_image_data->y >= rhs_c )
	{
	  if (DEBUG) std::cout << " is EXcluded." << std::endl;
	  return false;
	}
    }

  if (DEBUG) std::cout << "rhs_d : " << rhs_d << std::endl;
  if (DEBUG) std::cout << "y : " << bg_image_data -> x << std::endl;
  if ( delta >= -90 && delta <= 90 )
    {
      if (DEBUG) std::cout << " y >= rhs_d is being evaluated " 
      			   << std::endl;
      if ( bg_image_data->y >= rhs_d )
	{
	  if (DEBUG) std::cout << " is EXcluded." << std::endl;
	  return false;
	}
    }

  else
    {
           if (DEBUG) std::cout << " y <= rhs_d is being evaluated " 
      		   << std::endl;
      if ( bg_image_data->y <= rhs_d )
	{
	  if (DEBUG) std::cout << " is EXcluded." << std::endl;
	  return false;
	}
    }
 
  if (DEBUG) std::cout << " is INcluded." << std::endl;
  return true;
  
  */
}

bool SweepMetricCalc::RightlyOriented(robot_data * robot_status,
				      image_data * bg_image_data)
{

  if (DEBUG)
    {
      std::cout << "Robot orientation: " << robot_status->theta << std::endl;
      std::cout << "Camera orientation: " << bg_image_data->theta << std::endl;
    }

  float offset = fabs( Normalize180( robot_status -> theta ) -
		       Normalize180( bg_image_data -> theta ));

  if ( offset >= _angle_offset )
    return false;

  return true;
}

float SweepMetricCalc::Normalize180(float angle)
{
  if (angle > 180)
    return Normalize180(angle - 360);
    
  if (angle < -180)
    return Normalize180(angle + 360);
    
  return angle;
}

// float SweepMetricCalc::Normalize180(float angle_deegres)
// {
//   if (angle_deegres >= 0 && angle_deegres <= 180)
//     return angle_deegres;

//   if (angle_deegres >= -180 && angle_deegres <= 0)
//     return angle_deegres;
    
 
//   return Normalize180(angle_deegres-360);
// }


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
