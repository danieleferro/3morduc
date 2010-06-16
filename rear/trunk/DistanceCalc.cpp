#include "DistanceCalc.h"

float SpacialMetricCalc::Calculate(robot_data * robot_status,
				   image_data * bg_image_data)
{
  float distance;
  
  distance = 
    sqrt( pow((robot_status -> x ) - (bg_image_data -> x), 2) +
	  pow((robot_status -> y ) - (bg_image_data -> y), 2) +
	  pow((robot_status -> theta ) - (bg_image_data -> theta), 2));
  
  return distance;  
}

SweepMetricCalc::SweepMetricCalc( float sweep_angle,
				  float angle_offset)
{
  _sweep_angle = sweep_angle;
  _angle_offset = angle_offset;
}

float SweepMetricCalc::Calculate( robot_data * robot_status,
				  image_data * bg_image_data)
{
  if ( !WithinBoundaries(robot_status, bg_image_data) )
    return IMAGE_NOT_VALID;
  
  if ( !RightlyOriented(robot_status, bg_image_data) )
    return IMAGE_NOT_VALID;
  
  // now calculates score for the image
  return PointAlgorithm(robot_status, bg_image_data);
}

float SweepMetricCalc::PointAlgorithm( robot_data * robot_status,
				       image_data * bg_image_data)
{


}

bool SweepMetricCalc::WithinBoundaries( robot_data * robot_status, 
					image_data * bg_image_data)
{
  float line_a[3];
  float line_b[3];
  float line_c[3];
  float line_d[3];

  float gamma;
  float delta;

  float rhs_c;
  float rhs_d;

  /* calculate parameters for line "b" */
  line_b[X0] = robot_status->x;
  line_b[Y0] = robot_status->y;
  line_b[M0] = tan (TO_RADIANS( robot_status -> theta ));

  /* calculate parameters for line "a" */
  line_a[X0] = robot_status->x;
  line_a[Y0] = robot_status->y;
  line_a[M0] = - 1 / line_b[M0];

  /* calculate parameters for line "c" */
  gamma =  Normalize180(robot_status -> theta - ( 360 - _sweep_angle ));
  line_c[X0] = robot_status->x;
  line_c[Y0] = robot_status->y;
  line_c[M0] = tan( TO_RADIANS( gamma ));

  if (DEBUG)
    std::cout << "DEBUG: coefficiente angolare retta c: " <<
      tan ( TO_RADIANS( gamma )) <<
      std::endl;

  /* calculate parameters for line "d" */
  delta = Normalize180(robot_status -> theta + ( 360 - _sweep_angle ));
  line_d[X0] = robot_status->x;
  line_d[Y0] = robot_status->y;
  line_d[M0] = tan( TO_RADIANS( delta ));

  if (DEBUG) 
    std::cout << "DEBUG: coefficiente angolare d: " <<
      tan ( TO_RADIANS( delta )) <<
      std::endl;

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
  
  /* let's exclude images that do not fall within the area
   * identified by the _sweep_angle 
   */
  rhs_d = line_d[M0] * bg_image_data->x + ( line_d[Y0] - line_d[M0] * line_d[X0] );

  if (DEBUG) std::cout << "rhs_c : " << rhs_c << std::endl;
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
}

bool SweepMetricCalc::RightlyOriented(robot_data * robot_status,
				      image_data * bg_image_data)
{

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
