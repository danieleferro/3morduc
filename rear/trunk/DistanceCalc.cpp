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
      std::cout << "Calculates starts with: " << std::endl;
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
  line_b[M0] = tan (TO_RADIANS( - robot_status -> theta ));

  /* calculate parameters for line "a" */
  line_a[X0] = robot_status->x;
  line_a[Y0] = robot_status->y;
  line_a[M0] = - 1 / line_b[M0];

  /* calculate parameters for line "c" */
  gamma =  Normalize180( - robot_status -> theta - ( 360 - _sweep_angle ));
  line_c[X0] = robot_status->x;
  line_c[Y0] = robot_status->y;
  line_c[M0] = tan( TO_RADIANS( gamma ));

  if (DEBUG) std::cout << "DEBUG: coefficiente angolare retta c: " 
		       << tan ( TO_RADIANS( gamma )) 
		       << std::endl;
  /* calculate parameters for line "d" */
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
  
  /* let's exclude images that do not fall within the area
   * identified by the _sweep_angle 
   */
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
}

bool SweepMetricCalc::RightlyOriented(robot_data * robot_status,
				      image_data * bg_image_data)
{

  float offset = fabs( Normalize180( robot_status -> theta ) -
		       Normalize180( bg_image_data -> theta ));

  std:: cout << "*******************OFFSET: " << offset << std::endl;

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
