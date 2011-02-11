#include "AnotherSweepMetricCalc.h"

AnotherSweepMetricCalc::AnotherSweepMetricCalc( float sweep_angle,
				  float angle_offset,
				  float mu_distance,
				  float sigma_distance,
				  float mu_angle,
				  float sigma_angle
				  )
{

  if (sweep_angle < 0 || sweep_angle > 90)
    {
      std::cout << "Error ! \"sweep angle\" value must be in range ]0; 90[. Value assigned is 45 degrees."
		<< std::endl;
      
      _sweep_angle = 45;

    }
  else
    {
      _sweep_angle = sweep_angle;
    }

  _angle_offset = angle_offset;

  _mu_distance = mu_distance;
  _sigma_distance = sigma_distance;
  _mu_angle = mu_angle;
  _sigma_angle = sigma_angle;

  /* set radius with a large value
     to obtain a better approximation with
     the sweep area
   */
  _radius = 100;


  /* set A and B point, depending on
     radius and sweep angle
  */
  _A = (fPoint*) malloc(sizeof(fPoint));
  _B = (fPoint*) malloc(sizeof(fPoint));
  _O = (fPoint*) malloc(sizeof(fPoint));
  
  FindTriangleVerteces(_sweep_angle, _radius);

}

float AnotherSweepMetricCalc::Calculate(robot_data * robot_status, image_data * bg_image_data) {

  if (DEBUG)
    {
      std::cout << std::endl;
      std::cout << "*** Calculates starts with: " << std::endl;
      std::cout << "Robot position: (" 
		<< robot_status -> x << ", "
		<< robot_status -> y << ", "
		<< robot_status -> theta << ")"
		<< std::endl;

      std::cout << "Image position: (" 
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


void AnotherSweepMetricCalc::ChooseImage( robot_data * robot_status, image_data * bg_image_data,
				   std::vector<image_data> * _images_collection)
{


  float distances[_images_collection->size()];
  float min;

  // calculate the distance for each stored image
  int i = 0;
  for (std::vector<image_data>::iterator it =
	 _images_collection->begin();
       it != _images_collection->end();
       it++)
    {
      distances[i] = Calculate(robot_status, &*it);
      //      std::cout << "Return value from Calculate Function: " << distances[i] << std::endl;
      i++;
    }

  for (int i = 0; i < _images_collection->size(); i ++)
    std::cout << "distance[" << i << "]: "
	      << distances[i]
	      << std::endl;

  // find the minimum distance
  i = 0;
  min = distances[0];
  for (int j = 1; j < _images_collection->size(); j++)
    {
      if (distances[j] < min)
	{
	  i = j;
	  min = distances[j];
	}
    }

  // return the selected image data
  bg_image_data->x = (*_images_collection)[i].x;
  bg_image_data->y = (*_images_collection)[i].y;
  bg_image_data->theta = (*_images_collection)[i].theta;
  bg_image_data->time = (*_images_collection)[i].time;

  strcpy(bg_image_data->path, (*_images_collection)[i].path);

  // std::cout << "----> " << bg_image_data->path << "<-----" << std::endl;

}



void AnotherSweepMetricCalc::FindTriangleVerteces(float sweep_angle, float radius)
{

  /*
    A and B point are calculated by the intersection between
    the "c" and "d" line with the circle centerd in (0,0)
    see documentation for more details
  */

  float ang_coeff_line_c;
  float ang_coeff_line_d;

  /*
    variables to store temporary values
    of A point and the final one
  */
  float A_x1, A_y1;
  float A_x2, A_y2;
  float A_x,  A_y;

  /*
    variables to store temporary values
    of A point and the final one
  */  
  float B_x1, B_y1;
  float B_x2, B_y2;
  float B_x,  B_y;
  

  // angular coefficient for line "c" 
  ang_coeff_line_c = tan( M_PI/2 + TO_RADIANS( _sweep_angle ) );

  // angular coefficient for line "d"
  ang_coeff_line_d = tan( M_PI/2 - TO_RADIANS( _sweep_angle ) );

  if (DEBUG)
    {
      std::cout << "Angular coefficient line C: " << ang_coeff_line_c << std::endl;
      std::cout << "Angular coefficient line D: " << ang_coeff_line_d << std::endl;
    }

  /*
    find poit A and B to form AOB triangle
    A point (retrieved from intersection between line "c"
    and circle centered in (0,0))
  */
  A_x1 = sqrt( pow(_radius, 2) / ( pow( ang_coeff_line_c , 2) + 1 ) );
  A_y1 = ang_coeff_line_c * A_x1;
  
  A_x2 = -1 * A_x1;
  A_y2 = ang_coeff_line_c * A_x2;
  
  if (DEBUG)
    {
      std::cout << "A_x1: " << A_x1 << "; "
		<< "A_y1: " << A_y1
		<< std::endl << std::endl;
      
      std::cout << "A_x2: " << A_x2 << "; "
		<< "A_y2: " << A_y2
		<< std::endl << std::endl;
    }
    

  /*
    find poit A and B to form AOB triangle
    B point (retrieved from intersection between line "d"
    and circle centered in (0,0))
  */
  B_x1 = sqrt( pow(_radius, 2) / ( pow( ang_coeff_line_d , 2) + 1 ) );
  B_y1 = ang_coeff_line_d * B_x1;

  B_x2 = -1 * B_x1;
  B_y2 = ang_coeff_line_d * B_x2;

  if (DEBUG)
    {
      std::cout << "B_x1: " << B_x1 << "; "
		<< "B_y1: " << B_y1
		<< std::endl << std::endl;
      
      std::cout << "B_x2: " << B_x2 << "; "
		<< "B_y2: " << B_y2
		<< std::endl << std::endl;
    }

  /* find out which couple values are right (point A)  */
  if ( A_y1 < 0 )
    {
      A_x = A_x1;
      A_y = A_y1;

    }
  else
    {

      A_x = A_x2;
      A_y = A_y2;

    }
    

  /* find out which couple values are right (point B)  */  
  if ( B_y1 < 0 )
    {

      B_x = B_x1;
      B_y = B_y1;

    }
  else
    {

      B_x = B_x2;
      B_y = B_y2;

    }
    
  if (DEBUG) 
    {
      std::cout << "point A chosen: " << A_x << " ; " << A_y << std::endl;
      std::cout << "point B chosen: " << B_x << " ; " << B_y << std::endl;
    }

  /* copying values */
  _A->x = A_x;
  _A->y = A_y;
  
  _B->x = B_x;
  _B->y = B_y;

  _O->x = 0;
  _O->y = 0;

  
  return;

} 


float AnotherSweepMetricCalc::PointAlgorithm( robot_data * robot_status,
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


bool AnotherSweepMetricCalc::WithinBoundaries( robot_data * robot_status, 
					image_data * bg_image_data)
{
  float temp_x;
  float temp_y;
  
  fPoint pt;
  bool flag;

  /* translate system to move robot in origin center */
  temp_x = bg_image_data->x - robot_status->x;
  temp_y = bg_image_data->y - robot_status->y;

  /* rotate system to overlap robot orientation arrow with the Y-axis,
     exchange X axis with Y and viceversa
  */
  pt.y = temp_x * cos(TO_RADIANS(robot_status->theta)) -
    temp_y * sin(TO_RADIANS(robot_status->theta));
							   
  pt.x = temp_x * sin(TO_RADIANS(robot_status->theta)) +
    temp_y * cos(TO_RADIANS(robot_status->theta));

  
  if (DEBUG)
    {
      std::cout << "--> New values for image: ( "
		<< pt.y << "; "
		<< pt.x << " )" 
		<< std::endl;
    }


  flag = IsPointInTri(&pt, _O, _A, _B);
  

  if (DEBUG) 
    {
      if (flag) 
	{
	  std::cout << "Point ( "
		    << bg_image_data->x << "; "
		    << bg_image_data->y << "; " 
		    << bg_image_data->theta << " )";
	  
	  std::cout << " is included." << std::endl;
	  
	}
      else
	{
	  std::cout << "Point ( "
		    << bg_image_data->x << "; "
		    << bg_image_data->y << "; " 
		    << bg_image_data->theta << " )";
	  
	  std::cout << " is NOT included." << std::endl;
	}
      
      std::cout << "Robot angle: " << robot_status->theta << " deegres." << std::endl;
    }
      

  return flag;

}


bool AnotherSweepMetricCalc::IsPointInTri(fPoint * pt, fPoint * v1, fPoint * v2, fPoint * v3)
{
  bool b1, b2, b3;

  b1 = Sign(pt, v1, v2) < 0.0f;
  b2 = Sign(pt, v2, v3) < 0.0f;
  b3 = Sign(pt, v3, v1) < 0.0f;

  return ((b1 == b2) && (b2 == b3));
}


float AnotherSweepMetricCalc::Sign(fPoint * p1, fPoint * p2, fPoint * p3)
{
  return (p1->x - p3->x) * (p2->y - p3->y) - (p2->x - p3->x) * (p1->y - p3->y);
}


bool AnotherSweepMetricCalc::RightlyOriented(robot_data * robot_status,
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


float AnotherSweepMetricCalc::Normalize180(float angle)
{
  if (angle > 180)
    return Normalize180(angle - 360);
    
  if (angle < -180)
    return Normalize180(angle + 360);
    
  return angle;
}
