#include "DataManager.h"

DataManager::DataManager(Robot * robot)
{
  mem = GetNewPosition(1);
  index = 2;
  this->rob = robot;

}


void DataManager::NextStep() {

  actual_robot_status new_status;
  actual_robot_status delta;
  
  new_status = GetNewPosition(index);
  index++;

  delta.x     = new_status.x - mem.x;
  delta.y     = new_status.y - mem.y;
  delta.theta = new_status.theta - mem.theta;
  delta.time  = new_status.time - mem.time;

  /*
    std::cout << delta.x << std::endl;
    std::cout << delta.y << std::endl;
    std::cout << delta.theta << std::endl;
    std::cout << delta.time << std::endl;
  */

  /* check if robot changed its position */
  if (fabs(delta.x) <= 0.001f &&
      fabs(delta.y) <= 0.001f &&
      fabs(delta.theta) <= 0.001f)
    {
      
      std::cout << "Robot did not change its position." << std::endl;
      return;
    }


  rob->Place(delta.x + rob->GetX(),
	    delta.y + rob->GetY(),
	    delta.theta + rob->GetTheta() );

  mem = new_status;

  
}


actual_robot_status DataManager::GetNewPosition(int line_number)
{
  FILE * position_data = fopen("data.txt","rt");
  char line[50];
  std::string * line_read;
  std::string line_values[4];
  
  actual_robot_status out_value;


  
  while(fgets(line, 50, position_data) &&
	line_number > 1)
    {
      line_number--;
    }

  line_read = new std::string(line);

  std::string buf;
  std::stringstream ss(*line_read);
  // Create vector to hold our words
  std::vector<std::string> tokens;
  
  // put token in vector element
  while (ss >> buf)
    tokens.push_back(buf);

  out_value.x = atof ( tokens[1].c_str() );
  out_value.y = - atof ( tokens[0].c_str() );
  out_value.theta = atof ( tokens[2].c_str() );
  out_value.time = atof ( tokens[3].c_str() );

  return out_value;
  
}
