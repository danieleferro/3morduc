#include "DataManager.h"

DataManager::DataManager(Robot * robot, int session)
{

  robot_data buffer;
  this->session = session;


  buffer = GetNewData(1);

  mem = buffer.position;
  actual_image = buffer.image_path;

  index = 2;
  this->rob = robot;

  LoadGLTextures(texture, "screenshot.png");

}


void DataManager::NextStep() {

  robot_data new_status;
  actual_robot_status delta;
  
  new_status = GetNewData(index);
  index++;

  delta.x     = new_status.position.x - mem.x;
  delta.y     = new_status.position.y - mem.y;
  delta.theta = new_status.position.theta - mem.theta;
  delta.time  = new_status.position.time - mem.time;

  /*
    std::cout << delta.x << std::endl;
    std::cout << delta.y << std::endl;
    std::cout << delta.theta << std::endl;
    std::cout << delta.time << std::endl;

    std::cout << new_status.image_path << std::endl;

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

  mem = new_status.position;
  actual_image = new_status.image_path;





  /* managing queue */
  robot_data out_element;

  
  if (queue.size() < STACK_SIZE) {

    /* fill the queue */
    queue.push_back(new_status);

  }
  else {

    /* remove oldest element (first position) */
    out_element = queue[0];
    queue.erase(queue.begin());

    /* insert in tail */
    queue.push_back(new_status);

    /* use data in out_element to change
       camera position and texture */

  }

  for (std::vector<robot_data>::iterator it = queue.begin();
       it != queue.end();
       ++it) {

    std::cout << (*it).position.x << " %\t ";
    std::cout << (*it).position.y << " %\t ";
    std::cout << (*it).position.theta << " %\t ";
    std::cout << (*it).position.time << " %\t ";

    std::cout << (*it).image_path << std::endl;
  }  

  
}


robot_data DataManager::GetNewData(int line_number)
{
  FILE * position_data;
  char line[50];
  
  std::string * line_read;
  std::string line_values[4];
  std::string position_data_name;
  std::ostringstream o;
  
  int time;
  robot_data out_value;
  
  o << "../../../../rear/log/data_" << session << ".txt";
  position_data_name = o.str();


  position_data = fopen(position_data_name.c_str(), "rt");

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

  out_value.position.x = atof ( tokens[1].c_str() );
  out_value.position.y = - atof ( tokens[0].c_str() );
  out_value.position.theta = atof ( tokens[2].c_str() );
  out_value.position.time = atof ( tokens[3].c_str() );

  time = (int) out_value.position.time;

  /* clear the stream and 
     add a new value to it */
  o.str("");
  o.clear();
  o << "screenshot_" << session << "_" << time << ".png";
  out_value.image_path = o.str();

  std::cout << out_value.image_path << std::endl;

  return out_value;
  
}

    
// Load Bitmaps And Convert To Textures
void DataManager::LoadGLTextures(GLuint * texture, std::string filename) {	

  /* load image from png file */
  *texture = loadImage(filename.c_str());
  if (!*texture) {
    std::cout << "Texture value: " << *texture << std::endl;
    exit(1);
  }        
   
  // Bind 2d texture (x and y size)
  glBindTexture(GL_TEXTURE_2D, *texture);   

}
