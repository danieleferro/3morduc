#include "DataManager.h"

DataManager::DataManager(Robot * robot, DataLogic * logic)
{
  _rob = robot;
  _logic = logic;
  _robot_status = (robot_data *) malloc(sizeof(robot_data));
  _bg_image_data = (image_data *) malloc(sizeof(image_data));
  _bg_image_data->path = new std::string();

  if (_robot_status == NULL)
    std::cout << "Error 1" << std::endl;

  if(_bg_image_data == NULL)
    std::cout << "Error 2" << std::endl;
  
  /* do the first step */
  NextStep();
}

DataManager::~DataManager()
{
  free(_robot_status);
  free(_bg_image_data);
}

void DataManager::NextStep() {
  
  std::string image_path;

  _logic->RetrieveData(_robot_status);
  _logic->SelectImage(_robot_status, _bg_image_data);
 
  /* use image_path to load the image */
  LoadGLTextures(_texture, *(_bg_image_data->path));
      
  /* use data in out_element to change
     camera position
  */
  //       MoveCamera(out_element);
    
}

    
void DataManager::LoadGLTextures(GLuint * texture, std::string filename) {	

  /* load image from png file */
  * texture = loadImage(filename.c_str());
  if (!*texture) {
    std::cout << "Texture value: " << *texture << std::endl;
    exit(1);
  }        
   
  // Bind 2d texture (x and y size)
  glBindTexture(GL_TEXTURE_2D, *texture);   

}


void DataManager::MoveCamera(robot_data delta) {

  /* use data in out_element to change
     camera position
     (only one function make a change)
  */
  glTranslatef(- delta.x,
	       0.f,
	       - delta.y);

  glRotatef(delta.theta,
	    0.f, 1.f, 0.f);

}
