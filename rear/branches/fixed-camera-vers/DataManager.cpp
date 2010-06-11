#include "DataManager.h"

DataManager::DataManager(Robot * robot, DataLogic * logic)
{
  _rob = robot;
  _logic = logic;
  _robot_status = (robot_data *) malloc(sizeof(robot_data));
  _bg_image_data = (image_data *) malloc(sizeof(image_data));

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

  robot_data old_robot_status, robot_delta;
  image_data old_image_data, image_delta;
  robot_data camera_moving_vector;
  

  // save old robot and image data
  old_robot_status.x = _robot_status -> x;
  old_robot_status.y = _robot_status -> y;
  old_robot_status.theta = _robot_status -> theta;

  old_image_data.x = _bg_image_data -> x;
  old_image_data.y = _bg_image_data -> y;
  old_image_data.theta = _bg_image_data -> theta;

  // retrieve new data and select new bg image
  _logic->RetrieveData(_robot_status);
  _logic->SelectImage(_robot_status, _bg_image_data);
 
  /* use image_path to load the image */
  LoadGLTextures(_texture, _bg_image_data->path);
  
  // compute deltas
  robot_delta.x = _robot_status -> x - old_robot_status.x;
  robot_delta.y = _robot_status -> y - old_robot_status.y;
  robot_delta.theta = 
    _robot_status -> theta - old_robot_status.theta;

  image_delta.x = _bg_image_data -> x - old_image_data.x;
  image_delta.y = _bg_image_data -> y - old_image_data.y;
  image_delta.theta = 
    _bg_image_data -> theta - old_image_data.theta;

  camera_moving_vector.x = robot_delta.x - image_delta.x;
  camera_moving_vector.y = robot_delta.y - image_delta.y;
  camera_moving_vector.theta =
    robot_delta.theta - image_delta.theta;
  
  /* use data in out_element to change
     camera position
  */
  
  std::cout << "Camera has moved by " << camera_moving_vector.x 
	    << " " 
	    << camera_moving_vector.y 
	    << " " << camera_moving_vector.theta << std::endl;

//   robot_data * delta = (robot_data *) malloc(sizeof(robot_data));
//   delta -> x = 40.f;
//   delta -> y = 0.f;
//   delta -> theta = 0.f;

  MoveCamera(camera_moving_vector);
//  MoveCamera(*delta);
  return;
}

void DataManager::LoadGLTextures(GLuint * texture, const char* filename) {	

  /* load image from png file */
  * texture = loadImage(filename);
  if (!*texture) {
    std::cout << "Texture value: " << *texture << std::endl;
    exit(1);
  }        
   
  // Bind 2d texture (x and y size)
  glBindTexture(GL_TEXTURE_2D, * texture);   

}


void DataManager::MoveCamera(robot_data delta) {

  /* use data in out_element to change
     camera position
     (only one function make a change)
  */

  std::cout << "Rotating robot of: "
	    << delta.theta << std::endl;

  glRotatef( - delta.theta * 180 / M_PI,
	     0.f, 1.f, 0.f);

  std::cout << "Moving robot of: "
	    << delta.x << " "
	    << delta.y << std::endl;

  glTranslatef( delta.x,
		0.f,
		delta.y);
  
//  _rob->Place(delta.x, delta.y, delta.theta);

}
