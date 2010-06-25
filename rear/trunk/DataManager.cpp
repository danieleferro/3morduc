#include "DataManager.h"

DataManager::DataManager(Robot * robot, DataLogic * logic, Camera * camera, 
			 DistanceCalcInterface * calculator)
{
  _rob = robot;
  _logic = logic;
  _camera = camera;
  _calculator = calculator;

  _robot_status = (robot_data *) malloc(sizeof(robot_data));
  _bg_image_data = (image_data *) malloc(sizeof(image_data));

  if ( _robot_status == NULL ||
       _bg_image_data == NULL )
    {
      std::cout << "DataManager: failed to allocate memory \
                    for internal data." 
		<< std::endl << "exiting." << std::endl;
      exit(-1);
    }

  NextStep();
}

DataManager::~DataManager()
{
  free(_robot_status);
  free(_bg_image_data);
}

void DataManager::NextStep() {

  image_data old_image;

  old_image.x = _bg_image_data -> x;
  old_image.y = _bg_image_data -> y;
  old_image.theta = _bg_image_data -> theta;
  
  _logic->RetrieveData(_robot_status);

  // move robot with _robot_status data
  // theta in degrees
  _rob->Place(_robot_status->x,
	      _robot_status->y,
	      _robot_status->theta ); 
    
  _logic->SelectImage(_robot_status, _bg_image_data,
		      _calculator);

  /* use data in out_element to change
     camera position
  */   

  if ( old_image.x != _bg_image_data -> x ||
       old_image.y != _bg_image_data -> y ||
       old_image.theta != _bg_image_data -> theta )
    {
      std::cout << "*** Moving the camera... ***" << std::endl;
      std::cout <<  _bg_image_data->theta - old_image.theta << std::endl;

      _camera -> SetPosition( _bg_image_data -> x,
			      9.f,
			      _bg_image_data -> y);
      
      _camera -> SetYAngle( _bg_image_data -> theta - 90);
    }

  /* use image_path to load the image */
  LoadGLTextures(_texture, _bg_image_data->path);
  std::cout << std::endl << std::endl;
}

    
void DataManager::LoadGLTextures(GLuint * texture, const char* filename) {	

  /* load image from png file */
  * texture = loadImage(filename);
  if (!*texture) {
    std::cout << "Texture value: " << *texture << std::endl;
    exit(1);
  }        
   
  // Bind 2d texture (x and y size)
  glBindTexture(GL_TEXTURE_2D, *texture);   

}