#include "DataManager.h"
#define EPSILON 0.00001

DataManager::DataManager(Robot * robot, DataLogic * logic, Camera * camera, 
			 DistanceCalcInterface * calculator)
{
  _rob = robot;
  _logic = logic;
  _camera = camera;
  _calculator = calculator;

  _robot_status = (robot_data *) malloc(sizeof(robot_data));
  _bg_image_data = (image_data *) malloc(sizeof(image_data));

  if (_robot_status == NULL)
    std::cout << "Error 1" << std::endl;

  if(_bg_image_data == NULL)
    std::cout << "Error 2" << std::endl;

//   prev_x = 0;
//   prev_y = 0;
//   prev_theta = 0;

  //NextStep(true);

  _logic->RetrieveData(_robot_status);

  _rob->Place(_robot_status->x,
	      _robot_status->y,
	      _robot_status->theta);

  _logic->SelectImage(_robot_status, _bg_image_data,
		      _calculator);

  _camera -> SetPosition( _bg_image_data -> x,
			  9.f,
			  _bg_image_data -> y);

  _camera -> SetViewDir( 0.f,
			 0.f,
			 1.f);

  _camera -> RotateY(- 90);

  LoadGLTextures(_texture, _bg_image_data->path);
}

DataManager::~DataManager()
{
  free(_robot_status);
  free(_bg_image_data);
}

void DataManager::NextStep(bool first_time) {

  image_data old_image;
  old_image.x = _bg_image_data -> x;
  old_image.y = _bg_image_data -> y;
  old_image.theta = _bg_image_data -> theta;
  
  _logic->RetrieveData(_robot_status);

  // move robot with _robot_status data
  _rob->Place(_robot_status->x,
	      _robot_status->y,
	      _robot_status->theta);
    
  _logic->SelectImage(_robot_status, _bg_image_data,
		      _calculator);

  /* use data in out_element to change
     camera position
  */   
  std::cout << old_image.x << std::endl;
  std::cout << _bg_image_data -> x << std::endl;

  if ( old_image.x != _bg_image_data -> x ||
       old_image.y != _bg_image_data -> y )
    {
      std::cout << "********************Moving the camera..." << std::endl;
      std::cout <<  _bg_image_data->theta - old_image.theta << std::endl;
      _camera -> RotateY( _bg_image_data->theta - old_image.theta);
      _camera -> SetPosition( _bg_image_data -> x,
			      9.f,
			      _bg_image_data -> y);
    }

  /* use image_path to load the image */
  LoadGLTextures(_texture, _bg_image_data->path);
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


void DataManager::MoveCamera() {

//   _camera->Move( F3dVector( _bg_image_data->x - prev_x,
// 			    0.0,
// 			    _bg_image_data->y - prev_y) );
  
  // rotation in deegre
//   _camera->RotateY( _bg_image_data->theta - prev_theta);
  
//   prev_x = _bg_image_data->x;
//   prev_y = _bg_image_data->y;
//   prev_theta = _bg_image_data->theta;
  
  
  std::cout << "Camera in: \t"
	    << _bg_image_data->x << "; "
	    << _bg_image_data->y << "; "
	    << _bg_image_data->theta << std::endl;
}
