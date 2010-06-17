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


  if (_robot_status == NULL)
    std::cout << "Error 1" << std::endl;

  if(_bg_image_data == NULL)
    std::cout << "Error 2" << std::endl;


  prev_x = 0;
  prev_y = 0;
  prev_theta = 0;

  NextStep();
}

DataManager::~DataManager()
{
  free(_robot_status);
  free(_bg_image_data);
}

void DataManager::NextStep() {
  
  _logic->RetrieveData(_robot_status);

  // move robot with _robot_status data
  _rob->Place(_robot_status->x,
	      _robot_status->y - 20,
	      _robot_status->theta); 

  //_rob->Place(40.f, -70.45f - 30, 0.f);

  _logic->SelectImage(_robot_status, _bg_image_data,
		      _calculator);

  /* use image_path to load the image */
  LoadGLTextures(_texture, _bg_image_data->path);
      
  /* use data in out_element to change
     camera position
  */   
  MoveCamera();

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


  _camera->Move( F3dVector( _bg_image_data->x - prev_x,
			    0.0,
			    _bg_image_data->y - prev_y) );

  // rotation in deegre
  _camera->RotateY( _bg_image_data->theta - prev_theta);

  prev_x = _bg_image_data->x;
  prev_y = _bg_image_data->y;
  prev_theta = _bg_image_data->theta;



  std::cout << "Camera in: \t"
	    << _bg_image_data->x << "; "
	    << _bg_image_data->y << "; "
	    << _bg_image_data->theta << std::endl;


}
