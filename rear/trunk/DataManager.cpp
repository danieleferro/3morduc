#include "DataManager.h"

DataManager::DataManager(Robot * robot, DataLogic * logic, CCamera * camera)
{
  _rob = robot;
  _logic = logic;
  _camera = camera;


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
	      _robot_status->y,
	      _robot_status->theta);
	      
  

  //_rob->Place(40.f, -70.45f - 30, 0.f);


  _logic->SelectImage(_robot_status, _bg_image_data);
 
  /* use image_path to load the image */
  LoadGLTextures(_texture, _bg_image_data->path);
      
  /* use data in out_element to change
     camera position
  */ 
  
  /*
  MoveCamera(_bg_image_data->x,
	     _bg_image_data->y,
	     _bg_image_data->theta);
  
	       
    x = _bg_image_data->x;
    
    y = _bg_image_data->y;
    
    theta = _bg_image_data->theta;

  */

  /*

  prev_x = prev_x + 0.1;
  prev_y = prev_y + 0.1;
  prev_theta = prev_theta + 0.1;

  _camera->Move( F3dVector(prev_x, 0.0, 0.0) );
  
  */


  
  _camera->Move( F3dVector( _bg_image_data->x - prev_x,
			    0.0,
			    _bg_image_data->y - prev_y) );

  // rotation in deegre
  _camera->RotateY( _bg_image_data->theta - prev_theta );

  prev_x = _bg_image_data->x;
  prev_y = _bg_image_data->y;
  prev_theta = _bg_image_data->theta;


  std::cout << "Camera in: \t"
	    << _bg_image_data->x << "; "
	    << _bg_image_data->y << "; "
	    << _bg_image_data->theta << std::endl;
  
  

  //  _camera->Move( F3dVector(40, 0.0, -50.0) );

  
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


void DataManager::MoveCamera(float x, float y, float theta) {


  // Set the camera orientation
  glMatrixMode(GL_MODELVIEW);


  // Rotate and traslate the camera 
  glRotatef( - prev_theta * 180 / M_PI , 0.f , 1.f , 0.f );
  glTranslatef( 0.f , 0.f , - prev_y);
  glTranslatef( - prev_x , 0.f , 0.f);

  
  // Rotate and traslate the camera 
  glTranslatef( x , 0.f , 0.f);
  glTranslatef( 0.f , 0.f , y);
  glRotatef( theta *  180 / M_PI, 0.f , 1.f , 0.f );


  prev_x = x;
  prev_y = y;
  prev_theta = theta;


  // glutSwapBuffers ( ) ;


  std::cout << "Camera in: \t"
	    << x << "; "
	    << y << "; "
	    << theta << std::endl;


}
