#include "DataManager.h"

float SpacialMetricCalc(robot_data * robot_status,
		       image_data * bg_image_data)
{
  float distance;
  
  distance = 
    sqrt( pow((robot_status -> x ) - (bg_image_data -> x), 2) +
	  pow((robot_status -> y ) - (bg_image_data -> y), 2) +
	  pow((robot_status -> theta ) - (bg_image_data -> theta), 2));

  return distance;

}

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

  NextStep();
  

}

DataManager::~DataManager()
{
  free(_robot_status);
  free(_bg_image_data);
}

void DataManager::NextStep() {
  
  _logic->RetrieveData(_robot_status);

  /* move robot with _robot_status data
  _rob->Place(_robot_status->x,
	      _robot_status->y,
	      _robot_status->theta);
	      
  */

  _rob->Place(0.f, -70.f, 0.f);


  _logic->SelectImage(_robot_status, _bg_image_data,
		      SpacialMetricCalc);
 
  /* use image_path to load the image */
  LoadGLTextures(_texture, _bg_image_data->path);
      
  /* use data in out_element to change
     camera position
 
  
  MoveCamera(_bg_image_data->x,
	     _bg_image_data->y,
	     _bg_image_data->theta);
  */

  x = _bg_image_data->x;

  y = _bg_image_data->y;

  theta = _bg_image_data->theta;  
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

  // Clear Z−Buffer
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  // Set the camera orientation
  glMatrixMode(GL_MODELVIEW);
  
  
  glLoadIdentity();
  gluLookAt(0, 0, -1,
	    0 , 0 , 0,
	    0 , 1 , 0);

  // Rotate and traslate the camera 
  glTranslatef( x*100 , 0.f , y*100);
  glRotatef( theta , 0.f , 1.f , 0.f );



  // glutSwapBuffers ( ) ;


  std::cout << "Camera in: \t"
	    << x << "; "
	    << y << "; "
	    << theta << std::endl;

  // glutPostRedisplay();
  

}
