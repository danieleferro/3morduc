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

  LoadGLTextures(texture, "screenshot.bmp");

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
  o << "screenshot_" << session << "_" << time << ".bmp";
  out_value.image_path = o.str();

  std::cout << out_value.image_path << std::endl;

  return out_value;
  
}

// quick and dirty bitmap loader...for 24 bit bitmaps with 1 plane only.  
// See http://www.dcs.ed.ac.uk/~mxr/gfx/2d/BMP.txt for more info.
int DataManager::ImageLoad(std::string filename, Image *image) {
  FILE *file;
  unsigned long size;                 // size of the image in bytes.
  unsigned long i;                    // standard counter.
  unsigned short int planes;          // number of planes in image (must be 1) 
  unsigned short int bpp;             // number of bits per pixel (must be 24)
  char temp;                          // temporary color storage for bgr-rgb conversion.

  const char * filename_c = filename.c_str();

  // make sure the file is there.(
  if ((file = fopen(filename_c, "rb"))==NULL)
    {
      printf("File Not Found : %s\n", filename_c);
      return 0;
    }
  
  
  // seek through the bmp header, up to the width/height:
  fseek(file, 18, SEEK_CUR);

  // read the width
  if ((i = fread(&image->sizeX, 4, 1, file)) != 1) {
    printf("Error reading width from %s.\n", filename_c);
    return 0;
  }
  printf("Width of %s: %lu\n", filename_c, image->sizeX);
    
  // read the height 
  if ((i = fread(&image->sizeY, 4, 1, file)) != 1) {
    printf("Error reading height from %s.\n", filename_c);
    return 0;
  }
  printf("Height of %s: %lu\n", filename_c, image->sizeY);
    
  // calculate the size (assuming 24 bits or 3 bytes per pixel).
  size = image->sizeX * image->sizeY * 3;

  // read the planes
  if ((fread(&planes, 2, 1, file)) != 1) {
    printf("Error reading planes from %s.\n", filename_c);
    return 0;
  }
  if (planes != 1) {
    printf("Planes from %s is not 1: %u\n", filename_c, planes);
    return 0;
  }

  // read the bpp
  if ((i = fread(&bpp, 2, 1, file)) != 1) {
    printf("Error reading bpp from %s.\n", filename_c);
    return 0;
  }
  if (bpp != 24) {
    printf("Bpp from %s is not 24: %u\n", filename_c, bpp);
    return 0;
  }
	
  // seek past the rest of the bitmap header.
  fseek(file, 24, SEEK_CUR);


  // read the data. 
  image->data = (char *) malloc(size);
  if (image->data == NULL) {
    printf("Error allocating memory for color-corrected image data");
    return 0;	
  }
    
  if ((i = fread(image->data, size, 1, file)) != 1) {
    printf("Error reading image data from %s.\n", filename_c);
    return 0;
  }
    
  for (i=0;i<size;i+=3) { // reverse all of the colors. (bgr -> rgb)
    temp = image->data[i];
    image->data[i] = image->data[i+2];
    image->data[i+2] = temp;
  }
    
  // we're done.
  return 1;
}
    
// Load Bitmaps And Convert To Textures
void DataManager::LoadGLTextures(GLuint * texture, std::string filename) {	
  // Load Texture
  Image * image;
    
  // allocate space for texture
  image = (Image *) malloc(sizeof(Image));

  if (image == NULL) {
    printf("Error allocating space for image");
    exit(0);
  }

  if (!ImageLoad(filename, image)) {
    exit(1);
  }        

  // Create Texture	
  glGenTextures(1, texture);

  // Bind 2d texture (x and y size)
  glBindTexture(GL_TEXTURE_2D, *texture);   

  // scale linearly when image bigger than texture
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
 
  // scale linearly when image smalled than texture
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR); 

  /* 2d texture, level of detail 0 (normal), 3 components (red, green, blue), x size from image, y size from image, 
     border 0 (normal), rgb color data, unsigned byte data, and finally the data itself.
  */
  //glTexImage2D(GL_TEXTURE_2D, 0, 3, image->sizeX, image->sizeY, 0, GL_RGB, GL_UNSIGNED_BYTE, image->data);

  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, image->sizeX, image->sizeY, GL_RGB, GL_UNSIGNED_BYTE, image->data);


  // free data from memory
  if (image) {

    if (image->data) {
	
      // Free The Texture Image Memory
      free(image->data);			
    }

    // Free The Image Structure
    free(image);						
  }

}
