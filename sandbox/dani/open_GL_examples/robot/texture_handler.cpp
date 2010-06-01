#include "texture_handler.h"

// quick and dirty bitmap loader...for 24 bit bitmaps with 1 plane only.  
// See http://www.dcs.ed.ac.uk/~mxr/gfx/2d/BMP.txt for more info.
int ImageLoad(std::string filename, Image *image) {
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
void LoadGLTextures(GLuint * texture, std::string filename) {	
  // Load Texture
  Image *image1;
    
  // allocate space for texture
  image1 = (Image *) malloc(sizeof(Image));

  if (image1 == NULL) {
    printf("Error allocating space for image");
    exit(0);
  }

  if (!ImageLoad(filename, image1)) {
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
  //glTexImage2D(GL_TEXTURE_2D, 0, 3, image1->sizeX, image1->sizeY, 0, GL_RGB, GL_UNSIGNED_BYTE, image1->data);

  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, image1->sizeX, image1->sizeY, GL_RGB, GL_UNSIGNED_BYTE, image1->data);


  // free data from memory
  if (image1) {

    if (image1->data) {
	
      // Free The Texture Image Memory
      free(image1->data);			
    }

    // Free The Image Structure
    free(image1);						
  }

}

void DrawTexture()
{

  glEnable(GL_TEXTURE_2D);

  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glLoadIdentity();
  
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadIdentity();

  // No depth buffer writes for background.
  glDepthMask(false);

  glBegin( GL_QUADS );

  {
    glTexCoord2f( 0.f, 0.f );
    glVertex2f( -1, -1 );

    glTexCoord2f( 0.f, 1.f );
    glVertex2f( -1, 1.f );

    glTexCoord2f( 1.f, 1.f );
    glVertex2f( 1.f, 1.f );

    glTexCoord2f( 1.f, 0.f );
    glVertex2f( 1.f, -1 );

  }

  glEnd();

  glDepthMask(true);

  glPopMatrix();
  glMatrixMode(GL_PROJECTION);
  glPopMatrix();
  glMatrixMode(GL_MODELVIEW);

  glDisable(GL_TEXTURE_2D);
}