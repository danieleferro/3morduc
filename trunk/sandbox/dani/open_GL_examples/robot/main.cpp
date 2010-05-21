#include <GL/glut.h>    // Header File For The GLUT Library 
#include <GL/gl.h>	// Header File For The OpenGL32 Library
#include <GL/glu.h>	// Header File For The GLu32 Library
#include <stdio.h>      // Header file for standard file i/o.
#include <stdlib.h>     // Header file for malloc/free.
#include <unistd.h>     // needed to sleep.
#include "robot.h"

/* ascii code for the keys */
#define ESCAPE 27

/* The number of our GLUT window */
int window; 

/* storage for one texture  */
GLuint texture[1];

/* robot declaration */
Robot rob(0.f, 0.f, 1.f);

/* Image type - contains height, width, and data */
struct Image {
  unsigned long sizeX;
  unsigned long sizeY;
  char *data;
};

typedef struct Image Image;
// quick and dirty bitmap loader...for 24 bit bitmaps with 1 plane only.  
// See http://www.dcs.ed.ac.uk/~mxr/gfx/2d/BMP.txt for more info.
int ImageLoad(char *filename, Image *image) {
  FILE *file;
  unsigned long size;                 // size of the image in bytes.
  unsigned long i;                    // standard counter.
  unsigned short int planes;          // number of planes in image (must be 1) 
  unsigned short int bpp;             // number of bits per pixel (must be 24)
  char temp;                          // temporary color storage for bgr-rgb conversion.

  // make sure the file is there.(
  if ((file = fopen(filename, "rb"))==NULL)
    {
      printf("File Not Found : %s\n",filename);
      return 0;
    }
  
  
  // seek through the bmp header, up to the width/height:
  fseek(file, 18, SEEK_CUR);

  // read the width
  if ((i = fread(&image->sizeX, 4, 1, file)) != 1) {
    printf("Error reading width from %s.\n", filename);
    return 0;
  }
  printf("Width of %s: %lu\n", filename, image->sizeX);
    
  // read the height 
  if ((i = fread(&image->sizeY, 4, 1, file)) != 1) {
    printf("Error reading height from %s.\n", filename);
    return 0;
  }
  printf("Height of %s: %lu\n", filename, image->sizeY);
    
  // calculate the size (assuming 24 bits or 3 bytes per pixel).
  size = image->sizeX * image->sizeY * 3;

  // read the planes
  if ((fread(&planes, 2, 1, file)) != 1) {
    printf("Error reading planes from %s.\n", filename);
    return 0;
  }
  if (planes != 1) {
    printf("Planes from %s is not 1: %u\n", filename, planes);
    return 0;
  }

  // read the bpp
  if ((i = fread(&bpp, 2, 1, file)) != 1) {
    printf("Error reading bpp from %s.\n", filename);
    return 0;
  }
  if (bpp != 24) {
    printf("Bpp from %s is not 24: %u\n", filename, bpp);
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
    printf("Error reading image data from %s.\n", filename);
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
void LoadGLTextures() {	
  // Load Texture
  Image *image1;
    
  // allocate space for texture
  image1 = (Image *) malloc(sizeof(Image));

  if (image1 == NULL) {
    printf("Error allocating space for image");
    exit(0);
  }

  if (!ImageLoad("screenshot.bmp", image1)) {
    exit(1);
  }        

  // Create Texture	
  glGenTextures(1, &texture[0]);

  // Bind 2d texture (x and y size)
  glBindTexture(GL_TEXTURE_2D, texture[0]);   

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


};


void setMaterial ( GLfloat ambientR, GLfloat ambientG, GLfloat ambientB, 
		   GLfloat diffuseR, GLfloat diffuseG, GLfloat diffuseB, 
		   GLfloat specularR, GLfloat specularG, GLfloat specularB,
		   GLfloat shininess, GLfloat blend ) {
  
  GLfloat ambient[] = { ambientR, ambientG, ambientB,     blend };
  GLfloat diffuse[] = { diffuseR, diffuseG, diffuseB,     blend };
  GLfloat specular[] = { specularR, specularG, specularB, blend };
  
  glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT,ambient);
  glMaterialfv(GL_FRONT_AND_BACK,GL_DIFFUSE,diffuse);
  glMaterialfv(GL_FRONT_AND_BACK,GL_SPECULAR,specular);
  glMaterialf(GL_FRONT_AND_BACK,GL_SHININESS,shininess);
}

GLdouble * translCoordinates(int x, int y) {

  // printf(">> X: %d\t Y: %d\n", x, y);
  GLint viewport[4];
  GLdouble modelview[16];
  GLdouble projection[16];
  GLfloat winX, winY, winZ;
  GLdouble posX, posY, posZ;
  
  glGetDoublev( GL_MODELVIEW_MATRIX, modelview );
  glGetDoublev( GL_PROJECTION_MATRIX, projection );
  glGetIntegerv( GL_VIEWPORT, viewport );
  
  winX = (float)x;
  winY = (float)viewport[3] - (float)y;
  glReadPixels( x, int(winY), 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, &winZ );
  
  gluUnProject( winX, winY, winZ, modelview, projection, viewport, &posX, &posY, &posZ);

  GLdouble * out_values = new GLdouble[3];

  out_values[0] = posX;
  out_values[1] = posY;
  out_values[2] = posZ;

  /*
  printf("--> output for (%d, %d)\n", x, y);
  for (int i=0; i<3; i++) {

    printf("%4.4f ", out_values[i]); 
  }
  printf("\n\n");
  */
  
  return out_values;

  //printf(">> Win -> ( %4.4f, %4.4f, %4.4f ) \n>> GL  -> ( %4.4f, %4.4f, %4.4f ) \n\n",
  //	 winX, winY, winZ, posX, posY, posZ);
}


void display () {

  /*
  
  GLdouble * top_left     = translCoordinates(0,   0);
  GLdouble * top_right    = translCoordinates(624, 0);
  GLdouble * bottom_left  = translCoordinates(0, 442);
  GLdouble * bottom_right = translCoordinates(624, 442);


  
  printf("top_left: \t");

  for (int i=0; i<3; i++) {

    printf("%4.4f ", top_left[i]); 
  }

  printf("\ntop_right: \t");

  for (int i=0; i<3; i++) {

    printf("%4.4f ", top_right[i]); 
  }

  printf("\nbottom_left: \t");

  for (int i=0; i<3; i++) {

    printf("%4.4f ", bottom_left[i]); 
  }
  printf("\nbottom_right: \t");

  for (int i=0; i<3; i++) {

    printf("%4.4f ", bottom_right[i]); 
  }

  */



  /* clear window */
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  /* future matrix manipulations should affect the modelview matrix */
  glMatrixMode(GL_MODELVIEW);

  // BEGIN TEXTURE CODE
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadIdentity();

  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glLoadIdentity();
  
  glBegin(GL_QUADS);

  glTexCoord2f(0.0f, 0.0f); glVertex2f( -1,  -1); // Bottom Left Of The Texture and Quad
  glTexCoord2f(1.0f, 0.0f); glVertex2f(  1,  -1);	// Bottom Right Of The Texture and Quad
  glTexCoord2f(1.0f, 1.0f); glVertex2f(  1,   1);	// Top Right Of The Texture and Quad
  glTexCoord2f(0.0f, 1.0f); glVertex2f( -1,   1);	// Top Left Of The Texture and Quad

  glEnd();
    
    
  glPopMatrix();
  glPopMatrix();

  glMatrixMode(GL_MODELVIEW);


  /*
  glPushMatrix();
  glLoadIdentity();

  

  glBindTexture(GL_TEXTURE_2D, texture[0]);   // choose the texture to use.
  
  
  glBegin(GL_QUADS);		                // begin drawing a cube
  
  // Front Face (note that the texture's corners have to match the quad's corners)
  glTexCoord2f(0.0f, 0.0f); glVertex3f( -10,  -10,  -15.0f); // Bottom Left Of The Texture and Quad
  glTexCoord2f(1.0f, 0.0f); glVertex3f(  10,  -10,  -15.0f);	// Bottom Right Of The Texture and Quad
  glTexCoord2f(1.0f, 1.0f); glVertex3f(  10, 10,    -15.0f);	// Top Right Of The Texture and Quad
  glTexCoord2f(0.0f, 1.0f); glVertex3f( -10,  10,   -15.0f);	// Top Left Of The Texture and Quad
  
  glEnd();
  
  
  glPopMatrix();
  
  // END TEXTURE CODE 
  
  */
  
  //rob.DrawRobot();
  
  // last set material is for the textures
  setMaterial(1.0,1.0,1.0, 1.0,1.0,1.0, 1.0,1.0,1.0, 20, 1);



  /* flush drawing routines to the window */
  glFlush();
}

void reshape ( int width, int height ) {

  /* define the viewport transformation */
  glViewport(0,0,width,height);
}

/* The function called whenever a key is pressed. */
void keyPressed(unsigned char key, int x, int y) 
{
  /* avoid thrashing this procedure */
  usleep(100);

  /* If escape is pressed, kill everything. */
  if (key == ESCAPE) 
    { 


      /* shut down our window */
      glutDestroyWindow(window); 
      
      /* exit the program...normal termination. */
      exit(0);                   
    }
  


}

void animate () {

  /* update state variables */

  /* refresh screen */
  glutPostRedisplay();
}




void getGLPos(int x, int y) {


  // printf(">> X: %d\t Y: %d\n", x, y);
  GLint viewport[4];
  GLdouble modelview[16];
  GLdouble projection[16];
  GLfloat winX, winY, winZ;
  GLdouble posX, posY, posZ;
  
  glGetDoublev( GL_MODELVIEW_MATRIX, modelview );
  glGetDoublev( GL_PROJECTION_MATRIX, projection );
  glGetIntegerv( GL_VIEWPORT, viewport );
  
  winX = (float)x;
  winY = (float)viewport[3] - (float)y;
  glReadPixels( x, int(winY), 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, &winZ );
  
  gluUnProject( winX, winY, winZ, modelview, projection, viewport, &posX, &posY, &posZ);

  printf(">> Win -> ( %4.4f, %4.4f, %4.4f ) \n>> GL  -> ( %4.4f, %4.4f, %4.4f ) \n\n",
	 winX, (float)viewport[3] - winY, winZ, posX, posY, posZ);
}




int main ( int argc, char * argv[] ) {

  /* initialize GLUT, using any commandline parameters passed to the 
     program */
  glutInit(&argc,argv);

  /* setup the size, position, and display mode for new windows */
  glutInitWindowSize(624, 442);
  glutInitWindowPosition(0,0);
  glutInitDisplayMode( GLUT_RGBA | GLUT_DEPTH);
  // glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE | GLUT_ALPHA | GLUT_DEPTH);  

  /* create and set up a window */
  window = glutCreateWindow("robot");
  glutDisplayFunc(display);
  glutReshapeFunc(reshape);

  /* set mouse passive motion callback */
  glutPassiveMotionFunc(getGLPos);


  /* set up depth-buffering */
  glEnable(GL_DEPTH_TEST);
    
  /* blending */
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA,GL_ONE);


  /* Register the function called when the keyboard is pressed. */
  glutKeyboardFunc(keyPressed);

  /* animate function */
  //glutIdleFunc(animate);

  // InitGL
  LoadGLTextures();				// Load The Texture(s) 
  glEnable(GL_TEXTURE_2D);			// Enable Texture Mapping
  glClearColor(0.0f, 0.0f, 0.0f, 0.0f);	// set the color for the glClear()
  glClearDepth(1.0);				// Enables Clearing Of The Depth Buffer
  glDepthFunc(GL_LESS);			// The Type Of Depth Test To Do
  glShadeModel(GL_SMOOTH);			// Enables Smooth Color Shading


  /* set up lights */
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  GLfloat lightpos[] = { 0.0, 15.0, 15.0 };
  GLfloat lightcolor[] = { 0.5, 0.5, 0.5 };
  GLfloat ambcolor[] = { 0.2, 0.2, 0.2 };

  glEnable(GL_LIGHTING);
  //glLightModelfv(GL_LIGHT_MODEL_AMBIENT,ambcolor);

  glEnable(GL_LIGHT0);
//   glLightfv(GL_LIGHT0,GL_POSITION,lightpos);
//   glLightfv(GL_LIGHT0,GL_AMBIENT,lightcolor);
//   glLightfv(GL_LIGHT0,GL_DIFFUSE,lightcolor);
//   glLightfv(GL_LIGHT0,GL_SPECULAR,lightcolor);


  /* define the projection transformation */
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(60, 624/442, 0.001, 100000);
  
  /* define the viewing transformation */
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(0.0, 1.0, 10.0,
	    0.0, 0.0, 0.0,
	    0.0, 1.0, 0.0);
 

  /* tell GLUT to wait for events */
  glutMainLoop();
}
