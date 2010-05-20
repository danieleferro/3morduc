#include <GL/glut.h>    // Header File For The GLUT Library 
#include <GL/gl.h>	// Header File For The OpenGL32 Library
#include <GL/glu.h>	// Header File For The GLu32 Library
#include <stdio.h>      // Header file for standard file i/o.
#include <stdlib.h>     // Header file for malloc/free.
#include <unistd.h>     // needed to sleep.
#include <iostream>

/* ascii code for the escape key */
#define ESCAPE 27

/* The number of our GLUT window */
int window; 

/* storage for one texture  */
GLuint texture[1];

/* current teapot position (initialized in main) */ 
// double x, y, z;           


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

    if (!ImageLoad("NeHe.bmp", image1)) {
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

    // 2d texture, level of detail 0 (normal), 3 components (red, green, blue), x size from image, y size from image, 
    // border 0 (normal), rgb color data, unsigned byte data, and finally the data itself.
    glTexImage2D(GL_TEXTURE_2D, 0, 3, image1->sizeX, image1->sizeY, 0, GL_RGB, GL_UNSIGNED_BYTE, image1->data);

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

void display () {

    /* clear window */
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    /* future matrix manipulations should affect the modelview matrix */
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();


    /* rotate sceen */
    // glTranslatef(x,y,z);
    // printf("X:%lf Y:%lf Z:%lf \n", &x, &y, &z);


    /* BEGIN TEXTURE CODE */
    glPushMatrix();

    glTranslatef(0.0f, 0.0f, -5.5f);            // move 2.5 units into the screen.
    
    glBindTexture(GL_TEXTURE_2D, texture[0]);   // choose the texture to use.

    glBegin(GL_QUADS);		                // begin drawing a cube
    
    // Front Face (note that the texture's corners have to match the quad's corners)
    glTexCoord2f(0.0f, 0.0f); glVertex2f(-1.0f, -1.0f);	// Bottom Left Of The Texture and Quad
    glTexCoord2f(1.0f, 0.0f); glVertex2f( 1.0f, -1.0f);	// Bottom Right Of The Texture and Quad
    glTexCoord2f(1.0f, 1.0f); glVertex2f( 1.0f,  1.0f);	// Top Right Of The Texture and Quad
    glTexCoord2f(0.0f, 1.0f); glVertex2f(-1.0f,  1.0f);	// Top Left Of The Texture and Quad

    glEnd();

    //glTranslatef(0.0f, 0.0f, 2.5f);              // move 2.5 units into the screen.
    //glPopMatrix();

    // END TEXTURE CODE


    /* draw scene */
    //    glPushMatrix();

        // house
    //    glPushMatrix();
    setMaterial(0.0,0.5,1.0, 0.0,0.5,1.0, 1.0,1.0,1.0, 1, 0.8);
    //glutSolidCube(2);                 // building


    glPushMatrix();

    glRotatef(-90,1,0,0);
    glTranslatef(0,0,-1);

    setMaterial(0.0,0.5,1.0, 0.0,0.5,1.0, 1.0,1.0,1.0, 1, 0.8);

    GLUquadricObj *p = gluNewQuadric();
    gluQuadricDrawStyle(p, GLU_LINE);
    gluCylinder(p,1.2f,1.2f,2.f,32,32);

    glPopMatrix();




    glTranslatef(0,1,0);
    glPushMatrix();                   // roof
    glRotatef(-90,1,0,0);
    setMaterial(0.0,0.5,1.0, 0.0,0.5,1.0, 1.0,1.0,1.0, 50, 0.8);
    glutWireCone(1.5,1,16,8);
    glPopMatrix();

    glTranslatef(.75,.5,-.75);         
    glPushMatrix();                   // chimney
    glScalef(1,3,1);
    setMaterial( 1.0,0.0,0.0, 1.0,0.0,0.0, 0.0,0.0,0.0, 1, 0.8);
    glutWireCube(.25);
    glPopMatrix();
    glPopMatrix();

    glTranslatef(0,-.65,2);

    // car
    setMaterial(1.0,0.0,0.0, 1.0,0.0,0.0, 1.0,0.0,0.0, 50, 0.8);
    glPushMatrix();
    glPushMatrix();                   // body
    glScalef(2,.5,1);
    glutWireCube(.5);
    glPopMatrix();
    glTranslatef(0,0,.25);
    glPushMatrix();
    glTranslatef(-.4,-.2,0);


    setMaterial(0.2,1.0,0.2, 0.2,1.0,0.2, 0.2,1.0,0.2, 2, 0.8);

    glutWireTorus(.05,.1,8,8);       // wheel
    glTranslatef(.8,0,0);
    glutWireTorus(.05,.1,8,8);       // wheel
    glPopMatrix();
    glTranslatef(0,0,-.5);
    glPushMatrix();
    glTranslatef(-.4,-.2,0);
    glutWireTorus(.05,.1,8,8);       // wheel
    glTranslatef(.8,0,0);
    glutWireTorus(.05,.1,8,8);       // wheel
    glPopMatrix();
    glPopMatrix();

    glPopMatrix();

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
    //x += .00004;
    //y += .00004;
    //z -= .00004;

    /* refresh screen */
    glutPostRedisplay();
}


int main ( int argc, char * argv[] ) {

    /* initialize GLUT, using any commandline parameters passed to the 
       program */
    glutInit(&argc,argv);

    /* setup the size, position, and display mode for new windows */
    glutInitWindowSize(500,500);
    glutInitWindowPosition(0,0);
    glutInitDisplayMode( GLUT_RGBA | GLUT_DEPTH);
    // glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE | GLUT_ALPHA | GLUT_DEPTH);  

    /* create and set up a window */
    window = glutCreateWindow("house3");
    glutDisplayFunc(display);
    glutReshapeFunc(reshape);

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
    glLightModelfv(GL_LIGHT_MODEL_AMBIENT,ambcolor);

    glEnable(GL_LIGHT0);
    glLightfv(GL_LIGHT0,GL_POSITION,lightpos);
    glLightfv(GL_LIGHT0,GL_AMBIENT,lightcolor);
    glLightfv(GL_LIGHT0,GL_DIFFUSE,lightcolor);
    glLightfv(GL_LIGHT0,GL_SPECULAR,lightcolor);

    /* define the projection transformation */
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(40,1,4,20);

    /* define the viewing transformation */
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    gluLookAt(0.0,5.0,10.0,0.0,0.0,0.0,0.0,1.0,0.0);

//     x = 0;
//     y = 0;
//     z = 0;

    /* tell GLUT to wait for events */
    glutMainLoop();
}
