#include "stdafx.h"
#include "math.h"
//#include "Type.h"
#include "util.h"
#include <stdlib.h>
#include <time.h>
//#include "robot.h"


void DrawPar(GLfloat x0,GLfloat y0,GLfloat z0,GLfloat l,GLfloat h,GLfloat d )
/// xyz0 uk centro e il resto le dimensioni
{
	//glTranslatef(x0,y0,z0);
//	glBindTexture(GL_TEXTURE_2D, texture[0]);
	glBegin(GL_QUADS);
	
	GLfloat direction=1.0f;
	GLfloat coord=2.0f;
glNormal3f(0.0f,0.0f,direction);
		glTexCoord2f(0.0f, coord);glVertex3f(x0+l/2,y0+d/2,z0+h/2); //6
		glTexCoord2f(1.0f, coord);glVertex3f(x0-l/2,y0+d/2,z0+h/2);
		glTexCoord2f(1.0f, 0.0f);glVertex3f(x0-l/2,y0-d/2,z0+h/2);
		glTexCoord2f(0.0f,0.0f);glVertex3f(x0+l/2,y0-d/2,z0+h/2);
glNormal3f(0,0,-direction);
		glTexCoord2f(0.0f, coord);glVertex3f(x0+l/2,y0+d/2,z0-h/2);  //1
		glTexCoord2f(0.0f, 0.0f);glVertex3f(x0+l/2,y0-d/2,z0-h/2);
		glTexCoord2f(1.0f, 0.0f);glVertex3f(x0-l/2,y0-d/2,z0-h/2);
		glTexCoord2f(1.0f, coord);glVertex3f(x0-l/2,y0+d/2,z0-h/2);
glNormal3f(direction,0,0);
		glTexCoord2f(0.0f, coord);glVertex3f(x0+l/2,y0+d/2,z0+h/2); //2
		glTexCoord2f(0.0f, 0.0f);glVertex3f(x0+l/2,y0-d/2,z0+h/2);
		glTexCoord2f(1.0f, 0.0f);glVertex3f(x0+l/2,y0-d/2,z0-h/2);
		glTexCoord2f(1.0f, coord);glVertex3f(x0+l/2,y0+d/2,z0-h/2);
glNormal3f(-direction,0,0);
		glTexCoord2f(0.0f, coord);glVertex3f(x0-l/2,y0+d/2,z0+h/2); //5
		glTexCoord2f(1.0f, coord);glVertex3f(x0-l/2,y0+d/2,z0-h/2);
		glTexCoord2f(1.0f, 0.0f);glVertex3f(x0-l/2,y0-d/2,z0-h/2);
		glTexCoord2f(0.0f, 0.0f);glVertex3f(x0-l/2,y0-d/2,z0+h/2);
glNormal3f(0,-direction,0);
		glVertex3f(x0+l/2,y0+d/2,z0+h/2); //3
		glVertex3f(x0+l/2,y0+d/2,z0-h/2);
		glVertex3f(x0-l/2,y0+d/2,z0-h/2);
		glVertex3f(x0-l/2,y0+d/2,z0+h/2);/*
glNormal3f(0,-direction,0);

// REGOLA PER SUPERFICIE ESTERNA prima var +--+ seconda ++--
		glVertex3f(x0+l/2,y0-d/2,z0+h/2); //4
		glVertex3f(x0+l/2,y0-d/2,z0-h/2);
		glVertex3f(x0-l/2,y0-d/2,z0-h/2);
		glVertex3f(x0-l/2,y0-d/2,z0+h/2);*/
glEnd();
}


void Normalise(XYZ *p)
{
   double length;

   length = p->x * p->x + p->y * p->y + p->z * p->z;
   if (length > 0) {
		length = sqrt(length);
      p->x /= length;
      p->y /= length;
      p->z /= length;
   } else {
		p->x = 0;
		p->y = 0;
		p->z = 0;
	}	
}


/*void CAMERA::RotateCamera(int ix,int iy,int iz)
{
   XYZ vp,vu,vd;
   XYZ right;
   XYZ newvp,newr;
   double radius,dd,radians;
   double dx,dy,dz;

   //vu = camera.vu;
   Normalise(&vu);
   //vp = camera.vp;
   //vd = camera.vd;
   Normalise(&vd);
   CROSSPROD(vd,vu,right);
   Normalise(&right);
   radians = PI / 180.0;

   // Handle the roll 
   if (iz != 0) {
      vu.x += iz * right.x * radians;
      vu.y += iz * right.y * radians;
      vu.z += iz * right.z * radians;
      Normalise(&this->vu);
      return;
   }

   // Distance from the rotate point 
   dx = vp.x - pr.x;
   dy = vp.y - pr.y;
   dz = vp.z - pr.z;
   radius = sqrt(dx*dx + dy*dy + dz*dz);

   // Determine the new view point 
   dd = radius * radians;
   newvp.x = vp.x + dd * ix * right.x + dd * iy * vu.x - pr.x;
   newvp.y = vp.y + dd * ix * right.y + dd * iy * vu.y - pr.y;
   newvp.z = vp.z + dd * ix * right.z + dd * iy * vu.z - pr.z;
   Normalise(&newvp);
   vp.x = pr.x + radius * newvp.x;
   vp.y = pr.y + radius * newvp.y;
   vp.z = pr.z + radius * newvp.z;

   // Determine the new right vector
   newr.x = vp.x + right.x - pr.x;
   newr.y = vp.y + right.y - pr.y;
   newr.z = vp.z + right.z - pr.z;
   Normalise(&newr);
   newr.x = pr.x + radius * newr.x - vp.x;
   newr.y = pr.y + radius * newr.y - vp.y;
   newr.z = pr.z + radius * newr.z - vp.z;

   vd.x = pr.x - vp.x;
   vd.y = pr.y - vp.y;
   vd.z = pr.z - vp.z;
   Normalise(&this->vd);

   // Determine the new up vector 
   CROSSPROD(newr,vd,vu);
   Normalise(&this->vu);
}



*/
void CAMERA::CameraInit(ROBOT *robot)
{
   XYZ origin = {0,0,0};

   aperture = 45.0f;
   focallength = 12.0f;
   eyesep = 4; //4
   nearp = 1;/// focallength / 10;
   farp = 2000;
   phi=-40*DTOR;
   teta=robot->tetar;
   vp.x = 1;
   vp.y = 3; 
   vp.z = 1;
   vd.x = 1;
   vd.y = -1;
   vd.z = -1;
   vu.x = 0;
   vu.y = 1;
   vu.z = 0;
   pr = origin;

   Normalise(&this->vd);
   Normalise(&this->vu);
}
/*void CAMERA::CameraHome(void)
{
   XYZ origin = {0,0,0};

   aperture = 45;
   focallength = 100;
   eyesep = 2;
   nearp = focallength / 10;
   farp = 2000;

   vp.x = 30;
   vp.y = 3; 
   vp.z = 30;
   vd.x = -1;
   vd.y = -1;
   vd.z = -1;
   vu.x = -1;
   vu.y = 1;
   vu.z = 0;
   pr = origin;

   Normalise(&this->vd);
   Normalise(&this->vu);
}*/

void CAMERA::CameraOnRobot(ROBOT *robot)
{
  /*XYZ origin = {0,0,0}; /// commenti fino -40
	
   camera.aperture = 60;
   camera.focallength = 35;
   camera.eyesep = 0; //4
   camera.nearp = camera.focallength / 10;
   camera.farp = 2000;
   camera.pr = origin;

   camera.phi=-40;*/
float temp=0;
   teta=robot->tetar;
   teta+=tetaofs;
   vp.x = robot->xr;
   vp.y = 7.3f; 
   vp.z = robot->yr;
   vd.x = cos(teta)*cos(phi);
   vd.y = sin(phi);
   vd.z = sin(teta)*cos(phi);
   temp=vd.x*vd.x+vd.z*vd.z;
//   Normalise(&this->vd);
   vu.x = 0;
   vu.y = 1;
   vu.z = 0;
   
   //Normalise(&this->vd);
   //Normalise(&this->vu);
}

void CAMERA::CameraOnEnviroment(void)
{
  /*XYZ origin = {0,0,0}; /// commenti fino -40

   camera.aperture = 60;
   camera.focallength = 35;
   camera.eyesep = 0; //4
   camera.nearp = camera.focallength / 10;
   camera.farp = 2000;
   camera.pr = origin;

   camera.phi=-40;*/

   
   teta=tetaofs;
   //vp.x = 0;
   //vp.y = 7; 
   //vp.z = 0;
   vd.x = cos(teta)*cos(phi);
   vd.y = sin(phi);
   vd.z = sin(teta)*cos(phi);
   //Normalise(&this->vd);
   vu.x = 0;
   vu.y = 1;
   vu.z = 0;
   
   //Normalise(&this->vd);
   //Normalise(&this->vu);
}





void objallocate(OBJECT *k,int n)						// Allocate Memory For Each Object
{														// And Defines points
	k->points=(VERTEX*)malloc(sizeof(VERTEX)*n);		// Sets points Equal To VERTEX * Number Of Vertices
}														// (3 Points For Each Vertice)

void objfree(OBJECT *k)									// Frees The Object (Releasing The Memory)
{
	free(k->points);									// Frees Points
}

void readstr(FILE *f,char *string)						// Reads A String From File (f)
{
	do													// Do This
	{
		fgets(string, 255, f);							// Gets A String Of 255 Chars Max From f (File)
	} while ((string[0] == '/') || (string[0] == '\n'));// Until End Of Line Is Reached
	return;												// Return
}

int objload(char *name,OBJECT *k)						// Loads Object From File (name)
{
	int		ver;										// Will Hold Vertice Count
	float	rx,ry,rz;									// Hold Vertex X, Y & Z Position
	FILE	*filein;									// Filename To Open
	char	oneline[255];								// Holds One Line Of Text (255 Chars Max)

	filein = fopen(name, "rt");							// Opens The File For Reading Text In Translated Mode
														// CTRL Z Symbolizes End Of File In Translated Mode
	readstr(filein,oneline);							// Jumps To Code That Reads One Line Of Text From The File
	sscanf(oneline, "Vertices: %d\n", &ver);			// Scans Text For "Vertices: ".  Number After Is Stored In ver
	k->verts=ver;										// Sets Objects verts Variable To Equal The Value Of ver
	objallocate(k,ver);									// Jumps To Code That Allocates Ram To Hold The Object

	for (int i=0;i<ver;i++)								// Loops Through The Vertices
	{
		readstr(filein,oneline);						// Reads In The Next Line Of Text
		sscanf(oneline, "%f %f %f", &rx, &ry, &rz);		// Searches For 3 Floating Point Numbers, Store In rx,ry & rz
		k->points[i].x = rx;							// Sets Objects (k) points.x Value To rx
		k->points[i].y = ry;							// Sets Objects (k) points.y Value To ry
		k->points[i].z = rz;							// Sets Objects (k) points.z Value To rz
	}
	fclose(filein);										// Close The File

							// If ver Is Greater Than maxver Set maxver Equal To ver
	return ver;
}														// Keeps Track Of Highest Number Of Vertices Used In Any Of The
	

int CountFromFile(char *string)
{
        FILE *fcou;
        int ccount;
        fcou=fopen(string,"r");
        fscanf(fcou,"%d",&ccount);
        fclose(fcou);
        fflush(fcou);
        ccount++;
        fcou=fopen(string,"w");
        fprintf(fcou,"%d",ccount);
        fclose(fcou);
        fflush(fcou);
return ccount;
}

//#define RAND_MAX 100

 int random(OBJECT *k)
{

srand(time(NULL));
//short 	int  temp= (rand() % 10000) * k->verts;
int KNUM_MIN=	0;
int KNUM_MAX=	k->verts;
short int N = abs(KNUM_MIN) + abs(KNUM_MAX);
	N = rand() % N + KNUM_MIN;

	return N;
}

short int RandMM(int min, int max)
{
	short int N = abs(min) + abs(max);
	N = rand() % N + min;
	return N;
}


int RandRep(short int **array,int min,int max,int degree_disorder)
{
	short int *sort,*atemp;
	int i=0;
	int size=(max-min+1);
	sort=(short int*) malloc(sizeof(short int)*degree_disorder);
	*array=(short int*) malloc(sizeof(short int)*size);
	for (;i<size;i++)
		(*array)[i]=i+min;
	
	////////////////////////////////////// MODO 1 :
	// creo un vettore random di coppie e poi scambio gli elementi della coppie
	for (i = 0; i < degree_disorder; i++)
			sort[i]=RandMM(0,size);
	short int temp;
	for (i = 0; i < degree_disorder-1; i+=2)
	{
		temp=(*array)[sort[i]];
		(*array)[sort[i]]=(*array)[sort[i+1]];
		(*array)[sort[i+1]]=temp;
	}
return 	size;
}
