#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "Type.h"
#include "robot.h"

#define CROSSPROD(p1,p2,p3) \
	p3.x = p1.y*p2.z - p1.z*p2.y; \
	p3.y = p1.z*p2.x - p1.x*p2.z; \
	p3.z = p1.x*p2.y - p1.y*p2.x

#define NOSTEREO 0
#define ACTIVESTEREO 1
#define DUALSTEREO 2

void Normalise(XYZ *p);



void DrawPar(GLfloat x0,GLfloat y0,GLfloat z0,GLfloat l,GLfloat h,GLfloat d );
void objallocate(OBJECT *k,int n);						// Allocate Memory For Each Object
void objfree(OBJECT *k);									// Frees The Object (Releasing The Memory)
void readstr(FILE *f,char *string);						// Reads A String From File (f)
int objload(char *name,OBJECT *k);

class CAMERA {
public:
	XYZ vp;                /* View position           */
	XYZ vd;                /* View direction vector   */
	XYZ vu;                /* View up direction       */
	XYZ pr;                /* Point to rotate about   */
	double focallength;    /* Focal Length along vd   */
	double aperture;       /* Camera aperture         */
	double eyesep;         /* Eye separation          */
	GLfloat phi, teta,tetaofs;		// for pan tilt.
	int state;						/// state of camera
	double nearp, farp;       /* Cutting plane distances */
	int stereo;				  /* Are we in stereo mode   */
	int screenwidth;       /* Screen dimensions       */
	int screenheight;      /*                         */
	double cursordepth;    /* 3D cursor depth         */
	float cxb,cyb,czb,phib,tetaofsb;
	CAMERA() {vp.x=0;vp.y=0;vp.z=0; }
	void CameraOnRobot(ROBOT *robot);
	void CameraOnEnviroment(void);
	//void CameraHome(void);
void CameraInit(ROBOT *robot);
void RotateCamera(int ix,int iy,int iz);

} ;


int CountFromFile(char *string);

int random(OBJECT *k);
int RandRep(short int **array,int min,int max,int degree_disorder);