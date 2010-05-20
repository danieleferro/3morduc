#include "stdafx.h"

//#include <windows.h>									// Header File For Windows
#include <math.h>										// Math Library Header File
//#include <stdio.h>										// Header File For Standard Input/Output
//#include "GL\glut.h"										// Header File For Standard Input/Output
#include <gl\gl.h>										// Header File For The OpenGL32 Library
#include <gl\glu.h>										// Header File For The GLu32 Library
#include "CCronometer.h"





class ROBOT
{
public:
	GLfloat xr;					// posizione
	GLfloat yr;					
	GLfloat tetar;

public:
	GLfloat vlim;				/// v max
	GLfloat wlim;				/// w max
	CCronometer ck;				//base dei tempi

public:
int n ;                         //gear ratio
int res ;                       //risoluzione encoder
GLfloat d ;                     //diametro della ruota
GLfloat cm ;
public:
	//GLfloat xr;					// posizione
	//GLfloat yr;					
	//GLfloat tetar;
	float Time;					// tempo
	GLfloat dxr,dyr,dtetar;
	float vrx,vry;

GLfloat l;                       //distanza tra ruote
GLfloat v;
GLfloat w;
//float dnr = (2*v+w*l)/(2*cm);       //passi encoder destro
//float dnl = (2*v-w*l)/(2*cm);       //passi encoder sinistro
GLfloat radius; // max dimension	
FILE *fp;
int collisions;

	ROBOT();
	ROBOT(float x1, float x2, float ang3);
	void move();
	//void PaintRobot();
	void PaintRobot2();
	void SetW(float set);
	void SetV(float set);
	void movetank(float l, float r);
	bool LoadSPFromFile(char *string,int kk);
	int RobColl(OBJECT *coll, bool activecontrol);
	int RobColl2(OBJECT *coll, bool activecontrol);
};