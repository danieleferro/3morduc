// Main.h: interface for the CMain class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_MAIN_H__05A74DD2_C37A_4085_A563_7ED88F625CB7__INCLUDED_)
#define AFX_MAIN_H__05A74DD2_C37A_4085_A563_7ED88F625CB7__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
#include <list>
//#include "camera.h"
#include "atlimage.h"
#include "util.h"					
#include "font.h"
//#include "robot.h"
#include "sky.h"
#include "glext.h"
#include "wglext.h" 


extern "C" {
#include "MorducRPC_If.h"
}

typedef struct {
	int time;
	int record;
	void * image_data;
} logger_data_t;

class CMain  
{
public:


	HDC			hDC;									// Device Context Handle
	HWND		hWnd;									// Window Handle

	bool		keys[256];									// Key Array
	bool		active;								// Program's Active
	bool		fullscreen;							// Default Fullscreen To True


	GLfloat		cx,cy,cz;								// X, Y & Z Position

	GLfloat col_amb1[4];
	GLfloat col_amb2[4];
	GLfloat col_amb3[4];



	int			step,steps;				// Step Counter And Maximum Number Of Steps
	//OBJECT		obfloor;	// Our 4 Morphable Objects (obfloor,2,3 & 4)
	OBJECT      ostacoli;
	OBJECT      target;
	int target_selected;

	ROBOT robot1;
	CAMERA camera;
	XYZ r;
	POINT mouseup, mousedown;
	//Sky         *Cielo;
	CImage *bitmap;
	GLuint texture[2];
	unsigned char *p_bmp;
	double tetaold,phiold;
	int dx; //// l'uso di queste 4 variabili  è stato sostituito da camera.phi e tetaofs
	int dy;
	int dxold;
	int dyold;
	GLfloat *radar;
	int currentbutton;
	int buttondown;
	int targettouch;
	float v;
	float w;
	float xmorduc;
	float ymorduc;
	float amorduc;

	bool keyPressed[256];

	std::list <XYZ> cl;
	std::list <XYZ> coda;
	TRPCMap MapData;
	TRPCWebCam WCData;

	PFNWGLSWAPINTERVALEXTPROC       pwglSwapIntervalEXT  ;

	bool StereoEnable;
	bool SwapControl; // changed
	bool ManSwapEnable;
	bool ImageEnable;
	bool HDIEnable;
	bool StartRecord;
	int SimNumber;
	bool CameraOnRobot;
	int TextureEnable;
	bool AnaglyphEnable;
	int CollisionInterrupt;
	bool Pause;
	UINT_PTR timer0,timer1;
	bool Finish;
	GLfloat flo;

	/* variables to store last time */
	int LastScreenshotTime;
	FILE * FpData;

	virtual bool Initialize();
	virtual void Deinitialize();
	//virtual void Update (DWORD milliseconds);
	virtual void Draw(bool Update);
	void InitVariables();
	void HandleMouseMotion();
	void WriteParam();
	void CreateFloor(OBJECT *input);
	void LoadEnvRPC();
	void LoadEnv(GLfloat dim,char *filename);
	void DrawObs();
	virtual void HandleDisplay(bool swapping);
	void HandleKeyboard(char *keyDown);
	void LoadImage(char *name,char **dest,int *height, int *width);
	void LoadEnvRPC(char **dest,int *height,int *width);
	int LoadGLTextures();
	void SelectTexture();
	void StartRec();
	void StopRec();

	/* function to save screenshot and robot data */
	void SaveStatus();


public:
	//BOOL KeyPressed( int nCode );



	CMain();
	virtual ~CMain();

};


typedef struct 
{
	int TextureEnable;
	int numberposition;
	int nmap;
	int numpath;
	float time;
} Texperiment;


#endif // !defined(AFX_MAIN_H__05A74DD2_C37A_4085_A563_7ED88F625CB7__INCLUDED_)
