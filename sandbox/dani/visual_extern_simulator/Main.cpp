/****************************************
*   Nehe MFC by Yvo van Dillen based on *
*   Jeff Molofee's Basecode Example     *
*          nehe.gamedev.net             *
*             2001/2004                 *
*                                       *
*****************************************/

#include "stdafx.h"
#include "NeheMFC.h"
#include "NeheWindow.h"
#include "Main.h"
#include "AVIGenerator.h"
#include "corona/include/corona.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

#define TRIALNUMBER 6 //30
#define NMAP 12
#define TEMPO 60

char nomemappe[NMAP][25];

void ExperimentSet1();
void ExperimentSet2();
void AVIStart();
void AVIFinish();
VOID CALLBACK OnAvigenerationGenerate(HWND hwnd,UINT uMsg,UINT_PTR idEvent,DWORD dwTime) ;




void CreateFloor(OBJECT *input);
void DrawObs();


//void DrawEnv();

int wi;
int he;
char *dest;
bool g_swapcontrol;	
UINT_PTR nid,nidavi;
GLfloat light_position0[] = { 25.0f, 5.0f, 25.0f, 1.0f};
GLfloat lightambient0[] ={ 0.32f, 0.32f, 0.32f, 1.0f};
GLfloat LightDiffuse0[]= { 0.7f, 0.6f, 0.6f, 1.0f };	
GLfloat lightnull[] ={ 0.0f, 0.0f, 0.0f, 1.0f};
GLfloat LightDiffuse1[]= { 0.8f, 0.8f, 0.8f, 1.0f };	


Texperiment prova[TRIALNUMBER];
int numprova;


bool CheckExtension(char* extensionName)
{
	// get the list of supported extensions
	char* extensionList = (char*) glGetString(GL_EXTENSIONS);

	if (!extensionName || !extensionList)
		return false;

	while (*extensionList)
	{
		// find the length of the first extension substring
		unsigned int firstExtensionLength = strcspn(extensionList, " ");


		if (strlen(extensionName) == firstExtensionLength &&
			strncmp(extensionName, extensionList, firstExtensionLength) == 0)
		{
			return true;
		}

		// move to the next substring
		extensionList += firstExtensionLength + 1;
	}

	return false;
} // end CheckExtension()

void CMain::InitVariables()
{

	numprova=0;
	cz=0;								// X, Y & Z Position

	col_amb1[0]=0.60;
	col_amb1[1]=0.55;
	col_amb1[2]=0.55;
	col_amb1[3]=1;
	col_amb2[0]=1; /// if touched
	col_amb2[1]=1;
	col_amb2[2]=1;
	col_amb2[3]=1;
	col_amb3[0]=0;/// if touched
	col_amb3[1]=0;
	col_amb3[2]=0;
	col_amb3[3]=1;

	targettouch=0;
	step=0;
	steps=200;							// Step Counter And Maximum Number Of Steps
	Finish=false;
	dx=0; //// l'uso di queste 4 variabili  è stato sostituito da camera.phi e tetaofs
	dy=0;

	currentbutton= -1;
	buttondown=0;
	v=0;
	w=0;
	xmorduc=2;
	ymorduc=2;
	amorduc=45;


	StereoEnable=false;
	SwapControl=false;
	ManSwapEnable=false;
	TextureEnable=00;
	CameraOnRobot=true;
	HDIEnable=true;
	g_swapcontrol=CheckExtension("WGL_EXT_swap_control");
	if (g_swapcontrol)
	{
		pwglSwapIntervalEXT = (PFNWGLSWAPINTERVALEXTPROC) wglGetProcAddress("wglSwapIntervalEXT");
	}


	flo=0;
}


void CMain::WriteParam()
{
	GLfloat parametri[16];
	FILE *fp;
	glGetFloatv(GL_VIEWPORT,parametri);
	fp=fopen("param.txt","w");
	for (int i=0;i<4;i++)
	{
		fprintf (fp,"%f ",parametri[i]);
	}
	fprintf(fp,"\n"); // per portare a capo
	glGetFloatv(GL_MODELVIEW_MATRIX,parametri);

	for (int i=0;i<16;i++)
	{
		fprintf (fp,"%f ",parametri[i]);
	}
	fprintf(fp,"\n"); // per portare a capo
	glGetFloatv(GL_PROJECTION_MATRIX,parametri);
	for (int i=0;i<16;i++)
	{
		fprintf (fp,"%f ",parametri[i]);
	}
	fprintf(fp,"\n"); // per portare a capo
	glGetFloatv(GL_PERSPECTIVE_CORRECTION_HINT,parametri);
	fprintf (fp,"%f ",parametri[0]);
	fclose(fp);
}


void CMain::HandleMouseMotion()
{

	if (buttondown==0) 
	{
		tetaold=camera.tetaofs;
		phiold=camera.phi;
	}
	if (buttondown==1)
	{

		//dx = dxold + mouseup.x - mousedown.x;
		//dy = dyold + mouseup.y - mousedown.y;
		camera.tetaofs = tetaold - (mouseup.x - mousedown.x)*DTOR; // FILIPPO INTRODURRE PARAMETRO DI SCALAMENTO DEL TIPO 360/ NUMERO PIXEL ORIZZ
		camera.phi     = phiold  + (mouseup.y - mousedown.y)*DTOR;
	}

	//glRotatef(-dx,0,1,0);
	//glRotatef(dy,1,0,-1);

	//RotateCamera(-dx,dy,0);  /// sta routine non funziona bene per me.

}

void CMain::CreateFloor(OBJECT *input)
{

	if(TextureEnable>1)
		glEnable(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, texture[1]);
	GLfloat col_blu[] = { 0.0f, 0.0f, 0.3f,1.0f};
	glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE,col_blu);

	glBegin(GL_QUADS);		
	glNormal3f(0.0f,1.0f,0.0f);			
	glTexCoord2f(00.0f,00.0f); glVertex3f(input->mx,0,input->my);
	glTexCoord2f(0.0f, 10.0f); glVertex3f(input->mx,0,input->My);
	glTexCoord2f(10.0f, 10.0f); glVertex3f(input->Mx,0,input->My);
	glTexCoord2f(10.0f, 0.0f); glVertex3f(input->Mx,0,input->my);
	glEnd();		
	glMaterialfv(GL_FRONT, GL_DIFFUSE,col_amb1);
	glDisable(GL_TEXTURE_2D);
}

void CMain::LoadEnvRPC()
{
	int Connected=0;
	XYZ versa;
	double punto_new[3];
	char flag=0,draw;
	if (Connected) 
	{
		Connected=0;
		RPC_CloseConn();
	} else {
		if(RPC_OpenConn("localhost"))
			Connected=1;
		else 
		{
			AfxMessageBox("Can't connect to the robot, check server status");
		}
	}
	if (Connected)
		RPC_Read_Map(&MapData);
	if (Connected)
	{
		Connected=0;
		RPC_CloseConn();
	}
	bool memoria[80000];
	//memcpy(memoria,MapData.Map,sizeof(MapData.Map));
	for (int kk=0; kk<sizeof(MapData.Map);kk++)
	{
		//MapData.Map[kk];
		memoria[8*kk+0]=(MapData.Map[kk] & (1 << 0));
		memoria[8*kk+1]=(MapData.Map[kk] & (1 << 1));
		memoria[8*kk+2]=MapData.Map[kk] & (1 << 2);
		memoria[8*kk+3]=MapData.Map[kk] & (1 << 3);
		memoria[8*kk+4]=MapData.Map[kk] & (1 << 4);
		memoria[8*kk+5]=MapData.Map[kk] & (1 << 5);
		memoria[8*kk+6]=MapData.Map[kk] & (1 << 6);
		memoria[8*kk+7]=MapData.Map[kk] & (1 << 7);
	}
	for (int y=0; y<MapData.DimY;y++)
		for (int x=0; x<MapData.DimX;x++)
		{	
			if (memoria[x+(MapData.DimY-1)*y])
			{

				versa.x=x;
				versa.y=y;
				versa.z=0;
				coda.push_back(versa);
			}
		}

		XYZ temp;	
		int i=0;
		ostacoli.verts=coda.size();
		ostacoli.points=(VERTEX*)malloc(sizeof(VERTEX)*coda.size());
		while (coda.size()!= 0)
		{
			temp=coda.back();
			coda.pop_back();
			ostacoli.points[i].x = temp.x;		// Sets Objects (k) points.x Value To rx
			ostacoli.points[i].y = temp.y;		// Sets Objects (k) points.y Value To ry
			ostacoli.points[i].z = temp.z;		// Sets Objects (k) points.z Value To rz
			i++;
		}
}

void CMain::LoadEnv(GLfloat dim, char *filename)
{
	//Data\\pianta4.bmp
	XYZ versa;
	int mx=0,Mx=0,my=0,My=0;
	//CImage *bitmape;
	std::list <XYZ> cle;
	bitmap= new CImage();
	bitmap->Load(filename);
	if (!bitmap->IsDIBSection()) MessageBox(theApp.m_wndMain.m_hWnd,"no!!","",2);
	////p_bmp=(unsigned char *)bitmap->GetBits();
	///////////// convert jpg in dib
	//get some data we need...
	int pitch=bitmap->GetPitch()*-1;
	int height=bitmap->GetHeight();
	int width=bitmap->GetWidth();
	sBGR* pPixel;
	char* pBitmap=(char*)bitmap->GetBits()-pitch*(height-1);
	for (int y=0; y<height;y++)
		for (int x=0; x<width;x++)
		{
			pPixel=(sBGR*)&pBitmap[x*sizeof(sBGR)+(height-1-y)*pitch];
			if (pPixel->r+pPixel->g+pPixel->b<=300)
			{
				versa.x=x;
				versa.y=y;
				versa.z=0;

				cle.push_back(versa);
				if (versa.x<mx) mx=versa.x;
				if (versa.x>Mx) Mx=versa.x;
				if (y<my) my=y;
				if (y>My) My=y;
			}
		}
		bitmap->Destroy();


		ostacoli.Mx=Mx;
		ostacoli.mx=mx;
		ostacoli.My=My;
		ostacoli.my=my;


		///// convert the list in obj
		XYZ temp;	
		int i=0;
		ostacoli.verts=cle.size();
		ostacoli.points=(VERTEX*)malloc(sizeof(VERTEX)*cle.size());
		while (cle.size()!= 0)
		{
			temp=cle.back();
			cle.pop_back();
			ostacoli.points[i].x = temp.x;	// Sets Objects (k) points.x Value To rx
			ostacoli.points[i].y = temp.y;	// Sets Objects (k) points.y Value To ry
			ostacoli.points[i].z = temp.z;	// Sets Objects (k) points.z Value To rz
			ostacoli.points[i].value = dim;	// Sets Objects (k) points.z Value To rz
			ostacoli.points[i].touched=0;
			i++;
		}
		cle.clear();
}
void CMain::DrawObs()
{
	GLfloat tx,ty,tz,value;					// Temp X, Y & Z Variables
	// Begin Drawing Points
	int j=0;
	if(TextureEnable>1)
		glEnable(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, texture[0]);
	glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE,col_amb1);
	for(int i=0;i<ostacoli.verts;i++)	// Loop Through All The Verts Of obfloor (All Objects Have
	{												
		tx=ostacoli.points[i].x;						
		ty=ostacoli.points[i].z;						
		tz=ostacoli.points[i].y;						
		value=ostacoli.points[i].value;
		DrawPar(tx,ty+10*value,tz,1.01f*value,1.01f*value,20*value);  // SQUARE BASE
	}		
	glDisable(GL_TEXTURE_2D);	
	glBindTexture(GL_TEXTURE_2D, texture[1]);
}

AUX_RGBImageRec *LoadBMP(char *Filename)				// Loads A Bitmap Image
{
	FILE *File=NULL;									// File Handle

	if (!Filename)										// Make Sure A Filename Was Given
	{
		return NULL;									// If Not Return NULL
	}

	File=fopen(Filename,"r");							// Check To See If The File Exists

	if (File)											// Does The File Exist?
	{
		fclose(File);									// Close The Handle
		return auxDIBImageLoad(Filename);				// Load The Bitmap And Return A Pointer
	}

	return NULL;										// If Load Failed Return NULL
}




// Destination At The Same Time
GLvoid ReSizeGLScene(GLsizei width, GLsizei height)		// Resize And Initialize The GL Window
{

	if (height==0)										// Prevent A Divide By Zero By
	{
		height=1;										// Making Height Equal One
	}

	glViewport(0,0,width,height);						// Reset The Current Viewport
	glMatrixMode(GL_PROJECTION);						// Select The Projection Matrix
	glLoadIdentity();									// Reset The Projection Matrix

	// Calculate The Aspect Ratio Of The Window
	gluPerspective(45.0f,(GLfloat)width/(GLfloat)height,0.1f,100.0f);
	glMatrixMode(GL_MODELVIEW);							// Select The Modelview Matrix
	glLoadIdentity();									// Reset The Modelview Matrix

}


int CMain::LoadGLTextures()									// Load Bitmaps And Convert To Textures
{
	int Status=FALSE;									// Status Indicator

	AUX_RGBImageRec *TextureImage[1];					// Create Storage Space For The Texture
	memset(TextureImage,0,sizeof(void *)*1);           	// Set The Pointer To NULL

	// Load The Bitmap, Check For Errors, If Bitmap's Not Found Quit
	if (TextureImage[0]=LoadBMP("Data/wall.bmp"))
	{
		Status=TRUE;									// Set The Status To TRUE
		glGenTextures(1, &texture[0]);					// Create The Texture

		// Typical Texture Generation Using Data From The Bitmap
		glBindTexture(GL_TEXTURE_2D, texture[0]);

		//glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
		//glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);




		glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
		glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);


		glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_REPEAT);
		glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_REPEAT);

		//glTexImage2D(GL_TEXTURE_2D, 0, 3, TextureImage[0]->sizeX, TextureImage[0]->sizeY, 0, GL_RGB, GL_UNSIGNED_BYTE, TextureImage[0]->data);


		// Create MipMapped Texture

		glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
		glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
		gluBuild2DMipmaps(GL_TEXTURE_2D,3, TextureImage[0]->sizeX, TextureImage[0]->sizeY,GL_RGB, GL_UNSIGNED_BYTE, TextureImage[0]->data);


		glTexEnvf(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_DECAL); // senza è glmodulate
	}

	memset(TextureImage,0,sizeof(void *)*1);           	// Set The Pointer To NULL

	// Load The Bitmap, Check For Errors, If Bitmap's Not Found Quit
	if (TextureImage[0]=LoadBMP("Data/floor.bmp"))
	{
		Status=TRUE;									// Set The Status To TRUE
		glGenTextures(1, &texture[1]);					// Create The Texture

		// Typical Texture Generation Using Data From The Bitmap
		glBindTexture(GL_TEXTURE_2D, texture[1]);
		glTexImage2D(GL_TEXTURE_2D, 0, 3, TextureImage[0]->sizeX, TextureImage[0]->sizeY, 0, GL_RGB, GL_UNSIGNED_BYTE, TextureImage[0]->data);


		glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
		glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
		glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_REPEAT);
		glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_REPEAT);

		glTexEnvf(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE); // FILIPPO GL_MODULATE
	}

	memset(TextureImage,0,sizeof(void *)*1);           	// Set The Pointer To NULL

	// Load The Bitmap, Check For Errors, If Bitmap's Not Found Quit

	//gluBuild2DMipmaps(GL_TEXTURE_2D, 3, TextureImage[0]->sizeX, TextureImage[0]->sizeY, GL_RGB, GL_UNSIGNED_BYTE, TextureImage[0]->data);



	if (TextureImage[0])									// If Texture Exists
	{
		if (TextureImage[0]->data)							// If Texture Image Exists
		{
			free(TextureImage[0]->data);					// Free The Texture Image Memory
		}

		free(TextureImage[0]);								// Free The Image Structure
	}

	return Status;										// Return The Status
}


bool CMain::Initialize()										// All Setup For OpenGL Goes Here
{



	InitVariables();
	camera.stereo       = NOSTEREO;
	camera.focallength=8.4f;
	camera.cursordepth  = camera.focallength;
	camera.screenwidth  = theApp.m_Width;
	camera.screenheight = theApp.m_Height;
	camera.aperture=64.8f;
	camera.CameraOnRobot(&robot1);
	glBlendFunc(GL_ONE_MINUS_SRC_COLOR,GL_ZERO);					// Set The Blending Function For Translucency
	//glBlendFunc (GL_SRC_ALPHA_SATURATE, GL_ONE);    //ANTIALIAS?
	glClearColor(0.0f,0.0f,0.0f, 0.0f);				// This Will Clear The Background Color To Black
	glClearDepth(1.0);									// Enables Clearing Of The Depth Buffer
	glDepthFunc(GL_LESS);								// The Type Of Depth Test To Do
	//	glEnable(GL_DEPTH_TEST);							// Enables Depth Testing
	glShadeModel(GL_SMOOTH);							// Enables Smooth Color Shading

	glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);	// Really Nice Perspective Calculations
	gluPerspective(camera.focallength,(GLfloat)camera.screenwidth/(GLfloat)camera.screenheight,0.1f,100.0f);
	BuildFont(theApp.m_pDC->m_hDC);											// Build The Font
	glClearColor (0.05f, 0.0f, 0.0f, 0.0f);
	// 	GLfloat mat_specular[] = { 0.3f, 0.3f, 0.3f, 0.0f };
	//GLfloat mat_emission[] = { 0.0, 0.0, 1.0, 0.4 };
	//GLfloat mat_shininess[] = { 0.02f };
	//glMaterialf(GL_FRONT, GL_EMISSION,100);
	//glMaterialfv(GL_FRONT, GL_SHININESS, mat_shininess);



	glShadeModel (GL_SMOOTH);
	glMaterialfv(GL_FRONT, GL_DIFFUSE,col_amb1);
	glMaterialfv(GL_FRONT, GL_AMBIENT,col_amb3);



	glLightfv(GL_LIGHT0, GL_AMBIENT, lightambient0);				// Setup The Ambient Light
	glLightfv(GL_LIGHT0, GL_DIFFUSE, LightDiffuse0);				// Setup The Ambient Light
	glLightfv(GL_LIGHT0, GL_POSITION, light_position0);
	glLightfv(GL_LIGHT1, GL_AMBIENT, lightnull);				// Setup The Ambient Light
	glLightfv(GL_LIGHT1, GL_DIFFUSE, LightDiffuse1);				// Setup The Ambient Light

	glEnable(GL_LIGHT0);
	glEnable(GL_LIGHT1);
	glEnable(GL_LIGHTING);

	float fogColor[4] = {0.1, 0.1, 0.1, 1.0f};			// Let's make the Fog Color black too
	glFogi(GL_FOG_MODE, GL_LINEAR);						// Set The Fog Mode
	glFogfv(GL_FOG_COLOR, fogColor);					// Set The Fog Color
	glFogf(GL_FOG_DENSITY, 0.10f);						// Set How Dense Will The Fog Be
	glHint(GL_FOG_HINT, GL_FASTEST);					// Set The Fog's calculation accuracy
	glFogf(GL_FOG_START, 15.0f);							// Set The Fog's Start Depth
	glFogf(GL_FOG_END, 130.0f);							// Set The Fog's End Depth
	glEnable(GL_FOG);			// Enable fog (turn it on)

	glCullFace(GL_BACK );
	glEnable(GL_CULL_FACE);
	glEnable(GL_DEPTH_TEST);


	// Create Nearest Filtered Texture
	LoadGLTextures();

	ExperimentSet2();
	//LoadEnv(1,"Data//map.jpg");

	char data[26];
	strcpy(data,"Data/");
	strcat(data,nomemappe[prova[numprova].nmap]);
	LoadEnv(1,data);

	objload("Data\\target.txt",&target)	;					// Loads Object From File (name)
	target_selected=random(&target);


	LoadImage("Data\\webcam.jpg",&dest,&he,&wi);

	robot1.xr=-50;
	robot1.yr=-50;
	robot1.tetar=-3.141;


	TextureEnable=0;
	SelectTexture();

	//glEnable(GL_BLEND);

	// test of openGL extensions	
	/*	  if (pwglSwapIntervalEXT)
	{
	pwglSwapIntervalEXT(0);
	}
	*/

	//		AVIStart();


	radar=(GLfloat*) malloc(sizeof(GLfloat)*theApp.m_Width);

	return TRUE;										// Initialization Went OK
}






//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

static float angle=0,rot1,rot2;

CMain::CMain()
{
	// Start Of User Initialization	
	LastScreenshotTime = -1;

	char name[64];
	SimNumber=CountFromFile("Data//countersim.txt");
	sprintf(name,"trace/data_%d.txt",SimNumber);
	FpData=fopen(name,"w");

}

CMain::~CMain()
{

}


// Function name	: CMain::Initialize
// Description	    : This function will initialize your opengl application
//					  Put in what you need
// Return type		: BOOL 
//					  return TRUE on success or FALSE on error (example :texture/model not found = FALSE)

// Function name	: CMain::Deinitialize
// Description	    : This function will Deinitialize your opengl application
//					  destroy all objects here
// Return type		: void 
void CMain::Deinitialize()
{
	AVIFinish();
}


// Function name	: CMain::Update
// Description	    : this function will be called every xx milliseconds
//					  update your variables through milliseconds
// Return type		: void 
// Argument         : DWORD milliseconds


// Function name	: CMain::Draw
// Description	    : this function will draw (blt) your opengl scene to the window
// Return type		: void 
void CMain::Draw(bool Update)
{




	if (buttondown==1)
		GetCursorPos(&mousedown);
	if (buttondown==0)
		GetCursorPos(&mouseup);

	if (g_swapcontrol)
		if (SwapControl==true)	
			pwglSwapIntervalEXT(0);
		else	
			pwglSwapIntervalEXT(1);



	GLfloat parametri[16];  // controllo
	glGetFloatv(GL_PROJECTION_MATRIX,parametri);

	//glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);	// Clear The Screen And The Depth Buffer


	glColor3f(1,1,1);
	glLoadIdentity();		
	// Reset The View
	if (HDIEnable==true)
	{
		///////////////////////////////////////////////////////7/ Insert Text
		glMatrixMode(GL_PROJECTION);
		glPushMatrix();
		glLoadIdentity();
		glOrtho(-1,50,-1,50,0,5);
		glMatrixMode(GL_MODELVIEW);
		glDisable(GL_LIGHT0);
		glDisable(GL_LIGHTING);
		//		glDisable(GL_DEPTH_TEST);
		glTranslatef(0,0,camera.focallength);
		/*
		XYZ p;
		GLfloat grey;
		glBegin(GL_POINT);
		for (int n=0;n<10000; n++)
		{
		//	glColor3i(rand()/100,rand()/100,rand()/100);
		//	glVertex2f(rand()/100,rand()/100);

		p.x = rand() / (double)RAND_MAX ;
		p.y = rand() / (double)RAND_MAX ;
		p.z = rand() / (double)RAND_MAX ;
		grey = (rand() / (double)RAND_MAX) / 2 + 0.5;

		//Normalise(&p);
		glColor3f(0,grey,0);
		glVertex3f(50*p.x,50*p.y,0);

		}
		glEnd();
		*/

		//glPrint(0,0,0,"posizione su x: %d y: %d",mouseup.x,mouseup.y);
		//glPrint(0,1,1,"posizione giu x: %d y: %d",mousedown.x,mousedown.y);	
		if (CollisionInterrupt>0)
		{
			glColor3f(1.0f,0.0f,0.0f);
			glPrint(40,1,0,"COLLISION !");
		}
		glColor3f(0.0f,1.0f,0.0f);
		//glPrint(0,1,0,"TARGET : x:%f y:%f",target.points[target_selected].x,target.points[target_selected].y);	
		//glPrint(0,1,0,"mappa n:%d collisions:%d",prova[numprova-1].nmap,robot1.collisions);
		//glPrint(0,2,0,"camera.aperture: %f",camera.aperture);	
		//glPrint(0,3,0,"camera.focal: %.3f eyesep:%.3f",camera.focallength,camera.eyesep);	
		//glPrint(0,4,0,"cx: %.3f cy: %.3f cz: %.3f SimNumber:%d",cx,cy,cz,SimNumber);	
		glPrint(0,49,0,"Test Set #%d", SimNumber);	
		glPrint(0,48,0,"robot.Time: %.3f", robot1.Time);	
		glPrint(0,47,0,"Xr: %.3f Yr: %.3f Tetha: %.3f",robot1.xr,robot1.yr,robot1.tetar/DTOR);	
	   //glPrint(0,7,0,"v: %6f w: %6f ",robot1.v,flo);
		/*int len=0;
		if (SwapControl)
			len+=glPrint(0,49,0,"SWAP CONTROL");	
		if (StereoEnable)
			len+=glPrint(len,49,0,"DUALSTEREO");	
		if (ManSwapEnable)
			len+=glPrint(len,49,0,"Shutter Glasses");	
		if (AnaglyphEnable)
			len+=glPrint(len,49,0,"Anaglyph");	
		if (StartRecord)
		{
			glColor3f(1.0f,0.0f,0.0f);
			len+=glPrint(len,49,0,"RECORD %d",SimNumber);	
			glColor3f(0.0f,1.0f,0.0f);
		}
		glColor3f(0.0f,0.0f,1.0f);
		if (CameraOnRobot)
			len+=glPrint(len,49,0,"On Robot");	*/

		//else 
		//	len+=glPrint(len,49,0,"Fixed");	
		//glColor3f(0.0f,1.0f,0.0f);


		/*  // cosi appiccico un buffer in un punto.
		GLfloat pixel[400][3];
		for (int i=0;i<400;i++)
		{
		pixel[i][1]=1;
		pixel[i][2]=1;
		pixel[i][0]=1;
		}
		glLoadIdentity();
		glRasterPos3f(0.0f,0.0f,0.0f);
		glDrawPixels(20,20,GL_RGB,GL_FLOAT,pixel);
		*/	
		if (ImageEnable)
		{
			/*	da webcam.*/
			glLoadIdentity();
			glRasterPos3f(00.0f,00.0f,0.0f);
			glDrawPixels(wi,he,GL_BGR,GL_UNSIGNED_BYTE,dest);
		}

		glLoadIdentity();
		glRasterPos3f(-1.0f,00.0f,0.0f);
		//glDrawPixels(theApp.m_Width,1,GL_GREEN,GL_FLOAT,radar);

		glColor3f(0,1,0);
		//glBegin(GL_TRIANGLE_STRIP);
		glTranslatef(-0.5f,0,0);
		/*for(float po=0;po<theApp.m_Width;po+=1)
		{
		//		glVertex3f(po/theApp.m_Width*50-1,radar[po],0);
		glRasterPos3f(-1.0f+51.0/theApp.m_Width*po,1000*radar[(int)po] ,0.0f);
		glDrawPixels(1,1,GL_RED,GL_FLOAT,radar);
		}*/
		//glEnd();



		//		glEnable(GL_DEPTH_TEST);
		glEnable(GL_LIGHT0);
		glEnable(GL_LIGHTING);
		glMatrixMode(GL_PROJECTION);
		glPopMatrix();
		glMatrixMode(GL_MODELVIEW);
		//////////////////////////////////////////////////////// Exit Text
		glLoadIdentity();
	}


	HandleMouseMotion();
	glTranslatef(cx,cy,cz);								// Translate The The Current Position To Start Drawing

	glTranslatef(0.0f,0.0f,-5.4f);								// Translate The The Current Position To 


	gluLookAt(camera.vp.x + r.x,camera.vp.y + r.y,camera.vp.z + r.z,
		camera.vp.x + r.x + camera.vd.x,
		camera.vp.y + r.y + camera.vd.y,
		camera.vp.z + r.z + camera.vd.z,
		camera.vu.x,camera.vu.y,camera.vu.z);
	glPushMatrix();

	if (CameraOnRobot)
		camera.CameraOnRobot(&robot1);  /// aggiorna vp vd vu
	else camera.CameraOnEnviroment();	

	if(buttondown==1)
	{
		//WriteParam();
	}
	//glEnable(GL_SCISSOR_TEST);	// Enable Scissor Testing
	////////////////////////////

	DrawObs();


	////// DRAW TARGET 
	//glTranslatef(target.points[target_selected].x,target.points[target_selected].z,target.points[target_selected].y);
	//GLUquadricObj *quadrica=gluNewQuadric();
	//GLfloat col_tar[] ={0.0f, 0.7f, 0.0f, 1.0f};
	//glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE,col_tar);
	//target.points[target_selected].value=1.8;
	//gluSphere(quadrica,target.points[target_selected].value,32,32);
	//glTranslatef(-target.points[target_selected].x,-target.points[target_selected].z,-target.points[target_selected].y);
	//////////////////////7

	CreateFloor(&ostacoli);

	if (Update==true)
		robot1.move();

	int collisionold=robot1.collisions;
	CollisionInterrupt=robot1.RobColl2(&ostacoli,true);

	if ((robot1.collisions-collisionold) >0 && StartRecord==true)
	{
		fprintf(robot1.fp ,"%f %f %f %f %d %f %f %f %f\n",
			robot1.xr,robot1.yr,robot1.tetar,
			robot1.Time,
			robot1.collisions,
			robot1.vrx,robot1.vry,robot1.v,robot1.w);

		//-------------------------------------------------------
		// 		int _width = theApp.m_Width;//1024;
		// 		int _height = theApp.m_Height;//768
		
		// 		glReadBuffer(GL_BACK);
		// 		GLvoid * imageData = malloc(_width * _height * (16)); //Allocate memory for storing the image
		// 		glReadPixels(0, 0, _width, _height, GL_RGBA, GL_UNSIGNED_BYTE, imageData); //Copy
		
		// 		corona::Image * myImage = corona::CreateImage(_width, _height, corona::PF_R8G8B8A8, imageData);
		// 		corona::FlipImage(myImage, corona::CA_X); //Flip the image
		// 		corona::SaveImage("image.bmp", corona::FF_PNG, myImage); //Save i
		
		// 		delete myImage;
		// 		free(imageData);
		//-------------------------------------------------------	

	}


	//	glEnable(GL_TEXTURE_2D);
	robot1.PaintRobot2(); /// 0 0 0 su base robot.
	//glBindTexture(GL_TEXTURE_2D, texture[1]);
	glPixelTransferf(GL_DEPTH_BIAS,0);
	glPixelTransferf(GL_DEPTH_SCALE,0.01);
	glReadPixels(0,theApp.m_Height/2,theApp.m_Width,1,GL_DEPTH_COMPONENT ,GL_FLOAT,radar);

	/*	if (robot1.RobColl(&target)>0)
	{
	robot1.LoadSPFromFile("Data/robotstartposition.txt",2);
	target_selected=random(&target);
	robot1.v=0;
	robot1.w=0;
	Pause=true;
	}
	*/

	if (robot1.RobColl(&target,false)>0 && targettouch==0)
	{
		prova[numprova-1].time=robot1.Time;
		robot1.Time=0;
		targettouch++;
		StopRec();
		robot1.vlim=0;
		if (numprova<=TRIALNUMBER)
		{
			char data[26];
			strcpy(data,"Data/");
			strcat(data,nomemappe[prova[numprova-1].nmap]);
			LoadEnv(1,data);
		}
	}
	//	if (targettouch>0 )
	//		robot1.v=0;
	//glFlush();// Flush The GL Rendering Pipeline
	if ((robot1.Time>TEMPO && targettouch>0) || Finish==true )
	{
		Finish=false;
		cx=100;
		cy=100;
		cz=100;
		targettouch=0;

	}



}




void CMain::HandleDisplay(bool swapping)
{
	GLdouble ratio,radians,wd2,ndfl;
	GLdouble left,right,top,bottom;	
	if (StereoEnable)
	{
		glDrawBuffer(GL_BACK);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);




		//camera.aperture=45;
		camera.screenwidth  = theApp.m_Width;//1024;
		camera.screenheight = theApp.m_Height;//768;
		//camera.screenwidth *=2;		/// qui si raddoppiano la dimensione per usare i mirror l/r.
		camera.cursordepth  = camera.focallength;
		camera.farp=150;
		camera.nearp=1;
		/* Misc stuff needed for the frustum */
		ratio   = camera.screenwidth / (double)camera.screenheight;
		ratio /= 2;
		radians = DTOR * camera.aperture / 2;
		wd2     = camera.nearp * tan(radians);
		ndfl    = camera.nearp / camera.focallength;
		top     =   wd2;
		bottom  = - wd2;

		/* Determine the right eye vector */
		CROSSPROD(camera.vd,camera.vu,r);
		Normalise(&r);
		r.x *= camera.eyesep / 2.0;
		r.y *= camera.eyesep / 2.0;
		r.z *= camera.eyesep / 2.0;


		glMatrixMode(GL_PROJECTION);			// Matrix for RIGHT
		glLoadIdentity();
		left  = - ratio * wd2 - 0.5 * camera.eyesep * ndfl;
		right =   ratio * wd2 - 0.5 * camera.eyesep * ndfl;
		glFrustum(left,right,bottom,top,camera.nearp,camera.farp);
		glViewport(camera.screenwidth/2,0,camera.screenwidth/2,camera.screenheight);
		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity();
		glDrawBuffer(GL_BACK_RIGHT);
		//glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
		Draw(true);

		glMatrixMode(GL_PROJECTION);		//MAtrix for LEFT
		glLoadIdentity();
		left  = - ratio * wd2 + 0.5 * camera.eyesep * ndfl;
		right =   ratio * wd2 + 0.5 * camera.eyesep * ndfl;
		glFrustum(left,right,bottom,top,camera.nearp,camera.farp);
		glViewport(0,0,camera.screenwidth/2,camera.screenheight);
		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity();
		r.x=-r.x;
		r.y=-r.y;
		r.z=-r.z;
		glDrawBuffer(GL_BACK_LEFT);
		//glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
		Draw(false);

		SwapBuffers (theApp.m_pDC->m_hDC);					// Swap Buffers (Double buffer)
	}
	else
	{
		if ( swapping && ManSwapEnable)	
		{
			//
			// ManSwapEnable è per shutterglasses
			//swapping per disegnare una volta ciascuno gli occhi.
			// se 1 fa il destro e aggiorna
			// se 0 il sinistro
			/*
			/////////////////// TEST FOR STEREO
			GLboolean StereoSupported;
			glGetBooleanv(GL_STEREO,&StereoSupported);
			if (StereoSupported==0)  // tanto ci entra sempre!
			{	
			AfxMessageBox("Graphics Card does not support professional stereo vision");
			theApp.TerminateApplication ();						// Terminate The Program
			}
			*/

			CROSSPROD(camera.vd,camera.vu,r);
			Normalise(&r);

			r.x *= camera.eyesep / 2.0;
			r.y *= camera.eyesep / 2.0;
			r.z *= camera.eyesep / 2.0;
			camera.screenwidth  = theApp.m_Width;//1024;
			camera.screenheight = theApp.m_Height;//768

			//camera.aperture=45;
			camera.cursordepth  = camera.focallength;
			camera.farp=150;
			camera.nearp=1;
			/* Misc stuff needed for the frustum */
			ratio   = camera.screenwidth / (double)camera.screenheight;
			//ratio /= 2;
			radians = DTOR * camera.aperture / 2;
			wd2     = camera.nearp * tan(radians);
			ndfl    = camera.nearp / camera.focallength;
			top     =   wd2;
			bottom  = - wd2;

			glMatrixMode(GL_PROJECTION);			// Matrix for RIGHT
			glLoadIdentity();
			left  = - ratio * wd2 - 0.5 * camera.eyesep * ndfl;
			right =   ratio * wd2 - 0.5 * camera.eyesep * ndfl;
			glFrustum(left,right,bottom,top,camera.nearp,camera.farp);
			glViewport(0,0,camera.screenwidth,camera.screenheight);
			glMatrixMode(GL_MODELVIEW);
			glLoadIdentity();
			if (AnaglyphEnable==true)
				glColorMask(GL_FALSE, GL_TRUE, GL_TRUE, GL_TRUE);
			else
				glDrawBuffer(GL_BACK_RIGHT);

			glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
			Draw(true);
		}
		else
			//if (1)
		{
			CROSSPROD(camera.vd,camera.vu,r);
			Normalise(&r);
			r.x *= camera.eyesep / 2.0;
			r.y *= camera.eyesep / 2.0;
			r.z *= camera.eyesep / 2.0;
			camera.screenwidth  = theApp.m_Width;//1024;
			camera.screenheight = theApp.m_Height;//768
			r.x =-r.x ;
			r.y =-r.y ;
			r.z =-r.z ;

			//camera.aperture=45;
			camera.cursordepth  = camera.focallength;
			camera.farp=150;
			camera.nearp=1;
			/* Misc stuff needed for the frustum */
			ratio   = camera.screenwidth / (double)camera.screenheight;
			//ratio /= 2;
			radians = DTOR * camera.aperture / 2;
			wd2     = camera.nearp * tan(radians);
			ndfl    = camera.nearp / camera.focallength;
			top     =   wd2;
			bottom  = - wd2;

			glMatrixMode(GL_PROJECTION);			// Matrix for RIGHT
			glLoadIdentity();
			left  = - ratio * wd2 + 0.5 * camera.eyesep * ndfl;
			right =   ratio * wd2 + 0.5 * camera.eyesep * ndfl;
			glFrustum(left,right,bottom,top,camera.nearp,camera.farp);



			//	glMatrixMode (GL_PROJECTION);						// Select The Projection Matrix
			//	glLoadIdentity ();				
			//camera.focallength=45;// Reset The Projection Matrix
			//	gluPerspective (camera.focallength, (GLfloat)(camera.screenwidth)/(GLfloat)(camera.screenheight),			// Calculate The Aspect Ratio Of The Window
			//			1.0f, 100.0f);		
			glViewport(0,0,camera.screenwidth,camera.screenheight);
			glMatrixMode(GL_MODELVIEW);
			glLoadIdentity();
			glDrawBuffer(GL_BACK_LEFT);

			if (AnaglyphEnable==true)	
			{
				glColorMask(GL_TRUE, GL_FALSE, GL_FALSE, GL_TRUE);
			}
			glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);	
			//robot1.move();
			if (AnaglyphEnable==true || ManSwapEnable==true)
				Draw(false);
			else
			{
				Draw(true); /// FILIPPO forse false?
			}
			SwapBuffers (theApp.m_pDC->m_hDC);					// Swap Buffers (Double 

		}
		/// GLubyte *pImageBits1 = new GLubyte[3*rcClient.right*rcClient.bottom*8];

	}

}


VOID CALLBACK TimerOK(          HWND hwnd,    UINT uMsg,    UINT_PTR idEvent,    DWORD dwTime)
{
	// fprintf(theApp.m_appMain.robot1.fp ,"%f %f %f %f %d %f %f %f %f \n",theApp.m_appMain.robot1.xr,theApp.m_appMain.robot1.yr,theApp.m_appMain.robot1.tetar,theApp.m_appMain.robot1.Time,theApp.m_appMain.robot1.collisions,theApp.m_appMain.robot1.vrx,theApp.m_appMain.robot1.vry,theApp.m_appMain.robot1.v,theApp.m_appMain.robot1.w);
	//theApp.m_appMain.OnAvigenerationGenerate() ;
}

void CMain::StartRec()
{
	char name[64];
	nid=SetTimer(theApp.m_wndMain.m_hWnd,timer0,40,TimerOK);
	LARGE_INTEGER li;
	QueryPerformanceCounter(&li);
	//SimNumber=CountFromFile("Data//countersim.txt");
	sprintf(name,"path_simulation/path_%d.txt",SimNumber);
	robot1.fp=fopen(name,"w");
	robot1.Time=0;
	robot1.collisions=0;
	//fprintf(theApp.robot1.fp,"inizio simulazione\n");
	StartRecord=true;

}

void CMain::StopRec()
{
	KillTimer(theApp.m_wndMain.m_hWnd,nid);
	fclose(robot1.fp);
	StartRecord=false;
}

void CMain::HandleKeyboard(char *keyDown)
{
	char name[64];
	float temp0;
	if(keyDown['G'])								// Is Page Up Being Pressed?
		camera.phi-=0.02f;								// Increase xspeed

	if(keyDown['T'])									// Is Page Up Being Pressed?
		camera.phi+=0.02f;								// Decrease xspeed

	if(keyDown['H'])								// Is Page Up Being Pressed?
		camera.tetaofs+=0.01f;								// Increase yspeed

	if(keyDown['F'])								// Is Page Up Being Pressed?
		camera.tetaofs-=0.01f;								// Decrease yspeed

	if (keyDown['Q'])									// Is Q Key Being Pressed?
		cz+=0.3f;										// Move Object Away From Viewer

	if (keyDown['Z'])									// Is Z Key Being Pressed?
		cz-=0.3f;										// Move Object Towards Viewer

	if (keyDown['W'])									// Is W Key Being Pressed?
		cy-=0.3f;										// Move Object Up

	if (keyDown['S'])									// Is S Key Being Pressed?
		cy+=0.3f;										// Move Object Down

	if (keyDown['D'])									// Is D Key Being Pressed?
		cx-=0.3f;										// Move Object Right

	if (keyDown['A'])									// Is A Key Being Pressed?
		cx+=0.3f;										// Move Object Left

	if (keyDown['C'])									// Is D Key Being Pressed?
		robot1.radius+=0.3f;										// Move Object Right

	if (keyDown['X'])									// Is A Key Being Pressed?
		robot1.radius-=0.3f;										// Move Object Left

	
	/* Print image and data in trace folder */
	if (keyDown['P'])
		SaveStatus();


	if (keyDown[VK_F4])									// Is A Key Being Pressed?
		camera.eyesep=0.0f;					

	if (keyDown[VK_F5])		
	{
		camera.eyesep-=0.025;
	}
	if (keyDown[VK_F6])		
	{
		camera.eyesep+=0.025;
	}


	if (keyDown[VK_F7])	
	{
		camera.focallength-=0.1;;							
		glMatrixMode(GL_PROJECTION);						// Select The Projection Matrix
		glLoadIdentity();									// Reset The Projection Matrix
		gluPerspective(camera.focallength,(GLfloat)camera.screenwidth/(GLfloat)camera.screenheight,0.1f,100.0f);
		glMatrixMode(GL_MODELVIEW);							// Select The Modelview Matrix

	}
	if (keyDown[VK_F8])					
	{
		camera.focallength+=0.1;							
		glMatrixMode(GL_PROJECTION);						// Select The Projection Matrix
		glLoadIdentity();									// Reset The Projection Matrix
		// Calculate The Aspect Ratio Of The Window
		gluPerspective(camera.focallength,(GLfloat)camera.screenwidth/(GLfloat)camera.screenheight,0.1f,100.0f);
		glMatrixMode(GL_MODELVIEW);							// Select The Modelview Matrix

	}
	if (keyDown['E'])									// Is W Key Being Pressed?
		Finish=true;
	if (keyDown['1']&& !keyPressed['1'])								
	{
		keyPressed['1']=true;
		if (StartRecord==false)
		{
			/*nid=SetTimer(theApp.m_wndMain.m_hWnd,timer0,125,TimerOK);
			LARGE_INTEGER li;
			QueryPerformanceCounter(&li);
			SimNumber=CountFromFile("Data//countersim.txt");
			sprintf(name,"path_simulation/path_%d.txt",SimNumber);
			robot1.fp=fopen(name,"w");
			robot1.Time=0;
			robot1.collisions=0;
			//fprintf(theApp.robot1.fp,"inizio simulazione\n");
			StartRecord=true;*/
			StartRec();
		}
		else
		{
			/*	KillTimer(theApp.m_wndMain.m_hWnd,nid);
			fclose(robot1.fp);
			StartRecord=false;*/
			StopRec();
		}

	}
	if (!keyDown['1'])
	{
		keyPressed['1']=false;
	}
	if (keyDown['2'])
	{
		camera.phi=0;
		camera.tetaofs=0;
		cx=cy=cz=0;
	}
	if (keyDown['b'])
	{
		robot1.v=0;
		robot1.w=0;

	}
	if (keyDown['3'] && !keyPressed['3'])
	{
		keyPressed['3']=true;
		if (CameraOnRobot==false)
		{
			CameraOnRobot=true;
			cx=camera.cxb;
			cy=camera.cyb;
			cz=camera.czb;
			camera.tetaofs=camera.tetaofsb;
			camera.phi=camera.phib;
		} else {
			CameraOnRobot=false;
			camera.cxb=cx;
			camera.cyb=cy;
			camera.czb=cz;
			camera.tetaofsb=camera.tetaofs;
			camera.phib=camera.phi;
		}
	}
	if (!keyDown['3'])
	{
		keyPressed['3']=false;
	}

	if (keyDown['4'] && !keyPressed['4'])
	{
		keyPressed['4']=true;
		TextureEnable++;
		if (TextureEnable>2)
			TextureEnable=0;
		SelectTexture();

	}
	if (!keyDown['4'])
	{
		keyPressed['4']=false;
	}
	if (keyDown['5'] && !keyPressed['5'])
	{
		keyPressed['5']=true;
		target_selected=random(&target);
		robot1.v=0;
		robot1.w=0;
		Finish=false;

		/* put camera on the robort border */
		camera.phi=0;
		camera.tetaofs=0;

		cy=0;
		cx=0;
		cz=8;


		targettouch=0;
		robot1.vlim=7;
		if (numprova<TRIALNUMBER)
		{
			robot1.LoadSPFromFile("Data/robotstartposition.txt",prova[numprova].numberposition);
			LoadEnv(1,"Data/map/m13.jpg");
			if (StartRecord==true)
				StopRec();

			StartRec();

			prova[numprova].numpath=SimNumber;
			TextureEnable=prova[numprova].TextureEnable;
			SelectTexture();
		}
		else
		{

			sprintf(name,"exp2/exp2_%d.txt",SimNumber);
			FILE *fp;
			fp=fopen(name,"w");
			for (int i=0;i<TRIALNUMBER;i++)
			{
				fprintf(fp,"%d %d %f %d %d\n",prova[i].numberposition,prova[i].TextureEnable,prova[i].time,1+prova[i].nmap,prova[i].numpath);
			} 
			fclose(fp);
			theApp.TerminateApplication ();			
		}
		numprova++;
		robot1.Time=0;

	}
	if (!keyDown['5'])
	{
		keyPressed['5']=false;
	}
	if (keyDown['6'] && !keyPressed['6'])
	{
		keyPressed['6']=true;
		HDIEnable=!HDIEnable;
	}
	if (!keyDown['6'])
	{
		keyPressed['6']=false;
	}
	if (keyDown['7'] && !keyPressed['7'])
	{
		keyPressed['7']=true;
		if (SwapControl==false)
		{
			SwapControl=true;
		} else {
			SwapControl=false;
			glDrawBuffer(GL_BACK_RIGHT);
			glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
			glDrawBuffer(GL_BACK_LEFT);
		}
	}
	if (!keyDown['7'])
	{
		keyPressed['7']=false;
	}
	if (keyDown['8'] && !keyPressed['8'])
	{
		keyPressed['8']=true;
		if (AnaglyphEnable==false)
		{
			AnaglyphEnable=true;
			glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
			ManSwapEnable=true;
		} else {
			AnaglyphEnable=false;
			glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
		}
	}
	if (!keyDown['8'])
	{
		keyPressed['8']=false;
	}

	if (keyDown['9']&& !keyPressed['9'])
	{
		keyPressed['9']=true;
		if (ManSwapEnable==false)
		{
			ManSwapEnable=true;
			camera.eyesep=0.175;
		}
		else
		{
			ManSwapEnable=false;
			camera.eyesep=0;
			glDrawBuffer(GL_BACK_RIGHT);
			glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
		}
	}
	if (!keyDown['9'])
	{
		keyPressed['9']=false;
	}
	if (keyDown['0']&& !keyPressed['0'])
	{
		keyPressed['0']=true;
		if(StereoEnable==false)
		{
			StereoEnable=true;
		}
		else
		{
			StereoEnable=false;
			glDrawBuffer(GL_BACK_RIGHT);
			glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
			glDrawBuffer(GL_FRONT_RIGHT);
			glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
			glDrawBuffer(GL_BACK_LEFT);
		}
	}
	if (!keyDown['0'])
	{
		keyPressed['0']=false;
	}
	if (!keyDown[VK_UP])
	{
		keyPressed[VK_UP]=false;
		Pause=false;
	}
	if (!Pause)
	{
		if (keyDown[VK_UP])	//// ROBOT  FORWARD				// Is A Key Being Pressed?		
		{
			robot1.SetV(0.45f); 
		}
		if (keyDown[VK_DOWN])									// Is G Key Being Pressed?
		{
			robot1.SetV(-0.45f);
		}
		if (keyDown[VK_RIGHT])	//// ROBOT LEFT					// Is H Key Being Pressed?
		{
			robot1.SetW(+1.8f*DTOR);
		}
		if (keyDown[VK_LEFT])									// Is F Key Being Pressed?
		{
			robot1.SetW(-1.8f*DTOR);
		}
	}
	if (keyDown['N'])		
	{
		camera.aperture-=0.2;
	}
	if (keyDown['M'])		
	{
		camera.aperture+=0.2;
	}


	if (keyDown[VK_TAB] && !keyPressed[VK_TAB])		
	{
		keyPressed[VK_TAB]=true;
		ImageEnable=!ImageEnable;
	}
	if (!keyDown[VK_TAB])
	{
		keyPressed[VK_TAB]=false;
	}
	if (keyDown['V'])		
	{
		flo=+0.05f;
		lightambient0[0]+=flo;
		lightambient0[1]+=flo;
		lightambient0[2]+=flo;
		LightDiffuse0[0]+=flo;
		LightDiffuse0[1]+=flo;
		LightDiffuse0[2]+=flo;
		LightDiffuse1[0]+=flo;
		LightDiffuse1[1]+=flo;
		LightDiffuse1[2]+=flo;
		glLightfv(GL_LIGHT0, GL_AMBIENT, lightambient0);				// Setup The Ambient Light

		glLightfv(GL_LIGHT0, GL_DIFFUSE, LightDiffuse0);				// Setup The Ambient Light
		glLightfv(GL_LIGHT1, GL_DIFFUSE, LightDiffuse1);				// Setup The Ambient Light
	}
	if (keyDown['B'])		
	{

		flo=+0.05f;

		lightambient0[0]-=flo;
		lightambient0[1]-=flo;
		lightambient0[2]-=flo;
		LightDiffuse0[0]-=flo;
		LightDiffuse0[1]-=flo;
		LightDiffuse0[2]-=flo;
		LightDiffuse1[0]-=flo;
		LightDiffuse1[1]-=flo;
		LightDiffuse1[2]-=flo;
		glLightfv(GL_LIGHT0, GL_AMBIENT, lightambient0);				// Setup The Ambient Light
		glLightfv(GL_LIGHT0, GL_DIFFUSE, LightDiffuse0);				// Setup The Ambient Light
		glLightfv(GL_LIGHT1, GL_DIFFUSE, LightDiffuse1);				// Setup The Ambient Light
	}



}


void CMain::LoadImage(char *name, char **dest,int *height,int *width)
{

	bitmap= new CImage();
	bitmap->Load(name);
	if (!bitmap->IsDIBSection()) MessageBox(theApp.m_wndMain.m_hWnd,"no!!","",2);

	///////////// convert jpg in dib
	//get some data we need...
	int pitch=bitmap->GetPitch()*-1;
	*height=bitmap->GetHeight();
	*width=bitmap->GetWidth();
	sBGR* pPixel;
	char* pBitmap=(char*)bitmap->GetBits()-pitch*(*height-1);
	*dest=pBitmap;
}



void CMain::LoadEnvRPC(char **dest,int *height,int *width)
{
	int Connected=0;
	double punto_new[3];
	char flag=0,draw;
	if (Connected) 
	{
		Connected=0;
		RPC_CloseConn();
	} else {
		if(RPC_OpenConn("localhost"))
			Connected=1;
		else 
		{
			AfxMessageBox("Can't connect to the robot, check server status");
		}
	}
	if (Connected)
		RPC_Read_Cam(&WCData,1);
	if (Connected)
	{
		Connected=0;
		RPC_CloseConn();
	}


	//*dest=// mettere qui il puntatore all'area di memoria non compressa o usare la funz di sopra per estrarre da una CIMAge sto puntatore

}

void CMain::SelectTexture()
{
	switch (TextureEnable)
	{
	case 0 :
		glLightfv(GL_LIGHT0, GL_DIFFUSE, lightnull);				// Setup The Ambient Light
		glLightfv(GL_LIGHT1, GL_DIFFUSE, lightnull);			
		break;
	case 1:
		glLightfv(GL_LIGHT0, GL_DIFFUSE, LightDiffuse0);				// Setup The Ambient Light
		glLightfv(GL_LIGHT1, GL_DIFFUSE, LightDiffuse1);				// Setup The Ambient Light
		break;
	case 4:
		glLightfv(GL_LIGHT0, GL_DIFFUSE, LightDiffuse0);				// Setup The Ambient Light
		glLightfv(GL_LIGHT1, GL_DIFFUSE, lightnull);				// Setup The Ambient Light
		break;
	case 2:
		glLightfv(GL_LIGHT0, GL_DIFFUSE, LightDiffuse0);				// Setup The Ambient Light
		glLightfv(GL_LIGHT1, GL_DIFFUSE, LightDiffuse1);				// Setup The Ambient Light
		break;
	default:;

	}
}
UINT i,nFrames=200;
CAVIGenerator AviGen;
LPBITMAPINFOHEADER lpbih;
BYTE* bmBits;	
HRESULT hr;



void AVIStart()
{
	//	CGL2AviView* pView=(CGL2AviView*)GetActiveView();


	// set 15fps
	AviGen.SetRate(15);

	// give info about bitmap
	AviGen.SetBitmapHeader(&(theApp.m_wndMain));		

	// set filename, extension ".avi" is appended if necessary
	AviGen.SetFileName(_T("test.avo"));

	// retreiving size of image
	lpbih=AviGen.GetBitmapHeader();

	// allocating memory
	bmBits=new BYTE[lpbih->biSizeImage];

	hr=AviGen.InitEngine();

	if (FAILED(hr))
	{
		AfxMessageBox( AviGen.GetLastErrorMessage());
		AVIFinish();
	}

	nidavi=SetTimer(theApp.m_wndMain.m_hWnd,theApp.m_appMain.timer1,125,OnAvigenerationGenerate);
	LARGE_INTEGER li;
	QueryPerformanceCounter(&li);

}

void AVIFinish()
{
	KillTimer(theApp.m_wndMain.m_hWnd,nidavi);
	AviGen.ReleaseEngine();
	delete[] bmBits;
	// releasing engine and memory
	//	glReadBuffer(GL_FRONT); //+
}

VOID CALLBACK OnAvigenerationGenerate(HWND hwnd,UINT uMsg,UINT_PTR idEvent,DWORD dwTime) 
{
	// reading back buffer
	//glReadBuffer(GL_BACK);
	////for(i=0;i<nFrames;i++)
	////{
	// render frame
	//	Draw(true); //+
	// Copy from OpenGL to buffer
	glReadPixels(0,0,lpbih->biWidth,lpbih->biHeight,GL_BGR_EXT,GL_UNSIGNED_BYTE,bmBits); 
	// send to avi engine
	hr=AviGen.AddFrame(bmBits);
	if (FAILED(hr))
	{
		AfxMessageBox( AviGen.GetLastErrorMessage());
		AVIFinish();
	}
	//	}

}



void ExperimentSet1() // exp1
{
	short int *bulkarray;
	srand(time(NULL)); /// RICORDATELO!!!!
	RandRep(&bulkarray,1,TRIALNUMBER,100);
	short int *SPNumArray;
	srand(time(NULL)); 
	RandRep(&SPNumArray,1,3*10,100); // purtroppo lo spazio delle combinazioni coincide con gli SP al momento, questo prende i primi 10 dei 30 disponibili per rimappare le posizioni di partenza.
	for (int i=0;i<TRIALNUMBER;i++)
	{
		prova[i].TextureEnable=bulkarray[i] % 3;
		prova[i].numberposition=SPNumArray[bulkarray[i] %10];
	}
	FILE *fp;

	fp=fopen("controllo.txt" ,"w");
	for (int i=0;i<TRIALNUMBER;i++)
		fprintf(fp,"%d %d %f\n",prova[i].numberposition,prova[i].TextureEnable,prova[i].time);
	fclose(fp);

}


void ExperimentSet2()
{
	FILE *fp;
	fp=fopen("Data/mappe.txt" ,"r");
	for (int i=0;i<NMAP;i++)
		fscanf(fp,"%s\n",nomemappe[i]);
	fclose(fp);
	short int *bulkarray;
	srand(time(NULL)); /// RICORDATELO!!!!
	RandRep(&bulkarray,1,TRIALNUMBER,100);

	short int *MapNumArray;
	srand(time(NULL)); 
	RandRep(&MapNumArray,0,NMAP-1,100);

	for (int i=0;i<TRIALNUMBER;i++)
	{
		prova[i].TextureEnable=bulkarray[i] % 3;
		prova[i].numberposition=1+(bulkarray[i] % 2);
		prova[i].nmap=MapNumArray[i];
	}


	fp=fopen("controllo.txt" ,"w");
	for (int i=0;i<TRIALNUMBER;i++)
		fprintf(fp,"%d %d %d\n",prova[i].numberposition,prova[i].TextureEnable,prova[i].nmap);
	fclose(fp);


}

UINT TakeScreenshot(LPVOID param)
{
	int _width = theApp.m_Width;//1024;
	int _height = theApp.m_Height;//768
	char image_path[40];
	logger_data_t * data = (logger_data_t *) param;
	
	corona::Image * myImage = corona::CreateImage(_width, _height,
												  corona::PF_R8G8B8A8,
												  data->image_data);

	corona::FlipImage(myImage, corona::CA_X); //Flip the image
	sprintf(image_path, "trace/screenshot_%d_%d.png", data->record, data->time);
	corona::SaveImage(image_path, corona::FF_PNG, myImage); //Save the image

	delete myImage;
	free(data);

    return 0;   // thread completed successfully
}


/* function to save screenshot and robot data */
void CMain::SaveStatus() {

	int _width = theApp.m_Width;//1024;
	int _height = theApp.m_Height;//768
	int our_time = (int) robot1.Time;
	logger_data_t * data;
	
	//if ( (our_time % 5) == 0 && FpData!= NULL && our_time != LastScreenshotTime ) {

	if ( FpData!= NULL ) {

			fprintf(FpData,"%f %f %f %f\n",
			robot1.xr,robot1.yr,robot1.tetar,
			robot1.Time);

			/* save last time */
			LastScreenshotTime = our_time;

			data = (logger_data_t *) malloc(sizeof(logger_data_t));

			/* gets the pixel from the buffer */
			glReadBuffer(GL_BACK);
			/* allocate space for the pixel to be saved */
			data->image_data = malloc(_width * _height * (32)); //Allocate memory for storing the image
			glReadPixels(0, 0, _width, _height, GL_RGBA, GL_UNSIGNED_BYTE, data->image_data); //Copy

			data->time = our_time;
			data->record = SimNumber;
			/* launch a new thread whose target is to save the taken screenshot   */
			AfxBeginThread(TakeScreenshot, (void *) data);
		}
}