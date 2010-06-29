#define PI 3.14159265358979
#define pig 3.14159265358979
#define DTOR            0.0174532925


typedef struct {
	GLfloat x,y,z;
} XYZ;



typedef struct {
   double rotatespeed;       /* How fast the pulsar rotates             */
	int spinspeed;           /* How fast the user mouse moves           */
	int record,windowdump;   /* Image recording modes                   */
	int fullscreen;          /* Game mode or not                        */
} OPTIONS;

typedef struct											// Structure For 3D Points
{
	GLfloat	x, y, z,value,touched;						// X, Y & Z Points
} VERTEX;												// Called VERTEX

typedef struct											// Structure For An Object
{
 int		verts;										// Number Of Vertices For The Object
 int mx,Mx,my,My;										// boundary
 VERTEX		*points;									// One Vertice (Vertex x,y & z)
} OBJECT;												// Called OBJECT

typedef struct {
	int h,v;
} iPOINT;




typedef struct ssBGR {
unsigned char b;
unsigned char g;
unsigned char r;
// unsigned char pad;
} sBGR;

typedef sBGR *pBGR;