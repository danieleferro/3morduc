#include "stdafx.h"
#include "font.h"

GLuint	base;													// Base Display List For The Font Set
GLYPHMETRICSFLOAT gmf[256];										// Storage For Information About Our Outline Font Characters

GLvoid BuildFont(HDC hDC)								// Build Our Bitmap Font
{
	HFONT	font;												// Windows Font ID

	base = glGenLists(256);										// Storage For 256 Characters

	font = CreateFont(	2,									// Height Of Font
						0,										// Width Of Font
						0,										// Angle Of Escapement
						0,										// Orientation Angle
						FW_BOLD,								// Font Weight
						FALSE,									// Italic
						FALSE,									// Underline
						FALSE,									// Strikeout
						ANSI_CHARSET,							// Character Set Identifier
						OUT_TT_PRECIS,							// Output Precision
						CLIP_DEFAULT_PRECIS,					// Clipping Precision
						ANTIALIASED_QUALITY,					// Output Quality
						FF_DONTCARE|DEFAULT_PITCH,				// Family And Pitch
						NULL);									// Font Name
	

	SelectObject(hDC, font);									// Selects The Font We Created

	wglUseFontOutlines(	hDC,									// Select The Current DC
						0,										// Starting Character
						255,									// Number Of Display Lists To Build
						base,									// Starting Display Lists
						0.0f,									// Deviation From The True Outlines
						0.0f,									// Font Thickness In The Z Direction
						WGL_FONT_POLYGONS,						// Use Polygons, Not Lines
						gmf);									// Address Of Buffer To Recieve Data
}

GLvoid KillFont(GLvoid)											// Delete The Font
{
	glDeleteLists(base, 256);									// Delete All 256 Characters
}

/*GLvoid glPrint(float x, float y, float z, const char *fmt, ...)	// Custom GL "Print" Routine
{
	float		length=0;										// Used To Find The Length Of The Text
	char		text[256];										// Holds Our String
	va_list		ap;												// Pointer To List Of Arguments

	if (fmt == NULL)											// If There's No Text
		return;													// Do Nothing

	va_start(ap, fmt);											// Parses The String For Variables
	    vsprintf(text, fmt, ap);								// And Converts Symbols To Actual Numbers
	va_end(ap);													// Results Are Stored In Text

	for (unsigned int loop=0;loop<(strlen(text));loop++)		// Loop To Find Text Length
	{
		length+=gmf[text[loop]].gmfCellIncX;					// Increase Length By Each Characters Width
	}
}

*/



GLint glPrint(GLint x, GLint y, int set, const char *fmt, ...)	// Where The Printing Happens
{
	char		text[1024];									// Holds Our String
	va_list		ap;											// Pointer To List Of Arguments

	if (fmt == NULL)										// If There's No Text
		return -1;												// Do Nothing

	va_start(ap, fmt);										// Parses The String For Variables
	    vsprintf(text, fmt, ap);							// And Converts Symbols To Actual Numbers
	va_end(ap);												// Results Are Stored In Text

	if (set>1)												// Did User Choose An Invalid Character Set?
	{
		set=1;												// If So, Select Set 1 (Italic)
	}

	//glEnable(GL_TEXTURE_2D);								// Enable Texture Mapping
	glLoadIdentity();										// Reset The Modelview Matrix
	glTranslated(x,y,0);									// Position The Text (0,0 - Top Left)
	glListBase(base);							// Choose The Font Set (0 or 1)

	//glScalef(1.0f,1.0f,1.0f);								// Make The Text 2X Taller

	glCallLists(strlen(text),GL_UNSIGNED_BYTE, text);		// Write The Text To The Screen
	glDisable(GL_TEXTURE_2D);								// Disable Texture Mapping
	return strlen(text);
}



