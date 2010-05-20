#include "stdafx.h"

#include "sky.h"


Sky::Sky()
{
	movement =0.0f;
	

	LoadTGA(&this->textures[0],"sky/Sky.tga");
	quadratic=gluNewQuadric();
	gluQuadricNormals(quadratic, GLU_SMOOTH);			// Create Smooth Normals (NEW)
	gluQuadricTexture(quadratic, GL_TRUE);				// Create Texture Coords (NEW)
}

bool Sky::LoadTGA(TextureImage *texture, char *filename)				// Loads A TGA File Into Memory
{    
	GLubyte		TGAheader[12]={0,0,2,0,0,0,0,0,0,0,0,0};		// Uncompressed TGA Header
	GLubyte		TGAcompare[12];									// Used To Compare TGA Header
	GLubyte		header[6];										// First 6 Useful Bytes From The Header
	GLuint		bytesPerPixel;									// Holds Number Of Bytes Per Pixel Used In The TGA File
	GLuint		imageSize;										// Used To Store The Image Size When Setting Aside Ram
	GLuint		temp;											// Temporary Variable
	GLuint		type=GL_RGBA;									// Set The Default GL Mode To RBGA (32 BPP)

	FILE *file = fopen(filename, "rb");							// Open The TGA File

	if(	file==NULL ||											// Does File Even Exist?
		fread(TGAcompare,1,sizeof(TGAcompare),file)!=sizeof(TGAcompare) ||	// Are There 12 Bytes To Read?
		memcmp(TGAheader,TGAcompare,sizeof(TGAheader))!=0				||	// Does The Header Match What We Want?
		fread(header,1,sizeof(header),file)!=sizeof(header))				// If So Read Next 6 Header Bytes
	{
		if (file == NULL)										// Did The File Even Exist? *Added Jim Strong*
			return FALSE;										// Return False
		else													// Otherwise
		{
			fclose(file);										// If Anything Failed, Close The File
			return FALSE;										// Return False
		}
	}
	texture->width  = header[1] * 256 + header[0];				// Determine The TGA Width	(highbyte*256+lowbyte)
	texture->height = header[3] * 256 + header[2];				// Determine The TGA Height	(highbyte*256+lowbyte)
    
 	if(	texture->width	<=0	||									// Is The Width Less Than Or Equal To Zero
		texture->height	<=0	||									// Is The Height Less Than Or Equal To Zero
		(header[4]!=24 && header[4]!=32))						// Is The TGA 24 or 32 Bit?
	{
		fclose(file);											// If Anything Failed, Close The File
		return FALSE;											// Return False
	}

	texture->bpp	= header[4];								// Grab The TGA's Bits Per Pixel (24 or 32)
	bytesPerPixel	= texture->bpp/8;							// Divide By 8 To Get The Bytes Per Pixel
	imageSize		= texture->width*texture->height*bytesPerPixel;	// Calculate The Memory Required For The TGA Data

	texture->imageData=(GLubyte *)malloc(imageSize);			// Reserve Memory To Hold The TGA Data

	if(	texture->imageData==NULL ||								// Does The Storage Memory Exist?
		fread(texture->imageData, 1, imageSize, file)!=imageSize)	// Does The Image Size Match The Memory Reserved?
	{
		if(texture->imageData!=NULL)							// Was Image Data Loaded
			free(texture->imageData);							// If So, Release The Image Data

		fclose(file);											// Close The File
		return FALSE;											// Return False
	}

	for(GLuint i=0; i<int(imageSize); i+=bytesPerPixel)			// Loop Through The Image Data
	{															// Swaps The 1st And 3rd Bytes ('R'ed and 'B'lue)
		temp=texture->imageData[i];								// Temporarily Store The Value At Image Data 'i'
		texture->imageData[i] = texture->imageData[i + 2];		// Set The 1st Byte To The Value Of The 3rd Byte
		texture->imageData[i + 2] = temp;						// Set The 3rd Byte To The Value In 'temp' (1st Byte Value)
	}

	fclose (file);												// Close The File

	// Build A Texture From The Data
	glGenTextures(1, &texture[0].texID);						// Generate OpenGL texture IDs

	glBindTexture(GL_TEXTURE_2D, texture[0].texID);				// Bind Our Texture
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);	// Linear Filtered
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);	// Linear Filtered
	
	if (texture[0].bpp==24)										// Was The TGA 24 Bits
	{
		type=GL_RGB;											// If So Set The 'type' To GL_RGB
	}

	glTexImage2D(GL_TEXTURE_2D, 0, type, texture[0].width, texture[0].height, 0, type, GL_UNSIGNED_BYTE, texture[0].imageData);

	return true;												// Texture Building Went Ok, Return True
}



void Sky::Draw(void)													
{
	float xrot = 90.0f;
	float yrot = this->movement;//this->movement;
	glColor3f(1.0f,1.0f,1.0f);
	//glLoadIdentity();											// Reset The Modelview Matrix
	glEnable(GL_BLEND);	
	glEnable(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, this->textures[0].texID);			// Select The Sky Texture
	gluSphere(quadratic,50.0f,32,32);															
	/*glRotatef(yrot,0.0f,1.0f,0.0f);
	glRotatef(xrot,1.0f,0.0f,0.0f);
	
	glRotatef(-xrot,1.0f,0.0f,0.0f);
	glRotatef(-yrot ,0.0f,1.0f,0.0f);

	yrot = this->movement * 0.01f;
	glRotatef(yrot,0.0f,1.0f,0.0f);
	glRotatef(-xrot,1.0f,0.0f,0.0f);
	gluSphere(quadratic,49.0f,32,32);															
	glRotatef(xrot,1.0f,0.0f,0.0f);
	glRotatef(-yrot ,0.0f,1.0f,0.0f);
*/
	glDisable(GL_BLEND);	

	//this->movement +=0.07f;
	//if(movement >= 360.0f*10) movement =0.0f;
	/*
	glBegin(GL_QUADS);											// Begin Drawing Quads
		float roll = 0;
		float a = 50.0f;
		float height = 50.0f;
		float low = -30.0f;
		
		glTexCoord2f(1.0f,roll/1.5f+1.0f); glVertex3f(  a,+height,a);	// Top Right
		glTexCoord2f(0.0f,roll/1.5f+1.0f); glVertex3f( -a,+height, a);	// Top Left
		glTexCoord2f(0.0f,roll/1.5f+0.0f); glVertex3f( -a,+height, -a);	// Bottom Left
		glTexCoord2f(1.0f,roll/1.5f+0.0f); glVertex3f(  a,+height,-a);	// Bottom Right

		glTexCoord2f(1.5f,roll+1.0f); glVertex3f(  a,+height, a);		// Top Right
		glTexCoord2f(0.5f,roll+1.0f); glVertex3f( -a,+height, a);		// Top Left
		glTexCoord2f(0.5f,roll+0.0f); glVertex3f( -a,+height,-a);		// Bottom Left
		glTexCoord2f(1.5f,roll+0.0f); glVertex3f(  a,+height,-a);		// Bottom Right
		/////////
		glTexCoord2f(1.0f,roll/1.5f+1.0f); glVertex3f( -a,+height, a);	// Top Right
		glTexCoord2f(0.0f,roll/1.5f+1.0f); glVertex3f( -a,+height, -a);	// Top Left
		glTexCoord2f(0.0f,roll/1.5f+0.0f); glVertex3f( -a,low, -a);	// Bottom Left
		glTexCoord2f(1.0f,roll/1.5f+0.0f); glVertex3f( -a,low, a);	// Bottom Right

		glTexCoord2f(1.5f,roll+1.0f); glVertex3f(-a,+height,a);		// Top Right
		glTexCoord2f(0.5f,roll+1.0f); glVertex3f(-a,+height,-a);		// Top Left
		glTexCoord2f(0.5f,roll+0.0f); glVertex3f(-a,low,-a);		// Bottom Left
		glTexCoord2f(1.5f,roll+0.0f); glVertex3f(-a,low,a);		// Bottom Right
		
		glTexCoord2f(1.0f,roll/1.5f+1.0f); glVertex3f( -a,+height, -a);	// Top Right
		glTexCoord2f(0.0f,roll/1.5f+1.0f); glVertex3f(  a,+height, -a);	// Top Left
		glTexCoord2f(0.0f,roll/1.5f+0.0f); glVertex3f(  a,low, -a);	// Bottom Left
		glTexCoord2f(1.0f,roll/1.5f+0.0f); glVertex3f( -a,low,-a);	// Bottom Right

		glTexCoord2f(1.5f,roll+1.0f); glVertex3f( -a,+height,-a);		// Top Right
		glTexCoord2f(0.5f,roll+1.0f); glVertex3f( a,+height,-a);		// Top Left
		glTexCoord2f(0.5f,roll+0.0f); glVertex3f( a,low,-a);		// Bottom Left
		glTexCoord2f(1.5f,roll+0.0f); glVertex3f( -a,low,-a);		// Bottom Right
		//
		glTexCoord2f(1.0f,roll/1.5f+1.0f); glVertex3f(  a,+height, a);	// Top Right
		glTexCoord2f(0.0f,roll/1.5f+1.0f); glVertex3f(  a,+height, -a);	// Top Left
		glTexCoord2f(0.0f,roll/1.5f+0.0f); glVertex3f(  a,low, -a);	// Bottom Left
		glTexCoord2f(1.0f,roll/1.5f+0.0f); glVertex3f(  a,low, a);	// Bottom Right

		glTexCoord2f(1.5f,roll+1.0f); glVertex3f( a,+height, a);		// Top Right
		glTexCoord2f(0.5f,roll+1.0f); glVertex3f( a,+height,-a);		// Top Left
		glTexCoord2f(0.5f,roll+0.0f); glVertex3f( a,low,-a);		// Bottom Left
		glTexCoord2f(1.5f,roll+0.0f); glVertex3f( a,low, a);		// Bottom Right

		glTexCoord2f(1.0f,roll/1.5f+1.0f); glVertex3f(  -a,+height, a);	// Top Right
		glTexCoord2f(0.0f,roll/1.5f+1.0f); glVertex3f(  a,+height, a);	// Top Left
		glTexCoord2f(0.0f,roll/1.5f+0.0f); glVertex3f(  a,low, a);	// Bottom Left
		glTexCoord2f(1.0f,roll/1.5f+0.0f); glVertex3f(  -a,low, a);	// Bottom Right

		glTexCoord2f(1.5f,roll+1.0f); glVertex3f( -a,+height, a);		// Top Right
		glTexCoord2f(0.5f,roll+1.0f); glVertex3f( a,+height, a);		// Top Left
		glTexCoord2f(0.5f,roll+0.0f); glVertex3f( a,low, a);		// Bottom Left
		glTexCoord2f(1.5f,roll+0.0f); glVertex3f( -a,low, a);		// Bottom Right


	glEnd();*/ 
	glDisable(GL_TEXTURE_2D);
}