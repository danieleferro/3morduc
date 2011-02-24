/*
 * opengl_library.h    
 *
 * This file is part of REAR.
 * Copyright (C) 2010 Daniele Ferro (daniele.ferro86@gmail.com) 
 *                    Loris Fichera (loris.fichera@gmail.com)
 *
 * REAR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * REAR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with REAR.  If not, see <http://www.gnu.org/licenses/>.
 */


#ifndef __OPENGL
#define __OPENGL

#include <GL/glut.h>    // Header File For The GLUT Library 
#include <GL/gl.h>	// Header File For The OpenGL32 Library
#include <GL/glu.h>	// Header File For The GLu32 Library

#endif

#ifndef __OPENGL_LIBRARY__
#define __OPENGL_LIBRARY__




void DrawText(GLint x, GLint y, char* s,
	      GLfloat r, GLfloat g, GLfloat b,
	      void* font);

void DrawTexture();

void setMaterial (GLfloat ambientR, GLfloat ambientG, GLfloat ambientB, 
		  GLfloat diffuseR, GLfloat diffuseG, GLfloat diffuseB, 
		  GLfloat specularR, GLfloat specularG, GLfloat specularB,
		  GLfloat shininess);



#endif
