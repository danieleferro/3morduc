/*
 * texture_png.h    
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

#ifndef __PNG_LOADER_OPENGL__
#define __PNG_LOADER_OPENGL__

#include <cstdio>
#include <cstdlib>
#include <png.h>
#include <GL/gl.h>
#include <GL/glu.h>

int GetTextureInfo(int ColourType);
GLuint loadPNGImage(const char *filename);

#endif

