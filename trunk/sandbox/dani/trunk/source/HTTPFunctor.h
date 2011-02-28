/*
 * HTTPFunctor.h    
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

#ifndef __HTTP_FUNCTOR__
#define __HTTP_FUNCTOR__

#include <string>
#include <vector>
#include <stdlib.h>
#include <iostream>
#include <cstdio>
#include <cstring>

#include <jpeglib.h>
#include <jerror.h>
#include "jinclude.h"
#include "jpeg_library.h"

#define MAX_FILE_LENGTH 20000


class HTTPFunctor
{
 private:

  // to store jpeg image from HTTP server
  char* _m_pBuffer;

  // store header
  std::string _header;

  // store decompressed jpeg image
  unsigned char * _raw_image;

  size_t _m_Size;

  void* Realloc(void*, size_t);
  int ReadHalfJPEGBuffer();
  int WriteJPEGFile(const char*);

 public:
  HTTPFunctor();
  ~HTTPFunctor();

  std::string GetOdometryString();
  void ResetFunctor();
  void CreateImage(const char*);

  // callback
  size_t WriteHTTPBodyCallback(char*, size_t, size_t);
  size_t WriteHTTPHeaderCallback(char*, size_t, size_t);


};


#endif
