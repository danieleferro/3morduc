/*
 * DataLogicLogSimulator.h    
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

#ifndef __DATA_LOGIC_MORDUC
#define __DATA_LOGIC_MORDUC

#include <string>
#include <vector>
#include <stdlib.h>
#include <sstream>
#include <math.h>
#include <iostream>
#include <cstdio>
#include <cstring>
#include <stdlib.h>

#include "IDataLogic.h"
#include "IImageSelector.h"
#include "../jpeg-8c/jpeglib.h"
#include "HTTPFunctor.h"
#include "command_mapping.h"

#define TO_DEGREES(X) X * 180 / M_PI
#define MAGNITUDE 100
#define __DATA_LOGIC_MORDUC__DBG 1

class DataLogicMorduc : public IDataLogic
{
  

 private:

  
  std::vector<image_data> _images_collection;
  int _index;
  
  FILE * odom_file;
  FILE * img_file;

  std::string _path_server;
  std::string _path_data;

  HTTPFunctor _http_functor;
  
 public:
  DataLogicMorduc(const char*, const char*);
  ~DataLogicMorduc();
  void SelectImage(robot_data *, image_data *,
		   IImageSelector *);

  void RetrieveData(robot_data *);

  void Command(int);





};

#endif
