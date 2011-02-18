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

#ifndef __DATA_LOGIC_LOG_SIMULATOR
#define __DATA_LOGIC_LOG_SIMULATOR

#include <string>
#include <vector>
#include <stdlib.h>
#include <sstream>
#include <math.h>
#include <iostream>
#include <cstdio>
#include <cstring>
#include "IDataLogic.h"
#include "IImageSelector.h"

#define TO_DEGREES(X) X * 180 / M_PI

class DataLogicLogSimulator : public IDataLogic
{
 private:
  std::vector<image_data> _images_collection;
  int _index;
  int _simulation_session;
  
 public:
  DataLogicLogSimulator(int);
  ~DataLogicLogSimulator();
  void SelectImage(robot_data *, image_data *,
		   IImageSelector *);

  void RetrieveData(robot_data *);

  void Command(int);

};

#endif
