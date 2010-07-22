/*
 * IDataLogic.h
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

#ifndef __DATA_LOGIC_INTERFACE
#define __DATA_LOGIC_INTERFACE

#include "IImageSelector.h"
#include "data_types.h"

class IDataLogic {

 public:
  virtual void SelectImage(robot_data *, image_data *,
			   IImageSelector *) = 0;

  virtual void RetrieveData(robot_data *) = 0;

  virtual void Command(int) = 0;
  
};

#endif
