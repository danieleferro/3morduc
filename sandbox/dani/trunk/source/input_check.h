/*
 * input_check.h    
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


#ifndef __INPUT_CHECK__
#define __INPUT_CHECK__

#include <vector>
#include <string>
#include "IDataLogic.h"
#include "IImageSelector.h"
#include "Robot.h"
#include <stdlib.h>
#include "Morduc.h"
#include "DataLogicLogSimulator.h"
#include "DataLogicLogMorduc.h"
#include "DataLogicMorduc.h"
#include "SweepMetricCalc.h"
#include "SpacialMetricCalc.h"
#include "AnotherSweepMetricCalc.h"

// extern varirable defined in main.cpp
extern IDataLogic * logic;
extern IImageSelector* calculator;
extern Robot * rob;

void inputCheck(int, char**); 



#endif
