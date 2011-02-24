/*
 * input_check.cpp
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

#include "input_check.h"


// --------------------------------------------------------------------
//                       INPUT PARAMETER CHECKING
// --------------------------------------------------------------------


void printHelp() {

    printf("Usage: REAR -dl data_logic dl_opt1 [dl_opt2] [-is image_select is_dist] [-r radius] \n");
    printf("All values are case sensitive.\n\n");

    printf("Possible values for -dl:\n");

    printf("-dl morduc <address> [<save_path>]\n");
    printf("connect to real morduc and save data (default path is ../log_morduc/log_online/)\n\n");

    printf("-dl logmorduc <log_number>\n");
    printf("use log from real morduc session\n\n");

    printf("-dl logsimul <log_number>\n");
    printf("use log from simulated morduc session\n\n");

    printf("Possible values for -is:\n");

    printf("-dl sweep <opt_dist>\n");
    printf("use sweep metric algorithm\n\n");
 
    printf("-dl asweep <opt_dist>\n");
    printf("use another sweep metric algorithm\n\n");
 
    printf("-dl spacial <opt_dist>\n");
    printf("use spacial metric algorithm\n\n");

    printf("Possible values for -r:\n");

    printf("-r <radius>\n");
    printf("draw robot with specified radius (default is 5)\n");
 
    
    printf("\n");
    exit(0);

}

void printUsage() {

  printf("Usage: REAR -dl data_logic dl_opt1 [dl_opt2] [-is image_select is_dist] [-r radius] \n");
  printf("Help:  REAR -h \n");

  printf("\n");
  exit(0);

}

void inputDataLogic(std::vector<std::string>* opts) {
  
  // printf("inputDataLogic\n");

  if (opts->size() > 2) {

    // delete -dl element
    opts->erase(opts->begin());

    // check field data_logic

    // MORDUC case
    if (!(*opts)[0].compare("morduc")) {

      // delete this element
      opts->erase(opts->begin());

//       for(int j=0; j<opts->size(); j++)
// 	std::cout << "cicle: " << (*opts)[j] << std::endl;


      // next element is morduc address
      // next element (if present) path online
      if ((opts->size() > 1 && (*opts)[1][0] == '-') ||
	  opts->size() == 1) {
	// chose default path
	logic = new DataLogicMorduc((*opts)[0].c_str(), "../log_morduc/log_online");
	opts->erase(opts->begin());
	
      }
      else {
	// use specified path
	logic = new DataLogicMorduc((*opts)[0].c_str(), (*opts)[1].c_str());
	opts->erase(opts->begin());
	opts->erase(opts->begin());
      }
      return;
    }


    // LOGMORDUC case
    if (!(*opts)[0].compare("logmorduc")) {

      // delete this element
      opts->erase(opts->begin());

      // next element is log number
      if (!atoi((*opts)[0].c_str()))
	// conversion failed
	printUsage();

      // next element is log number
      logic = new DataLogicLogMorduc(atoi((*opts)[0].c_str()));
      opts->erase(opts->begin());
      return;
    }

    // LOGSIMULATOR case
    if (!(*opts)[0].compare("logsimul")) {

      // delete this element
      opts->erase(opts->begin());

      // next element is log number
      if (!atoi((*opts)[0].c_str()))
	// conversion failed
	printUsage();

      logic = new DataLogicLogSimulator(atoi((*opts)[0].c_str()));

      opts->erase(opts->begin());
      return;
    }

    // no previous option
    printUsage();
    
  }
  else
    printUsage();

}

void inputImageSelector(std::vector<std::string>* opts) {

  // printf("inputImageSelector\n");

  if (opts->size() > 2) {

    // delete -is element
    opts->erase(opts->begin());

    // check field image_selector

    // SWEEP case
    if (!(*opts)[0].compare("sweep")) {

      // delete this element
      opts->erase(opts->begin());

//       for(int j=0; j<opts->size(); j++)
// 	std::cout << "cicle: " << (*opts)[j] << std::endl;

      // next element is optimal distance
      if (!atof((*opts)[0].c_str()))
	// conversion failed
	printUsage();

      calculator = new SweepMetricCalc(45, 30, atof((*opts)[0].c_str()), 5, 0, 5);
      opts->erase(opts->begin());
      
      return;
    }


    // ASWEEP case
    if (!(*opts)[0].compare("asweep")) {

      // delete this element
      opts->erase(opts->begin());

      // next element is optimal distance
      if (!atof((*opts)[0].c_str()))
	// conversion failed
	printUsage();

      calculator = new AnotherSweepMetricCalc(45, 30, atof((*opts)[0].c_str()), 5, 0, 5);
      opts->erase(opts->begin());
      
      return;
    }

    // SPACIAL case
    if (!(*opts)[0].compare("spacial")) {

      // delete this element
      opts->erase(opts->begin());

      // next element is optimal distance
      if (!atof((*opts)[0].c_str()))
	// conversion failed
	printUsage();

      calculator = new SpacialMetricCalc(atof((*opts)[0].c_str()));
      opts->erase(opts->begin());
      
      return;
    }

    // no previous option
    printUsage();    
    
  }
  else
    printUsage();

}

void inputRobot(std::vector<std::string>* opts) {

  // printf("inputRobot\n");

  if (opts->size() > 1) {

    // delete -r element
    opts->erase(opts->begin());

    // next element is radius
    if (!atof((*opts)[0].c_str()))
      // conversion failed
      printUsage();

    rob = new Morduc(atof((*opts)[0].c_str()));
    opts->erase(opts->begin());
      
    return;
  }
  else
    printUsage();

}

void instanceDefault() {

  if (calculator == NULL)
    // default image selector (distance 25)
    calculator = new AnotherSweepMetricCalc(45, 30, 25, 5, 0, 5);

  if (rob == NULL)
    // default robot
    rob = new Morduc();

    
}

void inputCheck(int argc, char * argv[]) {


  if (argc < 2) {
    printUsage();
  }
  
  // store all options in a string vector
  std::vector<std::string> opts;
  for (int i=1; i < argc; i++) {
 
    opts.push_back(argv[i]);
  }
  

  if (!opts[0].compare("-h")) {

    printHelp();
  }



  if (!opts[0].compare("-dl")) {

    // first element is -dl or -DL
    inputDataLogic(&opts);

    // check if other parameters have to be read
    if (opts.size() > 0) {

      if (!opts[0].compare("-is")) {
	
	inputImageSelector(&opts);

	if (opts.size() > 0) {

	  if (!opts[0].compare("-r")) {
	    
	    inputRobot(&opts);
	  }
	}

	instanceDefault();
	return;
      }

      if (!opts[0].compare("-r")) {
	
	inputRobot(&opts);

	if (opts.size() > 0) {

	  if (!opts[0].compare("-is")) {
	    
	    inputImageSelector(&opts);
	  }
	}

	instanceDefault();
	return;
	
      }
      
    }
    
    instanceDefault();
    return;
  
  }
  else {
    
    // first option must be -dl
    printUsage();

  }
  
}
