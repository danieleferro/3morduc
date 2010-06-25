#ifndef __DATA_LOGIC_INTERFACE
#define __DATA_LOGIC_INTERFACE

#include "DistanceCalcInterface.h"
#include "data_types.h"

class DataLogicInterface {

 public:
  virtual void SelectImage(robot_data *, image_data *,
			   DistanceCalcInterface *) = 0;

  virtual void RetrieveData(robot_data *) = 0;
  
};

#endif
