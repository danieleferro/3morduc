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
