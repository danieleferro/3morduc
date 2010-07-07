#ifndef __DATA_TYPES
#define __DATA_TYPES

struct image_data {
  float x;
  float y;
  float theta;
  float time;
  char path[100];
};

struct robot_data {
  float x;
  float y;
  float theta;
  float time;
};

#endif
