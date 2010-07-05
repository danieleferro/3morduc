#include <iostream>
#include <stdlib.h>
#include <corona.h>

using namespace std;
using namespace corona;


int main ()
{
  Image * image = 0;
  image = OpenImage("screenshot_8_103.bmp");

  cout << "Success!" << endl;
  cout << image->getHeight() << endl;
  cout << image->getWidth() << endl;
  cout << *((char *)image->getPixels());
  


  return 0;
}
