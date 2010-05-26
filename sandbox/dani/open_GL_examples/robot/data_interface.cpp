#include "data_interface.h"

template<class T>
bool from_string(T& t, 
                 const std::string& s, 
                 std::ios_base& (*f)(std::ios_base&))
{
  std::istringstream iss(s);
  return !(iss >> f >> t).fail();
}

void GetNewPosition(int line_number, GLfloat * position)
{
  FILE * position_data = fopen("data.txt","rt");
  char line[50];
  std::string * line_read;
  std::string line_values[4];
  
  while(fgets(line, 50, position_data) &&
	line_number > 1)
    {
      line_number--;
    }

  line_read = new std::string(line);

  std::string buf;
  std::stringstream ss(*line_read);
  std::vector<std::string> tokens; // Create vector to hold our words
  
  while (ss >> buf)
    tokens.push_back(buf);

//   std::vector<std::string>::const_iterator cii;
//   for(cii=tokens.begin(); cii!=tokens.end(); cii++)
  for(int i = 0; i < tokens.size(); i++)
    {
      //      std::cout << *cii << std::endl;
      from_string<float>(position[i], 
			 tokens[i], std::dec);
      //      std::cout << position[i] << std::endl;
    }
}
