// endl_string_variable.cpp
#include <iostream>
#include <cstdlib>
// Strings C++.
#include <string>

int main()
{
  std::string user_name = "user"; // Define a variable.
  std::cout << "Hello, " << user_name << "!" << std::endl;

  user_name = "The Great Whale"; // Change the value of a variable.
  std::cout << "I am " << user_name;
  return EXIT_SUCCESS; // We will return the OS "success code"
}