#include <iostream>

int main()
{
  int arr[5] = {1, 2, 3, 4, 5};
  std::cout << "Out-of-bounds element: " << arr[10] << std::endl;
  return 0;
}
