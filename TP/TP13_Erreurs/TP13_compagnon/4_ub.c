#include <stdio.h>
#include <stdbool.h>


int main()
{
  bool p[10] = {false,false,false,false,false,false,false,false,false,false};
  int i = 0;
  while (!p[i] && i < 1000)
  {
    printf("%d : %d\n", i, p[i]); 
    i++;
  }
  return 0;
}