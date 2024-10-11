#include <stdio.h>

int main() {
  char c[200] = "Hello";
  char b[200] = "world";
  c[20] = 'd';
  printf("c is %s and b is %s", c, b);
  return 0;
}
