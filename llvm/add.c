#include <stdio.h>

int v_main() {
  int x = 2;
  x = 3;
  x = 4;
  x = 5;
  return x;
}

int main() {
  printf("%d", v_main());
  return 0;
}
