#include <stdio.h>

int v_main() {
  int x = 2;
  int y = 3;
  int z = x % y;
  return z;
}

int main() {
  printf("%d", v_main());
  return 0;
}
