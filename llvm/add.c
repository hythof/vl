#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

int64_t g = 1;
const char* m = "hello";

typedef struct {
  uint64_t len;
  char buf[0];
} array;

array* i64_array(int64_t n) {
  printf(m);
  array* a = (array*)malloc(sizeof(array) + sizeof(int64_t));
  ((int64_t*)a->buf)[0] = n;
  return a;
}

int64_t v_main() {
  array* a = i64_array(g);
  return (int64_t)(a->buf[0]);
}

int64_t upcast(char c) {
  return (int64_t)c;
}

int main() {
  g = 2;
  printf("%lld", (long long)v_main());

  char c=2;
  printf("%d", upcast(c));

  return 0;
}
