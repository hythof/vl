#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>

const char* ss_append(const char* s1, const char* s2) {
  ssize_t l1 = strlen(s1);
  ssize_t l2 = strlen(s2);
  char* m = (char *)malloc(l1 + l2 + 1);
  memcpy(m, s1, l1);
  memcpy(m + l1, s2, l2);
  m[l1 + l2] = '\0';
  return m;
}

int s_printf(const char* s) {
  printf("%s", s);
  return 0;
}

int true_printf() {
  printf("true");
  return 0;
}

int false_printf() {
  printf("false");
  return 0;
}

int i64_printf(int64_t n) {
  printf("%" PRId64, n);
  return 0;
}
