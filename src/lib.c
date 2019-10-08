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

const char* si64_prefix(const char* s, int64_t n) {
  int64_t l = strlen(s);
  n = n > l ? l : n;
  char* m = (char *)malloc(n + 1);
  memcpy(m, s, n);
  m[n] = '\0';
  return m;
}

const char* si64i64_slice(const char* s, int64_t n1, int64_t n2) {
  int64_t l = strlen(s);
  n2 = (n1 + n2) > l ? l : n1 + n2;
  int64_t n = n2 - n1;
  char* m = (char *)malloc(n);
  memcpy(m, s + n1, n2);
  m[n] = '\0';
  return m;
}

const char* si64_nth(const char* s, int64_t n) {
  char* m = (char *)malloc(2);
  m[0] = s[n];
  m[1] = '\0';
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
