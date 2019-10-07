#include <stdio.h>

const char* string_string_append(const char* s1, const char* s2) {
  ssize_t l1 = (strlen)(s1);
  ssize_t l2 = (strlen)(s2);
  char* m = (char*)(malloc)(l1 + l2 + 1);
  (memcpy)(m, s1, l1);
  (memcpy)(m + l1, s2, l2);
  m[l1 + l2] = '\0';
  return m;
}

const char* v_main() {
  const char* x = string_string_append("h", "i");
  return x;
}

int main() {
  printf("%s", v_main());
  return 0;
}
