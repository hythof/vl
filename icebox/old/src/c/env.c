#include <stdio.h>
typedef struct {
    int (*__puts)(const char *s);
} console_t;
struct {
    console_t console;
} app_env;

int main()
{
    app_env.console.__puts = puts;
    return vm_run(app_main, 1024);
}
