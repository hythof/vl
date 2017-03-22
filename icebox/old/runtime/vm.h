#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <signal.h>
#include <errno.h>
#include <time.h>

#define PARAM_DEFAULT_MEMORY_SIZE 1024 * 1024 * 4
#define PARAM_ERROR_COUNT 32
#define die(status) exit(status);

// error
typedef struct {
    int no;
    time_t time;
    const char *msg;
    const char *func;
    const char *file;
    unsigned int line;
} error_t;
#define vm_error(vm, msg) do { \
    vm->errors[vm->error_index].no = errno;        \
    vm->errors[vm->error_index].func = __func__;   \
    vm->errors[vm->error_index].file = __FILE__;   \
    vm->errors[vm->error_index].line = __LINE__;   \
    vm->errors[vm->error_index].time = time(NULL); \
    vm->error_index = (vm->error_index + 1) % PARAM_ERROR_COUNT; \
} while(0)

// vm
enum {
    VM_STATUS_INIT,
    VM_STATUS_RUN,
    VM_STATUS_WAIT,
    VM_STATUS_SIGNAL,
    VM_STATUS_EXIT,
};
typedef struct vm vm_t;
struct vm {
    int stdin;
    int stdout;
    int stderr;

    // callback
    void (*main_loop)(vm_t *);

    // flags
    int status;
    int return_code;

    // error
    size_t error_index;
    error_t errors[PARAM_ERROR_COUNT];

    // memory
    size_t memory_size;
    size_t memory_index;
    char *memory;
};

vm_t * vm_create(void (*main_loop)(vm_t *), size_t memory_size);
void* vm_malloc(vm_t *vm, size_t size);
void vm_destroy(vm_t *vm);
void vm_print_status(vm_t *vm);
int vm_run(void(*main_loop)(vm_t*), size_t memory_size);
