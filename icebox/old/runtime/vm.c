#include "vm.h"

// vm
vm_t *
vm_create(void (*main_loop)(vm_t *), size_t memory_size) {
    vm_t *vm;

    if(memory_size == 0) {
        memory_size = PARAM_DEFAULT_MEMORY_SIZE;
    }

    vm = calloc(1, sizeof(vm_t));
    vm->status = VM_STATUS_INIT;
    vm->stdin = STDIN_FILENO;
    vm->stdout = STDOUT_FILENO;
    vm->stderr = STDERR_FILENO;
    vm->memory_size = memory_size;
    vm->memory = (char *)malloc(memory_size);
    vm->main_loop = main_loop;
    vm->return_code = 0;
    return vm;
}
void
vm_wrap_main_loop(vm_t *vm, void (*main_loop)(vm_t *)) {
    vm->status = VM_STATUS_RUN;
    main_loop(vm);
    vm->status = VM_STATUS_EXIT;
}
void*
vm_malloc(vm_t *vm, size_t size) {
    size_t pos;

    pos = vm->memory_index + size;
    if(pos > vm->memory_size) {
        vm->memory_size = pos * 2;
        vm->memory = realloc(vm->memory, vm->memory_size);
    }
    vm->memory_index = pos;
    return (void *)(vm->memory + vm->memory_index - size);
}
void
vm_destroy(vm_t *vm) {
    free(vm->memory);
    free(vm);
}
void
vm_print_status(vm_t *vm) {
    size_t i;
    error_t *e;

    printf("memory size = %loMB\n", vm->memory_size / 1024 / 1024);
    printf("memory use  = %lo\n", vm->memory_index);
    printf("memory head = %p\n", vm->memory);
    for(i=vm->error_index; i>0; --i) {
        e = &vm->errors[i];
        if(e->line == 0) {
            break;
        }
        printf("%s %d %s | %s:%d) %s\n", ctime((const time_t *)e->time), e->no, e->msg, e->func, e->line, e->func);
    }
    for(i=0; i<vm->error_index; ++i) {
        e = &vm->errors[i];
        if(e->line == 0) {
            break;
        }
        printf("%s %d %s | %s:%d) %s\n", ctime((const time_t *)e->time), e->no, e->msg, e->func, e->line, e->func);
    }
}
int
vm_run(void(*main_loop)(vm_t*), size_t memory_size) {
    int ret;
    vm_t *vm = vm_create(main_loop, memory_size);
    vm->main_loop(vm);
    ret = vm->return_code;
    vm_destroy(vm);
    return ret;
}
