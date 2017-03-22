#include "vm.h"


void
app_logic(vm_t *vm) {
    write(vm->stdout, "hello\n", 6);
    vm_print_status(vm);
    vm->return_code = 0;
    vm->status = VM_STATUS_EXIT;
}

// main
int
main(int argc, char *argv[]) {
    int ret;
    vm_t *vm = vm_create(app_logic, 0);
    vm->main_loop(vm);
    ret = vm->return_code;
    vm_destroy(vm);
    return ret;
}
