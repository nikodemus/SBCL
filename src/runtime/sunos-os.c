#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/file.h>

#include <unistd.h>
#include <errno.h>
#include <sys/param.h>
#include <sys/utsname.h>

#include "sbcl.h"
#include "os.h"
#include "arch.h"
#include "interr.h"
#include "interrupt.h"
#include "globals.h"
#include "validate.h"
#include "target-arch-os.h"

#ifdef LISP_FEATURE_X86
#include "genesis/static-symbols.h"
#include "genesis/fdefn.h"
#endif

#ifdef LISP_FEATURE_GENCGC
#include "gencgc-internal.h"
#endif

os_vm_size_t os_vm_page_size=0;

void
os_init(char *argv[], char *envp[])
{
    /*
     * historically, this used sysconf to select the runtime page size
     * per recent changes on other arches and discussion on sbcl-devel,
     * however, this is not necessary -- the VM page size need not match
     * the OS page size (and the default backend page size has been
     * ramped up accordingly for efficiency reasons).
     */
    os_vm_page_size = BACKEND_PAGE_BYTES;
}

os_vm_address_t os_validate(os_vm_address_t addr, os_vm_size_t len, boolean fixedp)
{
    int flags = MAP_PRIVATE | MAP_NORESERVE | MAP_ANON;
    if (addr && fixedp)
        flags |= MAP_FIXED;

    addr = mmap(addr, len, OS_VM_PROT_ALL, flags, -1, 0);

    if (addr == MAP_FAILED) {
        return NULL;
    }

    return addr;
}

void os_invalidate(os_vm_address_t addr, os_vm_size_t len)
{
    if(munmap((void*) addr, len) == -1)
        perror("munmap");
}



os_vm_address_t
os_map(int fd, int offset, os_vm_address_t addr, os_vm_size_t len)
{

    addr = mmap(addr, len,
                OS_VM_PROT_ALL,
                MAP_PRIVATE | MAP_FIXED,
                fd, (off_t) offset);

    if (addr == MAP_FAILED) {
        perror("mmap");
        lose("Unexpedted mmap(..) failure\n");
    }

    return addr;
}

void
os_protect(os_vm_address_t address, os_vm_size_t length, os_vm_prot_t prot)
{
    if(mprotect((void*)address, length, prot) == -1) {
        perror("mprotect");
    }
}


#if defined LISP_FEATURE_GENCGC

void
sigsegv_handler(int signal, siginfo_t *info, os_context_t *context)
{
    void* fault_addr = (void*)info->si_addr;

    if (!gencgc_handle_wp_violation(fault_addr))
        if(!handle_guard_page_triggered(context, fault_addr))
            lisp_memory_fault_error(context, fault_addr);
}

#else

static void
sigsegv_handler(int signal, siginfo_t *info, os_context_t *context)
{
    os_vm_address_t addr = arch_get_bad_addr(signal, info, context);

    if (!cheneygc_handle_wp_violation(context, addr)) {
        if (!handle_guard_page_triggered(context,addr))
            lisp_memory_fault_error(context, addr);
    }
}

#endif

void
os_install_interrupt_handlers()
{
    undoably_install_low_level_interrupt_handler(SIG_MEMORY_FAULT,
                                                 sigsegv_handler);

#ifdef LISP_FEATURE_SB_THREAD
    undoably_install_low_level_interrupt_handler(SIG_STOP_FOR_GC,
                                                 sig_stop_for_gc_handler);
#endif
}

char *
os_get_runtime_executable_path(int external)
{
    char path[] = "/proc/self/object/a.out";

    if (external || access(path, R_OK) == -1)
        return NULL;

    return copied_string(path);
}

