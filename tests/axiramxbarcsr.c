#define SCRATCHPAD_ADDR 0x2000
#define CSR_ADDR 0x1100
#include "mmio.h"

int main() {
    int64_t *ptr = SCRATCHPAD_ADDR;
    int64_t data = 10LL;

    for (int i=0; i < 10; i++) {
        ptr[i] = data + i;
    }

    int64_t csr = reg_read64(CSR_ADDR)
    printf("CSR %ld\n", csr);

    for (int i=0; i < 10; i++) {
        int64_t val = ptr[i];
        printf("Val: %ld\n", val);
    }
}
