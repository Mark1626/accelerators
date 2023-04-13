#define SCRATCHPAD_ADDR 0x2000
#define CSR_ADDR 0x1100
#include "mmio.h"

int main() {
    int64_t *ptr = SCRATCHPAD_ADDR;
    int64_t data = 10LL;

    int64_t *csr = CSR_ADDR;
    printf("CSR %ld\n", csr[0]);

    ptr[0] = 10;
    printf("Val1: %ld\n", ptr[0]);

    csr[1] = SCRATCHPAD_ADDR;

    printf("Val2: %ld\n", csr[2]);

}
