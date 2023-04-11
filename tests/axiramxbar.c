#define SCRATCHPAD_ADDR 0x2000
#include "mmio.h"

int main() {
    int64_t *ptr = SCRATCHPAD_ADDR;
    int64_t data = 10LL;

    for (int i=0; i < 10; i++) {
        ptr[i] = data + i;
    }

    for (int i=0; i < 10; i++) {
        int64_t val = ptr[i];
        printf("Val: %ld\n", val);
    }
}
