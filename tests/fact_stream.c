#include "mmio.h"

#define WRITE_ADDR 0x2200
#define WRITE_LAST 0x2208
#define WRITE_COUNT 0x2210
//#define WRITE_COUNT 0x2208
#define READ_ADDR 0x2300
#define READ_COUNT 0x2308

unsigned int fact_ref(unsigned int n) {
  unsigned int res = 1;
  while (n != 1) {
    res *= n;
    n--;
  }
  return res;
}

// DOC include start: GCD test
int main(void)
{
    printf("Starting writing\n");
    uint32_t test_vector[7] = {1, 2, 3, 4, 5, 6, 7} ;
    uint32_t test_result[7] = {1, 2, 6, 24, 120, 720, 5040} ;

  //reg_write8(WRITE_LAST, 0);
    for (int i = 0; i < 7; i++) {
      reg_write64(WRITE_ADDR, test_vector[i]);
      reg_write8(WRITE_LAST, 1);
    }

    printf("Done writing\n");
    uint32_t rcnt = reg_read32(READ_COUNT);
    printf("Write count: %d\n", reg_read32(WRITE_COUNT));
    printf("Read count: %d\n", rcnt);

    int failed = 0;
    if (rcnt != 0) {
    for (int i = 0; i < 7; i++) {
      uint32_t res = reg_read32(READ_ADDR);
      uint32_t expected = test_result[i];
      if (res == expected) {
        printf("\n\nPass: Got %d Expected %d\n\n", res, test_result[i]);
      } else {
        failed = 1;
        printf("\n\nFail: Got %d Expected %d\n\n", res, test_result[i]);
      }
    }
    } else {
    failed = 1;
    }

    if (failed) {
    printf("\n\nSome tests failed\n\n");
    } else {
    printf("\n\nAll tests passed\n\n");
    }

}
