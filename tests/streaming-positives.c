#define PASSTHROUGH_WRITE 0x2200
//#define PASSTHROUGH_WRITE_LAST 0x2208
//#define PASSTHROUGH_WRITE_COUNT 0x2010
#define PASSTHROUGH_WRITE_COUNT 0x2208
#define PASSTHROUGH_READ 0x2300
#define PASSTHROUGH_READ_COUNT 0x2308

#define BP 5
#define BP_SCALE ((double)(1 << BP))

#include "mmio.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

int32_t roundi(double x)
{
  if (x < 0.0) {
    return (int32_t)(x - 0.5);
  } else {
    return (int32_t)(x + 0.5);
  }
}

int main(void)
{
  //double test_vector[15] = {-1.0, 2.0, 3.0, -4.0, 5.0, 4.0, -3.0, 2.0, 1.0, -0.5, 0.25, 0.125, 0.125};
  double test_vector[7] = {3, 2, 1, 0, -1.0, -2, -3} ;
  uint32_t num_tests = 7;
//  uint32_t num_tests = sizeof(test_vector) / sizeof(double);
  printf("Starting writing %d inputs\n", num_tests);

  for (int i = 0; i < num_tests; i++) {

    int32_t v = roundi(test_vector[i] * BP_SCALE);
    printf("i: %d, %d\n", i, v);
    //reg_write8(PASSTHROUGH_WRITE_LAST, 1);
    reg_write64(PASSTHROUGH_WRITE, v);
  }

  printf("Done writing\n");
  uint32_t rcnt = reg_read32(PASSTHROUGH_READ_COUNT);
  printf("Write count: %d\n", reg_read32(PASSTHROUGH_WRITE_COUNT));
  printf("Read count: %d\n", rcnt);

  int failed = 0;
  if (rcnt != 0) {

    for (int i = 0; i < num_tests; i++) {
      uint32_t res = reg_read32(PASSTHROUGH_READ) / BP_SCALE;
      // double res = ((double)reg_read32(PASSTHROUGH_READ)) / BP_SCALE;
      //double expected_double = test_vector[i] > 0 ? 1.0 : 0.0;
      uint32_t expected = test_vector[i] < 0 ? 1 : 0;
      if (res == expected) {
        printf("\n\nPass: Got %u Expected %u\n\n", res, expected);
      } else {
        failed = 1;
        printf("\n\nFail: Got %u Expected %u %f\n\n", res, expected, test_vector[i]);
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
  
  return 0;
}
