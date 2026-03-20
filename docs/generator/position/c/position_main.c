#include "position.h"

int main(void) {
  struct Main main = {.port = {.port_bit_read = 0,
                               .port_bit_write = 0,
                               .port_int_read = 0,
                               .port_int_write = 0,
                               .port_real_read = 0,
                               .port_real_write = 0,
                               .userdata = 0}};

  Main_init(&main);
  while (!Main_finished(&main)) {
    Main_tick(&main);
  }
  Main_reset(&main);
  return 0;
}