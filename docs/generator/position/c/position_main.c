#include "position.h"

int main(void) {
  struct Main main;

  Main_init(&main);
  while (!Main_finished(&main)) {
    Main_tick(&main);
  }
  Main_reset(&main);
  return 0;
}