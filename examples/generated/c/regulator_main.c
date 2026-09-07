// Драйвер проверки порождённого C: регулятор на q(8, 8) (examples/regulator.takt).
//
// Пропорциональный регулятор сходится сам и завершается (Adjust -> Settled ->
// Done), поднимая порт `ready`. Драйвер задаёт колбэк порта и крутит такты до
// терминального состояния.
#include "regulator.h"
#include <stdio.h>

static void write_bit(Regulator_Out_BitPort port, uint8_t bit, bool val, void *userdata) {
    (void)bit;
    (void)port;
    (void)userdata;
    printf("ready=%d\n", (int)val);
}

int main(void) {
    Regulator fsm;
    int i;

    fsm.write_bit = write_bit;
    fsm.userdata = NULL;
    Regulator_init(&fsm);

    for (i = 0; i < 100 && !Regulator_is_done(&fsm); i++) {
        Regulator_tick(&fsm);
    }

    printf("Регулятор: завершено за %d шагов\n", i);
    return 0;
}
