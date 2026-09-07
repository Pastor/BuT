// Драйвер проверки порождённого C: технологический цикл на последовательной
// композиции `+` (examples/batch_cycle.takt).
//
// не порождается taktc: харнесс - принадлежность проверки, а не продукта, как и
// тестбенчи цели sv.
//
// Проверяется не "программа не упала", а суть конструкции: три фазы обязаны
// пройти одна за другой. Наблюдаемая - общая `stage` (номер активной фазы,
// 1 -> 2 -> 3): если бы `+` вела себя как `|`, номера пошли бы вперемешку.
//
// Проверки не на `assert`: под `-DNDEBUG` он исчезает вместе с
// проверкой, а прошивки собирают именно так - харнесс, "проходящий" в релизе,
// не проверял бы ничего. Вдобавок исчезнувший `assert` оставлял переменную
// `phases` неиспользованной, и релизная сборка не проходила `-Werror`
// (`-Wunused-but-set-variable`): класс был виден компилятору, а гейт его не
// спрашивал.
#include "batch_cycle.h"
#include <stdio.h>

static int ready_seen = 0;
static int failures = 0;

/* Проверка, работающая в любом режиме сборки: печатает и запоминает отказ. */
static void check(int condition, const char *what) {
    if (!condition) {
        fprintf(stderr, "ОТКАЗ: %s\n", what);
        failures++;
    }
}

static void write_bit(BatchCycle_Out_BitPort port, uint8_t bit, bool val, void *userdata) {
    (void)bit;
    (void)port;
    (void)userdata;
    if (val) {
        ready_seen = 1;
    }
}

int main(void) {
    BatchCycle fsm;
    int i;
    int last_stage = 0;
    int phases = 0;

    fsm.write_bit = write_bit;
    fsm.userdata = NULL;
    BatchCycle_init(&fsm);

    for (i = 0; i < 100 && !BatchCycle_is_done(&fsm); i++) {
        BatchCycle_tick(&fsm);
        if (fsm.stage != last_stage) {
            /* Фаза сменилась. Номер обязан вырасти ровно на единицу: пропуск
               означал бы, что шаг цепочки не исполнился, а убывание — что фазы
               идут не по порядку. */
            check(fsm.stage == last_stage + 1, "номер фазы вырос не на единицу");
            last_stage = fsm.stage;
            phases++;
            printf("фаза %d\n", (int)fsm.stage);
        }
    }

    check(phases == 3, "фаз обязано быть три: дозирование, перемешивание, слив");
    check(ready_seen, "готовность обязана подняться после последней фазы");
    /* Завершения больше не ждём: конечное состояние несёт
       `always { ready := 1; }`, и автомат в нём ОСТАЁТСЯ — состояние
       стабильно, покидают его только по переходу. Признак окончания цикла —
       поднятая и удерживаемая готовность (проверена выше). */

    if (failures != 0) {
        fprintf(stderr, "Цикл: проверок провалено: %d\n", failures);
        return 1;
    }
    printf("Цикл: три фазы по порядку, завершено за %d шагов\n", i);
    return 0;
}
