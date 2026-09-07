/**
 * stacker_main.c - Симулятор автоматизированного штабелятора.
 *
 * Физика: штабелятор движется по 1 единице/такт на каждой оси к cmd_target_*.
 * Вилы: sense_loaded меняется через такт после выдвижения cmd_fork=1,
 *        направление (захват/укладка) определяется по fsm.lift_op.
 *
 * Сценарии
 * ────────
 * SIM1 [POSITIVE] Приёмка: задание type=0, ячейка [3,2,8].
 *   Ожидаемо: cmd_ack=1 -> паллет взят на приёмке -> паллет уложен -> cmd_done=1.
 *
 * SIM2 [POSITIVE] Отгрузка: задание type=1, ячейка [5,4,12].
 *   Ожидаемо: cmd_ack=1 -> паллет взят из ячейки -> доставлен на выдачу -> cmd_done=1.
 *
 * SIM3 [NEGATIVE] Аварийная зарядка: батарея садится в ходе движения.
 *   Ожидаемо: задание прерывается, cmd_done не выдаётся, штабелятор уходит
 *             на зарядку и возвращается в MovementIdle.
 *
 * SIM4 [NEGATIVE] Задание при разряженной батарее.
 *   Ожидаемо: cmd_ack и cmd_done не выдаются, busy остаётся 0.
 *
 * SIM5 [POSITIVE] 20 заданий подряд: чередование приёмки и отгрузки.
 *   Ожидаемо: каждое задание получает cmd_ack и cmd_done; после всех заданий
 *             busy=0, вилы свободны, MovementController в Idle.
 */

#include "stacker.h"

#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>

/* ════════════════════════════════════════════════════════════════════════════
   Состояние симулятора
   ════════════════════════════════════════════════════════════════════════════ */

typedef struct {
    /* Входные битовые порты */
    bool sense_at_charge;
    bool sense_battery_low;
    bool sense_loaded;
    bool task_type;
    bool task_valid;

    /* Входные числовые порты */
    uint8_t pos_stack, pos_row, pos_section;
    uint8_t task_stack_no, task_row_no, task_section_no;

    /* Выходные битовые порты (записываются FSM через коллбэки) */
    bool cmd_ack;
    bool cmd_done;
    bool cmd_fork;

    /* Выходные числовые порты */
    uint8_t cmd_target_stack, cmd_target_row, cmd_target_section;

    /* Флаги событий - поднимаются при write_bit(val=true) */
    bool ack_seen;
    bool done_seen;

    /* Имя сценария и счётчик ошибок */
    const char *name;
    int errors;

    /* Учёт ячеек склада [stack 0..11][row 0..10][section 0..20].
     * true = ячейка занята паллетом.
     * Зона приёмки (0,1,1) - бесконечный источник, не отслеживается.
     * Зона выдачи (11,1,1) - бесконечный приёмник, не отслеживается. */
    bool cells[12][11][21];
} SimState;

/* ════════════════════════════════════════════════════════════════════════════
   Коллбэки портов
   ════════════════════════════════════════════════════════════════════════════ */

static bool sim_read_bit(Stacker_In_BitPort port, uint8_t bit, void *ud) {
    (void)bit;
    SimState *s = (SimState *)ud;
    switch (port) {
        case STACKER_PORT_SENSE_AT_CHARGE:   return s->sense_at_charge;
        case STACKER_PORT_SENSE_BATTERY_LOW: return s->sense_battery_low;
        case STACKER_PORT_SENSE_LOADED:      return s->sense_loaded;
        case STACKER_PORT_TASK_TYPE:         return s->task_type;
        case STACKER_PORT_TASK_VALID:        return s->task_valid;
    }
    return false;
}

static void sim_write_bit(Stacker_Out_BitPort port, uint8_t bit, bool val, void *ud) {
    (void)bit;
    SimState *s = (SimState *)ud;
    switch (port) {
        case STACKER_PORT_CMD_ACK:
            s->cmd_ack = val;
            if (val) s->ack_seen = true;
            break;
        case STACKER_PORT_CMD_DONE:
            s->cmd_done = val;
            if (val) s->done_seen = true;
            break;
        case STACKER_PORT_CMD_FORK:
            s->cmd_fork = val;
            break;
    }
}

static int64_t sim_read_numeric(Stacker_In_NumericPort port, uint8_t index, void *ud) {
    (void)index;
    SimState *s = (SimState *)ud;
    switch (port) {
        case STACKER_PORT_POS_STACK:       return s->pos_stack;
        case STACKER_PORT_POS_ROW:         return s->pos_row;
        case STACKER_PORT_POS_SECTION:     return s->pos_section;
        case STACKER_PORT_TASK_STACK_NO:   return s->task_stack_no;
        case STACKER_PORT_TASK_ROW_NO:     return s->task_row_no;
        case STACKER_PORT_TASK_SECTION_NO: return s->task_section_no;
    }
    return 0;
}

static void sim_write_numeric(Stacker_Out_NumericPort port, uint8_t index, int64_t val, void *ud) {
    (void)index;
    SimState *s = (SimState *)ud;
    switch (port) {
        case STACKER_PORT_CMD_TARGET_STACK:   s->cmd_target_stack   = (uint8_t)val; break;
        case STACKER_PORT_CMD_TARGET_ROW:     s->cmd_target_row     = (uint8_t)val; break;
        case STACKER_PORT_CMD_TARGET_SECTION: s->cmd_target_section = (uint8_t)val; break;
    }
}

/* ════════════════════════════════════════════════════════════════════════════
   Физика симулятора
   ════════════════════════════════════════════════════════════════════════════ */

static uint8_t step_toward(uint8_t cur, uint8_t tgt) {
    if (cur < tgt) return (uint8_t)(cur + 1);
    if (cur > tgt) return (uint8_t)(cur - 1);
    return cur;
}

/* Вызывается перед Stacker_tick(): движение, датчик зарядки, вилы. */
static void sim_update_physics(SimState *s, const Stacker *fsm) {
    s->pos_stack   = step_toward(s->pos_stack,   s->cmd_target_stack);
    s->pos_row     = step_toward(s->pos_row,     s->cmd_target_row);
    s->pos_section = step_toward(s->pos_section, s->cmd_target_section);

    s->sense_at_charge = (s->pos_stack == 0 && s->pos_row == 0 && s->pos_section == 0);

    /* Реакция на вилы: один паллет за раз.
     * Захват: приёмка - бесконечный источник; ячейка - только если занята.
     * Укладка: выдача - бесконечный приёмник; ячейка помечается занятой. */
    if (s->cmd_fork && fsm->lift_request) {
        bool at_pickup  = (s->pos_stack ==  0 && s->pos_row == 1 && s->pos_section == 1);
        bool at_dropoff = (s->pos_stack == 11 && s->pos_row == 1 && s->pos_section == 1);

        if (!fsm->lift_op && !s->sense_loaded) {
            /* Захват паллета */
            bool has_pallet = at_pickup
                           || s->cells[s->pos_stack][s->pos_row][s->pos_section];
            if (has_pallet) {
                s->sense_loaded = true;
                if (!at_pickup)
                    s->cells[s->pos_stack][s->pos_row][s->pos_section] = false;
            }
        } else if (fsm->lift_op && s->sense_loaded) {
            /* Укладка / выдача паллета */
            s->sense_loaded = false;
            if (!at_dropoff)
                s->cells[s->pos_stack][s->pos_row][s->pos_section] = true;
        }
    }
}

/* ════════════════════════════════════════════════════════════════════════════
   Имена состояний для вывода
   ════════════════════════════════════════════════════════════════════════════ */

static const char *cr_state(int s) {
    switch (s) {
        case STACKER_COMMAND_RECEIVER_INIT:             return "INIT";
        case STACKER_COMMAND_RECEIVER_WAITING_FOR_TASK: return "Waiting";
        case STACKER_COMMAND_RECEIVER_ACCEPTING_TASK:   return "Accepting";
        case STACKER_COMMAND_RECEIVER_TASK_ACTIVE:      return "Active";
        case STACKER_COMMAND_RECEIVER_END:              return "END";
        default:                                        return "?";
    }
}

static const char *mv_state(int s) {
    switch (s) {
        case STACKER_MOVEMENT_CONTROLLER_INIT:                    return "INIT";
        case STACKER_MOVEMENT_CONTROLLER_MOVEMENT_IDLE:           return "Idle";
        case STACKER_MOVEMENT_CONTROLLER_DISPATCH_MOVE:           return "Dispatch";
        case STACKER_MOVEMENT_CONTROLLER_MOVING_TO_PICKUP:        return "→Pickup";
        case STACKER_MOVEMENT_CONTROLLER_WAITING_FORK_AT_PICKUP:  return "ForkPickup";
        case STACKER_MOVEMENT_CONTROLLER_MOVING_TO_CELL:          return "→Cell";
        case STACKER_MOVEMENT_CONTROLLER_WAITING_FORK_AT_CELL:    return "ForkCell";
        case STACKER_MOVEMENT_CONTROLLER_MOVING_TO_STORAGE:       return "→Storage";
        case STACKER_MOVEMENT_CONTROLLER_WAITING_FORK_AT_STORAGE: return "ForkStorage";
        case STACKER_MOVEMENT_CONTROLLER_MOVING_TO_DROPOFF:       return "→Dropoff";
        case STACKER_MOVEMENT_CONTROLLER_WAITING_FORK_AT_DROPOFF: return "ForkDropoff";
        case STACKER_MOVEMENT_CONTROLLER_TASK_COMPLETING:         return "Completing";
        case STACKER_MOVEMENT_CONTROLLER_EMERGENCY_CHARGE:        return "EMERGENCY!";
        case STACKER_MOVEMENT_CONTROLLER_END:                     return "END";
        default:                                                   return "?";
    }
}

static const char *lf_state(int s) {
    switch (s) {
        case STACKER_LIFT_CONTROLLER_INIT:          return "INIT";
        case STACKER_LIFT_CONTROLLER_LIFT_IDLE:     return "Idle";
        case STACKER_LIFT_CONTROLLER_LIFT_OPERATING:return "Operating";
        case STACKER_LIFT_CONTROLLER_LIFT_DONE:     return "Done";
        case STACKER_LIFT_CONTROLLER_END:           return "END";
        default:                                    return "?";
    }
}

/* ════════════════════════════════════════════════════════════════════════════
   Вывод состояния одного такта
   ════════════════════════════════════════════════════════════════════════════ */

static void print_tick(int tick, const SimState *s, const Stacker *fsm) {
    printf("T%3d | pos(%2u,%2u,%2u)→(%2u,%2u,%2u)"
           " | task[t=%d s=%u r=%u y=%u v=%d]"
           " | fork=%d load=%d chrg=%d batt=%d"
           " | ack=%d done=%d eta=%u"
           " | CR=%-10s MV=%-12s LF=%-10s\n",
           tick,
           s->pos_stack, s->pos_row, s->pos_section,
           s->cmd_target_stack, s->cmd_target_row, s->cmd_target_section,
           (int)s->task_type,
           s->task_stack_no, s->task_row_no, s->task_section_no,
           (int)s->task_valid,
           (int)s->cmd_fork, (int)s->sense_loaded,
           (int)s->sense_at_charge, (int)s->sense_battery_low,
           (int)s->cmd_ack, (int)s->cmd_done, (unsigned)fsm->eta,
           cr_state((int)fsm->stacker.command_receiver0.state),
           mv_state((int)fsm->stacker.movement_controller1.state),
           lf_state((int)fsm->stacker.lift_controller2.state));
}

/* ════════════════════════════════════════════════════════════════════════════
   Проверочные макросы
   ════════════════════════════════════════════════════════════════════════════ */

#define CHECK(sim, cond, msg) do { \
    if (!(cond)) { \
        printf("  [FAIL] %-6s: %s\n", (sim)->name, (msg)); \
        (sim)->errors++; \
    } else { \
        printf("  [PASS] %-6s: %s\n", (sim)->name, (msg)); \
    } \
} while (0)

/* ════════════════════════════════════════════════════════════════════════════
   Вспомогательные функции
   ════════════════════════════════════════════════════════════════════════════ */

static void stacker_setup(Stacker *fsm, SimState *sim) {
    memset(fsm, 0, sizeof(*fsm));
    fsm->userdata      = sim;
    fsm->read_bit      = sim_read_bit;
    fsm->write_bit     = sim_write_bit;
    fsm->read_numeric  = sim_read_numeric;
    fsm->write_numeric = sim_write_numeric;
    Stacker_init(fsm);
}

/* Один шаг: физика -> тик FSM -> вывод. */
static void sim_step(int tick, SimState *sim, Stacker *fsm) {
    sim_update_physics(sim, fsm);
    Stacker_tick(fsm);
    print_tick(tick, sim, fsm);
}

/* ════════════════════════════════════════════════════════════════════════════
   SIM1 — POSITIVE: Приёмка паллета (task_type=0), ячейка [3,2,8]
   ════════════════════════════════════════════════════════════════════════════ */

static int run_sim1(void) {
    printf("\n═══════════════════════════════════════════════════════════════════\n");
    printf("SIM1 [POSITIVE] Приёмка: task_type=0, ячейка [3,2,8]\n");
    printf("═══════════════════════════════════════════════════════════════════\n");

    SimState sim = {0};
    sim.name           = "SIM1";
    sim.sense_at_charge = true;
    /* задание */
    sim.task_type       = false;
    sim.task_stack_no   = 3;
    sim.task_row_no     = 2;
    sim.task_section_no = 8;

    Stacker fsm;
    stacker_setup(&fsm, &sim);
    print_tick(0, &sim, &fsm);

    /* инициализационные такты */
    for (int t = 1; t <= 3; t++) sim_step(t, &sim, &fsm);

    sim.task_valid = true;
    printf("  >> WMS: task_valid=1 (приёмка → ячейка [3,2,8])\n");

    int done_tick = -1;
    for (int t = 4; t <= 70; t++) {
        if (sim.ack_seen && sim.task_valid) {
            sim.task_valid = false;
            printf("  >> WMS: cmd_ack получен — снимаем task_valid\n");
        }
        sim_step(t, &sim, &fsm);
        if (sim.done_seen && done_tick < 0) {
            printf("  >> cmd_done=1 обнаружен на такте %d\n", t);
            done_tick = t;
        }
        /* дополнительный такт для перехода Completing -> Idle */
        if (done_tick > 0 && t >= done_tick + 1) break;
    }

    CHECK(&sim, sim.ack_seen,  "cmd_ack=1 выдан при приёме задания");
    CHECK(&sim, sim.done_seen, "cmd_done=1 выдан после укладки паллета");
    CHECK(&sim, !sim.sense_loaded,
          "вилы без паллета после завершения");
    CHECK(&sim, fsm.stacker.movement_controller1.state ==
                STACKER_MOVEMENT_CONTROLLER_MOVEMENT_IDLE,
          "MovementController вернулся в Idle");
    return sim.errors;
}

/* ════════════════════════════════════════════════════════════════════════════
   SIM2 — POSITIVE: Отгрузка паллета (task_type=1), ячейка [5,4,12]
   ════════════════════════════════════════════════════════════════════════════ */

static int run_sim2(void) {
    printf("\n═══════════════════════════════════════════════════════════════════\n");
    printf("SIM2 [POSITIVE] Отгрузка: task_type=1, ячейка [5,4,12] → выдача\n");
    printf("═══════════════════════════════════════════════════════════════════\n");

    SimState sim = {0};
    sim.name            = "SIM2";
    sim.sense_at_charge = true;
    sim.task_type       = true;    /* 1 = отгрузка */
    sim.task_stack_no   = 5;
    sim.task_row_no     = 4;
    sim.task_section_no = 12;
    /* Ячейка [5,4,12] предзаполнена: имитируем паллет, уложенный ранее */
    sim.cells[5][4][12] = true;
    printf("  >> СКЛАД: ячейка [5,4,12] содержит паллет\n");

    Stacker fsm;
    stacker_setup(&fsm, &sim);
    print_tick(0, &sim, &fsm);

    for (int t = 1; t <= 3; t++) sim_step(t, &sim, &fsm);

    sim.task_valid = true;
    printf("  >> WMS: task_valid=1 (отгрузка из ячейки [5,4,12])\n");

    int done_tick = -1;
    for (int t = 4; t <= 90; t++) {
        if (sim.ack_seen && sim.task_valid) {
            sim.task_valid = false;
            printf("  >> WMS: cmd_ack получен — снимаем task_valid\n");
        }
        sim_step(t, &sim, &fsm);
        if (sim.done_seen && done_tick < 0) {
            printf("  >> cmd_done=1 обнаружен на такте %d\n", t);
            done_tick = t;
        }
        if (done_tick > 0 && t >= done_tick + 1) break;
    }

    CHECK(&sim, sim.ack_seen,  "cmd_ack=1 выдан при приёме задания");
    CHECK(&sim, sim.done_seen, "cmd_done=1 выдан после доставки паллета");
    CHECK(&sim, !sim.sense_loaded,
          "вилы без паллета после завершения");
    CHECK(&sim, fsm.stacker.movement_controller1.state ==
                STACKER_MOVEMENT_CONTROLLER_MOVEMENT_IDLE,
          "MovementController вернулся в Idle");
    return sim.errors;
}

/* ════════════════════════════════════════════════════════════════════════════
   SIM3 — NEGATIVE: Аварийная зарядка (разряд батареи в ходе движения)
   ════════════════════════════════════════════════════════════════════════════ */

static int run_sim3(void) {
    printf("\n═══════════════════════════════════════════════════════════════════\n");
    printf("SIM3 [NEGATIVE] Разряд батареи в пути к ячейке [7,5,15]\n");
    printf("═══════════════════════════════════════════════════════════════════\n");

    SimState sim = {0};
    sim.name           = "SIM3";
    sim.sense_at_charge = true;
    sim.task_type       = false;
    sim.task_stack_no   = 7;
    sim.task_row_no     = 5;
    sim.task_section_no = 15;

    Stacker fsm;
    stacker_setup(&fsm, &sim);
    print_tick(0, &sim, &fsm);

    for (int t = 1; t <= 3; t++) sim_step(t, &sim, &fsm);

    sim.task_valid = true;
    printf("  >> WMS: task_valid=1 (приёмка → ячейка [7,5,15])\n");

    /* тактируем до подтверждения и начала движения */
    for (int t = 4; t <= 10; t++) {
        if (sim.ack_seen && sim.task_valid) {
            sim.task_valid = false;
            printf("  >> WMS: cmd_ack получен — снимаем task_valid\n");
        }
        sim_step(t, &sim, &fsm);
    }

    /* вводим разряд батареи */
    sim.sense_battery_low = true;
    printf("  >> FAULT: sense_battery_low=1 — разряд!\n");

    bool emergency_seen    = false;
    bool done_in_emergency = false;

    for (int t = 11; t <= 60; t++) {
        sim_step(t, &sim, &fsm);

        if (fsm.stacker.movement_controller1.state ==
            STACKER_MOVEMENT_CONTROLLER_EMERGENCY_CHARGE)
            emergency_seen = true;

        if (sim.cmd_done) done_in_emergency = true;

        /* снимаем разряд при возврате к зарядке */
        if (sim.sense_at_charge && sim.sense_battery_low) {
            sim.sense_battery_low = false;
            printf("  >> RESTORE: зарядка завершена (sense_battery_low=0)\n");
        }
    }

    CHECK(&sim, sim.ack_seen,
          "cmd_ack=1 был выдан до разряда");
    CHECK(&sim, emergency_seen,
          "FSM перешёл в EmergencyCharge при разряде");
    CHECK(&sim, !done_in_emergency,
          "cmd_done НЕ выдан в ходе аварийной зарядки");
    CHECK(&sim, fsm.stacker.movement_controller1.state ==
                STACKER_MOVEMENT_CONTROLLER_MOVEMENT_IDLE,
          "после зарядки MovementController вернулся в Idle");
    CHECK(&sim, !fsm.busy,
          "флаг busy сброшен — задание прервано");
    return sim.errors;
}

/* ════════════════════════════════════════════════════════════════════════════
   SIM4 — NEGATIVE: Отказ приёма задания при разряженной батарее
   ════════════════════════════════════════════════════════════════════════════ */

static int run_sim4(void) {
    printf("\n═══════════════════════════════════════════════════════════════════\n");
    printf("SIM4 [NEGATIVE] WMS выдаёт задание при разряженной батарее\n");
    printf("═══════════════════════════════════════════════════════════════════\n");

    SimState sim = {0};
    sim.name             = "SIM4";
    sim.sense_at_charge  = true;
    sim.sense_battery_low = true;   /* батарея мертва с самого начала */
    sim.task_type        = false;
    sim.task_stack_no    = 2;
    sim.task_row_no      = 3;
    sim.task_section_no  = 5;

    Stacker fsm;
    stacker_setup(&fsm, &sim);
    print_tick(0, &sim, &fsm);

    for (int t = 1; t <= 3; t++) sim_step(t, &sim, &fsm);

    sim.task_valid = true;
    printf("  >> WMS: task_valid=1 при sense_battery_low=1\n");

    for (int t = 4; t <= 15; t++) sim_step(t, &sim, &fsm);

    CHECK(&sim, !sim.ack_seen,
          "cmd_ack НЕ выдан — задание отклонено из-за разряда");
    CHECK(&sim, !sim.done_seen,
          "cmd_done НЕ выдан — задание не исполнялось");
    CHECK(&sim, !fsm.busy,
          "busy=0 — задание не принято");
    CHECK(&sim, fsm.stacker.command_receiver0.state ==
                STACKER_COMMAND_RECEIVER_WAITING_FOR_TASK,
          "CommandReceiver остался в WaitingForTask");
    return sim.errors;
}

/* ════════════════════════════════════════════════════════════════════════════
   SIM5 — POSITIVE: 20 заданий подряд (приёмка и отгрузка)
   ════════════════════════════════════════════════════════════════════════════ */

static int run_sim5(void) {
    printf("\n═══════════════════════════════════════════════════════════════════\n");
    printf("SIM5 [POSITIVE] 20 заданий подряд: приёмка и отгрузка\n");
    printf("═══════════════════════════════════════════════════════════════════\n");

    typedef struct { bool type; uint8_t stack, row, section; } Task;
    static const Task tasks[20] = {
        {false,  3,  2,  8},   /*  1 приёмка  → [3,2,8]   */
        {false,  7,  5, 15},   /*  2 приёмка  → [7,5,15]  */
        {false,  1,  1,  3},   /*  3 приёмка  → [1,1,3]   */
        {false,  9,  8, 18},   /*  4 приёмка  → [9,8,18]  */
        {false,  5,  4, 12},   /*  5 приёмка  → [5,4,12]  */
        {true,   3,  2,  8},   /*  6 отгрузка ← [3,2,8]   */
        {true,   7,  5, 15},   /*  7 отгрузка ← [7,5,15]  */
        {false,  2,  6,  7},   /*  8 приёмка  → [2,6,7]   */
        {false,  8,  3, 11},   /*  9 приёмка  → [8,3,11]  */
        {false,  4,  9, 14},   /* 10 приёмка  → [4,9,14]  */
        {true,   1,  1,  3},   /* 11 отгрузка ← [1,1,3]   */
        {true,   9,  8, 18},   /* 12 отгрузка ← [9,8,18]  */
        {true,   5,  4, 12},   /* 13 отгрузка ← [5,4,12]  */
        {false,  6,  7,  9},   /* 14 приёмка  → [6,7,9]   */
        {true,   2,  6,  7},   /* 15 отгрузка ← [2,6,7]   */
        {false, 10,  2, 16},   /* 16 приёмка  → [10,2,16] */
        {true,   8,  3, 11},   /* 17 отгрузка ← [8,3,11]  */
        {true,   4,  9, 14},   /* 18 отгрузка ← [4,9,14]  */
        {true,   6,  7,  9},   /* 19 отгрузка ← [6,7,9]   */
        {true,  10,  2, 16},   /* 20 отгрузка ← [10,2,16] */
    };

    SimState sim = {0};
    sim.name            = "SIM5";
    sim.sense_at_charge = true;

    Stacker fsm;
    stacker_setup(&fsm, &sim);
    print_tick(0, &sim, &fsm);

    for (int t = 1; t <= 3; t++) sim_step(t, &sim, &fsm);

    int  task_idx   = 0;
    int  ack_count  = 0;
    int  done_count = 0;
    bool submitted  = false;

    for (int t = 4; t <= 3000; t++) {
        /* Подача следующего задания, когда FSM свободен */
        if (!fsm.busy && !submitted && task_idx < 20) {
            sim.task_type       = tasks[task_idx].type;
            sim.task_stack_no   = tasks[task_idx].stack;
            sim.task_row_no     = tasks[task_idx].row;
            sim.task_section_no = tasks[task_idx].section;
            sim.task_valid      = true;
            submitted           = true;
            printf("  >> WMS: задание %2d/20 type=%d [%u,%u,%u] %s\n",
                   task_idx + 1, (int)tasks[task_idx].type,
                   tasks[task_idx].stack, tasks[task_idx].row,
                   tasks[task_idx].section,
                   tasks[task_idx].type ? "отгрузка" : "приёмка");
        }

        /* Снятие task_valid после подтверждения */
        if (sim.ack_seen && sim.task_valid) {
            sim.task_valid = false;
            sim.ack_seen   = false;
            ack_count++;
        }

        sim_step(t, &sim, &fsm);

        if (sim.done_seen) {
            done_count++;
            printf("  >> DONE %2d/20 на такте %d\n", done_count, t);
            sim.done_seen = false;
            submitted     = false;
            task_idx++;

            if (done_count == 20) {
                /* Дополнительный такт для перехода Completing -> Idle */
                sim_step(t + 1, &sim, &fsm);
                break;
            }
        }
    }

    int remaining = 0;
    for (int st = 0; st < 12; st++)
        for (int ro = 0; ro < 11; ro++)
            for (int se = 0; se < 21; se++)
                if (sim.cells[st][ro][se]) remaining++;

    CHECK(&sim, ack_count  == 20, "все 20 заданий подтверждены (cmd_ack)");
    CHECK(&sim, done_count == 20, "все 20 заданий выполнены (cmd_done)");
    CHECK(&sim, !fsm.busy,        "busy=0 после последнего задания");
    CHECK(&sim, !sim.sense_loaded, "вилы свободны после всех заданий");
    CHECK(&sim, fsm.stacker.movement_controller1.state ==
                STACKER_MOVEMENT_CONTROLLER_MOVEMENT_IDLE,
          "MovementController в Idle после всех заданий");
    CHECK(&sim, remaining == 0,   "склад пуст — все 10 паллетов приняты и отгружены");
    return sim.errors;
}

/* ════════════════════════════════════════════════════════════════════════════
   main
   ════════════════════════════════════════════════════════════════════════════ */

int main(void) {
    int total = 0;
    total += run_sim1();
    total += run_sim2();
    total += run_sim3();
    total += run_sim4();
    total += run_sim5();

    printf("\n═══════════════════════════════════════════════════════════════════\n");
    if (total == 0)
        printf("Итог: все проверки PASS\n");
    else
        printf("Итог: %d проверок FAIL\n", total);
    printf("═══════════════════════════════════════════════════════════════════\n");

    return total == 0 ? 0 : 1;
}
