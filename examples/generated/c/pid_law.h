#ifndef PID_LAW_H__
#define PID_LAW_H__
#include <stdint.h>
#include <stdbool.h>

typedef struct PidLaw PidLaw;

typedef struct PidState {
    double kp;
    double ki;
    double kd;
    double ts;
    double out_min;
    double out_max;
    double i_acc;
    double err_prev;
    double output;
} PidState;

struct PidLaw {
    double ctrl;
    uint8_t hold;
    PidState loop_pid;
    double meas;
    double target;
    enum {
        PID_LAW_INIT,
        PID_LAW_RUN,
        PID_LAW_END
    } state;
};

void PidLaw_init(PidLaw *main);
void PidLaw_tick(PidLaw *main);
void PidLaw_reset(PidLaw *main);
bool PidLaw_is_done(const PidLaw *main);
#endif
