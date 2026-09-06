#include "pid_regulator.h"
#include <assert.h>
#include <math.h>
static void PidRegulatorPid_init(PidRegulatorPid *model);
static void PidRegulatorPid_tick(PidRegulatorPid *model, PidRegulator *main);
static bool PidRegulatorPid_is_done(const PidRegulatorPid *model);

void PidRegulatorPid_init(PidRegulatorPid *model) {
    assert(0 != model);
    model->state = PID_REGULATOR_PID_INIT;
    model->ctrl = 0.0;
    model->deriv = 0.0;
    model->eps = 0.125;
    model->err = 0.0;
    model->err_prev = 0.0;
    model->i_acc = 0.0;
    model->imax = 32.0;
    model->kd = 0.25;
    model->ki = 0.0625;
    model->kp = 0.5;
    model->kplant = 0.5;
    model->meas = 0.0;
    model->neg_imax = -32.0;
    model->target = 8.0;
}

void PidRegulatorPid_tick(PidRegulatorPid *model, PidRegulator *main) {
    assert(0 != model);
    assert(0 != main);
    if (model->state == PID_REGULATOR_PID_INIT) {
        model->state = PID_REGULATOR_PID_CONTROL;
    }
    switch (model->state) {
        case PID_REGULATOR_PID_CONTROL: {
            model->err = model->target - model->meas;
            model->i_acc = model->i_acc + model->err;
            if (model->i_acc > model->imax) {
                model->i_acc = model->imax;
            }
            if (model->i_acc < model->neg_imax) {
                model->i_acc = model->neg_imax;
            }
            model->deriv = model->err - model->err_prev;
            model->ctrl = model->kp * model->err + model->ki * model->i_acc + model->kd * model->deriv;
            model->meas = model->meas + model->kplant * model->ctrl;
            model->err_prev = model->err;
            if (model->err < model->eps) {
                model->state = PID_REGULATOR_PID_SETTLED;
                break;
            }
            break;
        }
        case PID_REGULATOR_PID_DONE: {
            (*main->write_bit)(PID_REGULATOR_PID_PORT_READY, 0, 1, main->userdata);
            break;
        }
        case PID_REGULATOR_PID_SETTLED: {
            model->meas = model->target;
            model->state = PID_REGULATOR_PID_DONE;
            break;
        }
        case PID_REGULATOR_PID_END: {
            break;
        }
        default: break;
    }
}

void PidRegulatorPid_reset(PidRegulatorPid *model) {
    PidRegulatorPid_init(model);
}

bool PidRegulatorPid_is_done(const PidRegulatorPid *model) {
    return model->state == PID_REGULATOR_PID_END;
}

void PidRegulator_init(PidRegulator *model) {
    assert(0 != model);
    model->state = PID_REGULATOR_INIT;
    PidRegulatorPid_init(&model->main);
}

void PidRegulator_tick(PidRegulator *model) {
    assert(0 != model);
    if (model->state == PID_REGULATOR_INIT) {
        model->state = PID_REGULATOR_MAIN;
    }
    switch (model->state) {
        case PID_REGULATOR_MAIN: {
            PidRegulatorPid_tick(&model->main, model);
            if (PidRegulatorPid_is_done(&model->main)) {
                model->state = PID_REGULATOR_END;
                break;
            }
            break;
        }
        case PID_REGULATOR_END: {
            break;
        }
        default: break;
    }
}

void PidRegulator_reset(PidRegulator *model) {
    PidRegulator_init(model);
}

bool PidRegulator_is_done(const PidRegulator *model) {
    return model->state == PID_REGULATOR_END;
}

