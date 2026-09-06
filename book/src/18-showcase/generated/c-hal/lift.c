#include "lift.h"
#include <assert.h>
#include <math.h>
#define CONST_LIFT_DWELL_TICKS 3
void Lift_init(Lift *model) {
    assert(0 != model);
    model->state = LIFT_INIT;
    model->doors = 0;
    model->dwell = 0;
    model->moving = 0;
}

void Lift_tick(Lift *model) {
    assert(0 != model);
    assert(model->moving == 0 || model->doors == 0);
    if (model->state == LIFT_INIT) {
        model->moving = 0;
        (*model->write_bit)(LIFT_PORT_MOTOR_UP, 0, 0, model->userdata);
        (*model->write_bit)(LIFT_PORT_MOTOR_DOWN, 0, 0, model->userdata);
        (*model->write_bit)(LIFT_PORT_BRAKE, 0, 1, model->userdata);
        model->doors = 0;
        (*model->write_bit)(LIFT_PORT_DOORS_OPEN, 0, 0, model->userdata);
        model->state = LIFT_WAITING;
    }
    switch (model->state) {
        case LIFT_BOARDING: {
            model->dwell = model->dwell + 1;
            if (model->dwell >= CONST_LIFT_DWELL_TICKS) {
                model->doors = 0;
                (*model->write_bit)(LIFT_PORT_DOORS_OPEN, 0, 0, model->userdata);
                model->state = LIFT_LEAVING;
                break;
            }
            break;
        }
        case LIFT_GOING_DOWN: {
            (*model->write_numeric)(LIFT_PORT_DISPLAY, 0, (*model->read_numeric)(LIFT_PORT_AT_FLOOR, 0, model->userdata), model->userdata);
            if ((*model->read_numeric)(LIFT_PORT_AT_FLOOR, 0, model->userdata) <= (*model->read_numeric)(LIFT_PORT_CALL, 0, model->userdata)) {
                model->moving = 0;
                (*model->write_bit)(LIFT_PORT_MOTOR_UP, 0, 0, model->userdata);
                (*model->write_bit)(LIFT_PORT_MOTOR_DOWN, 0, 0, model->userdata);
                (*model->write_bit)(LIFT_PORT_BRAKE, 0, 1, model->userdata);
                model->state = LIFT_STOPPING;
                break;
            }
            break;
        }
        case LIFT_GOING_UP: {
            (*model->write_numeric)(LIFT_PORT_DISPLAY, 0, (*model->read_numeric)(LIFT_PORT_AT_FLOOR, 0, model->userdata), model->userdata);
            if ((*model->read_numeric)(LIFT_PORT_AT_FLOOR, 0, model->userdata) >= (*model->read_numeric)(LIFT_PORT_CALL, 0, model->userdata)) {
                model->moving = 0;
                (*model->write_bit)(LIFT_PORT_MOTOR_UP, 0, 0, model->userdata);
                (*model->write_bit)(LIFT_PORT_MOTOR_DOWN, 0, 0, model->userdata);
                (*model->write_bit)(LIFT_PORT_BRAKE, 0, 1, model->userdata);
                model->state = LIFT_STOPPING;
                break;
            }
            break;
        }
        case LIFT_LEAVING: {
            model->moving = 0;
            (*model->write_bit)(LIFT_PORT_MOTOR_UP, 0, 0, model->userdata);
            (*model->write_bit)(LIFT_PORT_MOTOR_DOWN, 0, 0, model->userdata);
            (*model->write_bit)(LIFT_PORT_BRAKE, 0, 1, model->userdata);
            model->doors = 0;
            (*model->write_bit)(LIFT_PORT_DOORS_OPEN, 0, 0, model->userdata);
            model->state = LIFT_WAITING;
            break;
        }
        case LIFT_STOPPING: {
            model->doors = 1;
            (*model->write_bit)(LIFT_PORT_DOORS_OPEN, 0, 1, model->userdata);
            model->dwell = 0;
            model->state = LIFT_BOARDING;
            break;
        }
        case LIFT_WAITING: {
            (*model->write_numeric)(LIFT_PORT_DISPLAY, 0, (*model->read_numeric)(LIFT_PORT_AT_FLOOR, 0, model->userdata), model->userdata);
            if ((*model->read_numeric)(LIFT_PORT_CALL, 0, model->userdata) == (*model->read_numeric)(LIFT_PORT_AT_FLOOR, 0, model->userdata)) {
                model->doors = 1;
                (*model->write_bit)(LIFT_PORT_DOORS_OPEN, 0, 1, model->userdata);
                model->dwell = 0;
                model->state = LIFT_BOARDING;
                break;
            }
            if ((*model->read_numeric)(LIFT_PORT_CALL, 0, model->userdata) > (*model->read_numeric)(LIFT_PORT_AT_FLOOR, 0, model->userdata)) {
                model->moving = 1;
                (*model->write_bit)(LIFT_PORT_BRAKE, 0, 0, model->userdata);
                (*model->write_bit)(LIFT_PORT_MOTOR_UP, 0, 1, model->userdata);
                model->state = LIFT_GOING_UP;
                break;
            }
            if ((*model->read_numeric)(LIFT_PORT_CALL, 0, model->userdata) > 0 && (*model->read_numeric)(LIFT_PORT_CALL, 0, model->userdata) < (*model->read_numeric)(LIFT_PORT_AT_FLOOR, 0, model->userdata)) {
                model->moving = 1;
                (*model->write_bit)(LIFT_PORT_BRAKE, 0, 0, model->userdata);
                (*model->write_bit)(LIFT_PORT_MOTOR_DOWN, 0, 1, model->userdata);
                model->state = LIFT_GOING_DOWN;
                break;
            }
            break;
        }
        case LIFT_END: {
            break;
        }
        default: break;
    }
}

void Lift_reset(Lift *model) {
    Lift_init(model);
}

bool Lift_is_done(const Lift *model) {
    return model->state == LIFT_END;
}

