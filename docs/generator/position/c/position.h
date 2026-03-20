#pragma once
#ifndef POSITION_H__
#define POSITION_H__

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

struct Port {
  void *userdata;
  void (*port_bit_write)(int address, int bit, bool value, void *userdata);
  bool (*port_bit_read)(int address, int bit, void *userdata);
  void (*port_int_write)(int address, int value, void *userdata);
  int (*port_int_read)(int address, void *userdata);
  void (*port_real_write)(int address, float value, void *userdata);
  float (*port_real_read)(int address, void *userdata);
};

#define write_bit_port(port, address, bit, value)                              \
  {                                                                            \
    if ((port).port_bit_write != 0)                                            \
      (port).port_bit_write(address, bit, value, (port).userdata);             \
  }
#define write_int_port(port, address, value)                                   \
  {                                                                            \
    if ((port).port_int_write != 0)                                            \
      (port).port_int_write(address, value, (port).userdata);                  \
  }

#define debug(message) printf("%s\n", message)

#define read_int_port(port, address, def)                                      \
  (((port).port_int_read != 0)                                                 \
       ? (port).port_int_read(address, (port).userdata)                        \
       : def)
#define read_real_port(port, address, def)                                     \
  (((port).port_real_read != 0)                                                \
       ? (port).port_real_read(address, (port).userdata)                       \
       : def)
#define read_bit_port(port, address, bit, def)                                 \
  (((port).port_bit_read != 0)                                                 \
       ? (port).port_bit_read(address, bit, (port).userdata)                   \
       : def)

struct Main {
  struct {
    enum {
      MAIN_COMMAND_INIT,
      MAIN_COMMAND_READING,
      MAIN_COMMAND_SAVING,
      MAIN_COMMAND_DECODE,
      MAIN_COMMAND_PUSHING_UP_CODE,
      MAIN_COMMAND_PUSHING_DOWN_CODE,
      MAIN_COMMAND_ROTATE_LEFT_CODE,
      MAIN_COMMAND_ROTATE_RIGHT_CODE,
      MAIN_COMMAND_MOVING_UP_CODE,
      MAIN_COMMAND_MOVING_DOWN_CODE,
      MAIN_COMMAND_MOVING_SHIFT_LEFT_CODE,
      MAIN_COMMAND_MOVING_SHIFT_RIGHT_CODE,
      MAIN_COMMAND_UNKNOWN_CODE,
      MAIN_COMMAND_RESET_CODE,
      MAIN_COMMAND_COMPLETE_CODE,
      MAIN_COMMAND_EMERGENCY,
      MAIN_COMMAND_END,
    } state;
    int32_t code;
    double next_angle;
    double next_angl_up;
    double next_angl_down;
    int32_t next_point;
    int32_t next_level;
  } command;
  struct {
    struct {
      enum {
        MAIN_CONTROLLER_DETECT_ANGLE_INIT,
        MAIN_CONTROLLER_DETECT_ANGLE_START,
        MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_45,
        MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_90,
        MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_135,
        MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_180,
        MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_225,
        MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_270,
        MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_315,
        MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_360,
        MAIN_CONTROLLER_DETECT_ANGLE_END,
      } state;
    } detect_angle;
    struct {
      enum {
        MAIN_CONTROLLER_DETECT_POINT_INIT,
        MAIN_CONTROLLER_DETECT_POINT_START,
        MAIN_CONTROLLER_DETECT_POINT_UP,
        MAIN_CONTROLLER_DETECT_POINT_DOWN,
        MAIN_CONTROLLER_DETECT_POINT_LEFT,
        MAIN_CONTROLLER_DETECT_POINT_RIGHT,
        MAIN_CONTROLLER_DETECT_POINT_COMPROMISE,
        MAIN_CONTROLLER_DETECT_POINT_END,
      } state;
      int32_t direct;
    } detect_point;
    struct {
      enum {
        MAIN_CONTROLLER_DETECT_LINE_INIT,
        MAIN_CONTROLLER_DETECT_LINE_START,
        MAIN_CONTROLLER_DETECT_LINE_UP,
        MAIN_CONTROLLER_DETECT_LINE_LEFT,
        MAIN_CONTROLLER_DETECT_LINE_BOTTOM,
        MAIN_CONTROLLER_DETECT_LINE_RIGHT,
        MAIN_CONTROLLER_DETECT_LINE_DETECT_WALL,
        MAIN_CONTROLLER_DETECT_LINE_WALL_LEFT,
        MAIN_CONTROLLER_DETECT_LINE_WALL_RIGHT,
        MAIN_CONTROLLER_DETECT_LINE_WALL_TOP,
        MAIN_CONTROLLER_DETECT_LINE_WALL_BOTTOM,
        MAIN_CONTROLLER_DETECT_LINE_END,
      } state;
      int32_t it;
      int32_t step;
    } detect_line;
    enum {
      MAIN_CONTROLLER_INIT,
      MAIN_CONTROLLER_START,
      MAIN_CONTROLLER_DETECT_ANGLE,
      MAIN_CONTROLLER_DETECT_LINE,
      MAIN_CONTROLLER_DETECT_POINT,
      MAIN_CONTROLLER_RESET,
      MAIN_CONTROLLER_WAITING,
      MAIN_CONTROLLER_EXECUTING_COMMAND,
      MAIN_CONTROLLER_ERROR,
      MAIN_CONTROLLER_END,
    } state;
    int32_t raise;
  } controller;
  int watchdog;
  bool reset;
  enum { MAIN_IMPLEMENT_INIT, MAIN_IMPLEMENT_TICK, MAIN_IMPLEMENT_END } state;
  struct Port port;
};

void Main_init(struct Main *main);
void Main_tick(struct Main *main);
bool Main_finished(const struct Main *main);
void Main_reset(struct Main *main);

#endif /* POSITION_H__ */
