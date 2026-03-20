#include <assert.h>
#include "position.h"

#define PORT_ADDRESS_ANGLE 0
#define PORT_ADDRESS_POINT 1
#define PORT_ADDRESS_ON_LINE 2

#define PORT_ADDRESS_DISTANCE_TOP 10
#define PORT_ADDRESS_DISTANCE_BOTTOM 11
#define PORT_ADDRESS_DISTANCE_LEFT 12
#define PORT_ADDRESS_DISTANCE_RIGHT 13

#define PORT_ADDRESS_MACHINE_GAS 20
#define PORT_ADDRESS_MACHINE_BACK 21
#define PORT_ADDRESS_MACHINE_ROT_LEFT 22
#define PORT_ADDRESS_MACHINE_ROT_RIGHT 23
#define PORT_ADDRESS_MACHINE_SHIFT_LEFT 24
#define PORT_ADDRESS_MACHINE_SHIFT_RIGHT 25

#define PORT_ADDRESS_PUSH_UP 26
#define PORT_ADDRESS_PUSH_DOWN 27
#define PORT_ADDRESS_PUSH_LEVEL 28
#define PORT_ADDRESS_BUSY 30

#define PORT_ADDRESS_ERROR 31

#define PORT_ADDRESS_COMMAND_READY 40
#define PORT_ADDRESS_COMMAND_CODE 41
#define PORT_ADDRESS_COMMAND_EXECUTING 42
#define PORT_ADDRESS_COMMAND_RESET 42
#define PORT_ADDRESS_COMMAND_POINT 43
#define PORT_ADDRESS_COMMAND_LEVEL 44

// model: Detect_Line
__attribute__((always_inline)) static void
Main_Controller_Detect_Line_reset(struct Main *main) {
  assert(main != 0);
  main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_INIT;
}

__attribute__((always_inline)) static void
Main_Controller_Detect_Line_init(struct Main *main) {
  assert(main != 0);
  Main_Controller_Detect_Line_reset(main);
}

__attribute__((always_inline)) static void
Main_Controller_Detect_Line_tick(struct Main *main) {
  assert(main != 0);
  const float distance_top =
      read_real_port(main->port, PORT_ADDRESS_DISTANCE_TOP, 0.);
  const float distance_bottom =
      read_real_port(main->port, PORT_ADDRESS_DISTANCE_BOTTOM, 0.);
  const float distance_left =
      read_real_port(main->port, PORT_ADDRESS_DISTANCE_LEFT, 0.);
  const float distance_right =
      read_real_port(main->port, PORT_ADDRESS_DISTANCE_RIGHT, 0.);
  const bool on_line =
      read_bit_port(main->port, PORT_ADDRESS_ON_LINE, 0, false);
  const int point = read_int_port(main->port, PORT_ADDRESS_POINT, 0);
  switch (main->controller.detect_line.state) {
  case MAIN_CONTROLLER_DETECT_LINE_INIT: {
    main->controller.detect_line.it = 0;
    main->controller.detect_line.step = 1;
    main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_START;
    break;
  }
  case MAIN_CONTROLLER_DETECT_LINE_START: {
    if (on_line || point > 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_END;
    }
    if (distance_top >= 0 || distance_bottom >= 0 || distance_left >= 0 ||
        distance_right >= 0) {
      main->controller.detect_line.state =
          MAIN_CONTROLLER_DETECT_LINE_DETECT_WALL;
    }
    if (true) {
      {
        main->controller.detect_line.it = main->controller.detect_line.it + 1;
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_GAS, true);
      }
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_UP;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_LINE_UP: {
    if (on_line || point > 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_END;
    }
    if (distance_top >= 0 || distance_bottom >= 0 || distance_left >= 0 ||
        distance_right >= 0) {
      main->controller.detect_line.state =
          MAIN_CONTROLLER_DETECT_LINE_DETECT_WALL;
    }
    if (main->controller.detect_line.it >= main->controller.detect_line.step) {
      {
        main->controller.detect_line.it = main->controller.detect_line.it + 1;
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_LEFT, true);
      }
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_LEFT;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_LINE_LEFT: {
    if (on_line || point > 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_END;
    }
    if (distance_top >= 0 || distance_bottom >= 0 || distance_left >= 0 ||
        distance_right >= 0) {
      main->controller.detect_line.state =
          MAIN_CONTROLLER_DETECT_LINE_DETECT_WALL;
    }
    if (main->controller.detect_line.it >= main->controller.detect_line.step) {
      {
        main->controller.detect_line.it = main->controller.detect_line.it + 1;
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_BACK, true);
      }
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_BOTTOM;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_LINE_BOTTOM: {
    if (on_line || point > 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_END;
    }
    if (distance_top >= 0 || distance_bottom >= 0 || distance_left >= 0 ||
        distance_right >= 0) {
      main->controller.detect_line.state =
          MAIN_CONTROLLER_DETECT_LINE_DETECT_WALL;
    }
    if (main->controller.detect_line.it >= main->controller.detect_line.step) {
      {
        main->controller.detect_line.it = main->controller.detect_line.it + 1;
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_RIGHT, true);
      }
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_RIGHT;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_LINE_RIGHT: {
    if (on_line || point > 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_END;
    }
    if (distance_top >= 0 || distance_bottom >= 0 || distance_left >= 0 ||
        distance_right >= 0) {
      main->controller.detect_line.state =
          MAIN_CONTROLLER_DETECT_LINE_DETECT_WALL;
    }
    if (main->controller.detect_line.it >= main->controller.detect_line.step) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_START;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_LINE_DETECT_WALL: {
    if (distance_left >= 0) {
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_RIGHT, true);
      }
      main->controller.detect_line.state =
          MAIN_CONTROLLER_DETECT_LINE_WALL_LEFT;
    }
    if (distance_right >= 0) {
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_LEFT, true);
      }
      main->controller.detect_line.state =
          MAIN_CONTROLLER_DETECT_LINE_WALL_RIGHT;
    }
    if (distance_top >= 0) {
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_BACK, true);
      }
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_WALL_TOP;
    }
    if (distance_bottom >= 0) {
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_GAS, true);
      }
      main->controller.detect_line.state =
          MAIN_CONTROLLER_DETECT_LINE_WALL_BOTTOM;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_LINE_WALL_LEFT: {
    if (distance_top >= 0 || distance_bottom >= 0 || distance_right >= 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_START;
    }
    if ((on_line || point) > 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_END;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_LINE_WALL_RIGHT: {
    if (distance_top >= 0 || distance_bottom >= 0 || distance_left >= 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_START;
    }
    if ((on_line || point) > 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_END;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_LINE_WALL_TOP: {
    if (distance_left >= 0 || distance_bottom >= 0 || distance_right >= 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_START;
    }
    if ((on_line || point) > 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_END;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_LINE_WALL_BOTTOM: {
    if (distance_top >= 0 || distance_left >= 0 || distance_right >= 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_START;
    }
    if ((on_line || point) > 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_END;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_LINE_END: {
    break;
  }
  }
}
// model: Detect_Angle
__attribute__((always_inline)) static void
Main_Controller_Detect_Angle_reset(struct Main *main) {
  assert(main != 0);
  main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_INIT;
}

__attribute__((always_inline)) static void
Main_Controller_Detect_Angle_init(struct Main *main) {
  assert(main != 0);
  Main_Controller_Detect_Angle_reset(main);
}

__attribute__((always_inline)) static void
Main_Controller_Detect_Angle_tick(struct Main *main) {
  assert(main != 0);
  const float angle = read_real_port(main->port, PORT_ADDRESS_ANGLE, 0.);
  const float distance_top =
      read_real_port(main->port, PORT_ADDRESS_DISTANCE_TOP, 0.);
  const float distance_bottom =
      read_real_port(main->port, PORT_ADDRESS_DISTANCE_BOTTOM, 0.);
  switch (main->controller.detect_angle.state) {
  case MAIN_CONTROLLER_DETECT_ANGLE_INIT: {
    main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_START;
    break;
  }
  case MAIN_CONTROLLER_DETECT_ANGLE_START: {
    if (angle == 0 || angle == 90 || angle == 180 || angle == 270) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_END;
    }
    if (angle > 0 && angle < 45) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_45;
    }
    if (angle >= 45 && angle < 90) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_90;
    }
    if (angle > 90 && angle < 135) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_135;
    }
    if (angle >= 135 && angle < 180) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_180;
    }
    if (angle > 180 && angle < 225) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_225;
    }
    if (angle >= 225 && angle < 270) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_270;
    }
    if (angle > 270 && angle < 315) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_315;
    }
    if (angle >= 315 && angle < 360) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_360;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_45: {
    if ((distance_top >= 0 && distance_top <= 2) ||
        (distance_bottom >= 0 && distance_bottom <= 2)) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_90;
    }
    if (angle > 90 && angle < 135) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_135;
    }
    if (angle >= 135 && angle < 180) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_180;
    }
    if (angle > 180 && angle < 225) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_225;
    }
    if (angle >= 225 && angle < 270) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_270;
    }
    if (angle > 270 && angle < 315) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_315;
    }
    if (angle >= 315 && angle < 360) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_360;
    }
    if (angle == 0) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_END;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_90: {
    if ((distance_top >= 0 && distance_top <= 2) ||
        (distance_bottom >= 0 && distance_bottom <= 2)) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_45;
    }
    if (angle > 90 && angle < 135) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_135;
    }
    if (angle >= 135 && angle < 180) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_180;
    }
    if (angle > 180 && angle < 225) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_225;
    }
    if (angle >= 225 && angle < 270) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_270;
    }
    if (angle > 270 && angle < 315) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_315;
    }
    if (angle >= 315 && angle < 360) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_360;
    }
    if (angle == 90) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_END;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_135: {
    if ((distance_top >= 0 && distance_top <= 2) ||
        (distance_bottom >= 0 && distance_bottom <= 2)) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_180;
    }
    if (angle > 0 && angle < 45) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_45;
    }
    if (angle >= 45 && angle < 90) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_90;
    }
    if (angle > 180 && angle < 225) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_225;
    }
    if (angle >= 225 && angle < 270) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_270;
    }
    if (angle > 270 && angle < 315) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_315;
    }
    if (angle >= 315 && angle < 360) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_360;
    }
    if (angle == 90) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_END;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_180: {
    if ((distance_top >= 0 && distance_top <= 2) ||
        (distance_bottom >= 0 && distance_bottom <= 2)) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_135;
    }
    if (angle > 0 && angle < 45) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_45;
    }
    if (angle >= 45 && angle < 90) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_90;
    }
    if (angle > 180 && angle < 225) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_225;
    }
    if (angle >= 225 && angle < 270) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_270;
    }
    if (angle > 270 && angle < 315) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_315;
    }
    if (angle >= 315 && angle < 360) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_360;
    }
    if (angle == 180) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_END;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_225: {
    if ((distance_top >= 0 && distance_top <= 2) ||
        (distance_bottom >= 0 && distance_bottom <= 2)) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_270;
    }
    if (angle > 0 && angle < 45) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_45;
    }
    if (angle >= 45 && angle < 90) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_90;
    }
    if (angle > 90 && angle < 135) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_135;
    }
    if (angle >= 135 && angle < 180) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_180;
    }
    if (angle > 270 && angle < 315) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_315;
    }
    if (angle >= 315 && angle < 360) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_360;
    }
    if (angle == 180) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_END;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_270: {
    if ((distance_top >= 0 && distance_top <= 2) ||
        (distance_bottom >= 0 && distance_bottom <= 2)) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_225;
    }
    if (angle > 0 && angle < 45) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_45;
    }
    if (angle >= 45 && angle < 90) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_90;
    }
    if (angle > 90 && angle < 135) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_135;
    }
    if (angle >= 135 && angle < 180) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_180;
    }
    if (angle > 270 && angle < 315) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_315;
    }
    if (angle >= 315 && angle < 360) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_360;
    }
    if (angle == 180) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_END;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_315: {
    if ((distance_top >= 0 && distance_top <= 2) ||
        (distance_bottom >= 0 && distance_bottom <= 2)) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_360;
    }
    if (angle > 0 && angle < 45) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_45;
    }
    if (angle >= 45 && angle < 90) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_90;
    }
    if (angle > 90 && angle < 135) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_135;
    }
    if (angle >= 135 && angle < 180) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_180;
    }
    if (angle > 180 && angle < 225) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_225;
    }
    if (angle >= 225 && angle < 270) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_270;
    }
    if (angle == 270) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_END;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_360: {
    if ((distance_top >= 0 && distance_top <= 2) ||
        (distance_bottom >= 0 && distance_bottom <= 2)) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_315;
    }
    if (angle > 0 && angle < 45) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_45;
    }
    if (angle >= 45 && angle < 90) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_90;
    }
    if (angle > 90 && angle < 135) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_135;
    }
    if (angle >= 135 && angle < 180) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_180;
    }
    if (angle > 180 && angle < 225) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_225;
    }
    if (angle >= 225 && angle < 270) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      main->controller.detect_angle.state =
          MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_270;
    }
    if (angle == 0) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_END;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_ANGLE_END: {
    break;
  }
  }
}

__attribute__((always_inline)) static bool
Main_Controller_Detect_Angle_finished(const struct Main *main) {
  assert(main != 0);
  return main->controller.detect_angle.state ==
         MAIN_CONTROLLER_DETECT_ANGLE_END;
}

// model: Detect_Point
__attribute__((always_inline)) static void
Main_Controller_Detect_Point_reset(struct Main *main) {
  assert(main != 0);
  main->controller.detect_point.state = MAIN_CONTROLLER_DETECT_POINT_INIT;
}

__attribute__((always_inline)) static void
Main_Controller_Detect_Point_init(struct Main *main) {
  assert(main != 0);
  Main_Controller_Detect_Point_reset(main);
}

__attribute__((always_inline)) static void
Main_Controller_Detect_Point_tick(struct Main *main) {
  assert(main != 0);
  const bool on_line =
      read_bit_port(main->port, PORT_ADDRESS_ON_LINE, 0, false);
  const int point = read_int_port(main->port, PORT_ADDRESS_POINT, 0);

  switch (main->controller.detect_point.state) {
  case MAIN_CONTROLLER_DETECT_POINT_INIT: {
    main->controller.detect_point.direct = -1;
    main->controller.detect_point.state = MAIN_CONTROLLER_DETECT_POINT_START;
    break;
  }
  case MAIN_CONTROLLER_DETECT_POINT_START: {
    if (point > 0) {
      main->controller.detect_point.state = MAIN_CONTROLLER_DETECT_POINT_END;
    }
    if (true) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_GAS, true);
      main->controller.detect_point.state = MAIN_CONTROLLER_DETECT_POINT_UP;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_POINT_UP: {
    if (point > 0) {
      {
        if (main->controller.detect_point.direct == 1) {
          write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_GAS, true);
        } else if (main->controller.detect_point.direct == 2) {
          write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_RIGHT, true);
        } else if (main->controller.detect_point.direct == 3) {
          write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_LEFT, true);
        }
      }
      main->controller.detect_point.state =
          MAIN_CONTROLLER_DETECT_POINT_COMPROMISE;
    }
    if (!on_line && point < 0) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_BACK, true);
      main->controller.detect_point.state = MAIN_CONTROLLER_DETECT_POINT_DOWN;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_POINT_DOWN: {
    if (point > 0) {
      {
        if (main->controller.detect_point.direct == 1) {
          write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_GAS, true);
        } else if (main->controller.detect_point.direct == 2) {
          write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_RIGHT, true);
        } else if (main->controller.detect_point.direct == 3) {
          write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_LEFT, true);
        }
      }
      main->controller.detect_point.state =
          MAIN_CONTROLLER_DETECT_POINT_COMPROMISE;
    }
    if (!on_line && point < 0) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_LEFT, true);
      main->controller.detect_point.state = MAIN_CONTROLLER_DETECT_POINT_LEFT;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_POINT_LEFT: {
    if (point > 0) {
      {
        if (main->controller.detect_point.direct == 1) {
          write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_GAS, true);
        } else if (main->controller.detect_point.direct == 2) {
          write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_RIGHT, true);
        } else if (main->controller.detect_point.direct == 3) {
          write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_LEFT, true);
        }
      }
      main->controller.detect_point.state =
          MAIN_CONTROLLER_DETECT_POINT_COMPROMISE;
    }
    if (!on_line && point < 0) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_RIGHT, true);
      main->controller.detect_point.state = MAIN_CONTROLLER_DETECT_POINT_RIGHT;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_POINT_RIGHT: {
    if (point > 0) {
      {
        if (main->controller.detect_point.direct == 1) {
          write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_GAS, true);
        } else if (main->controller.detect_point.direct == 2) {
          write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_RIGHT, true);
        } else if (main->controller.detect_point.direct == 3) {
          write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_LEFT, true);
        }
      }
      main->controller.detect_point.state =
          MAIN_CONTROLLER_DETECT_POINT_COMPROMISE;
    }
    if (!on_line && point < 0) {
      main->controller.detect_point.state = MAIN_CONTROLLER_DETECT_POINT_START;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_POINT_COMPROMISE: {
    if (true) {
      main->controller.detect_point.state = MAIN_CONTROLLER_DETECT_POINT_END;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_POINT_END: {
    break;
  }
  }
}

__attribute__((always_inline)) static bool
Main_Controller_Detect_Point_finished(const struct Main *main) {
  assert(main != 0);
  return main->controller.detect_point.state ==
         MAIN_CONTROLLER_DETECT_POINT_END;
}

// model: Command
__attribute__((always_inline)) static void
Main_Command_reset(struct Main *main) {
  assert(main != 0);
  main->command.state = MAIN_COMMAND_INIT;
}
__attribute__((always_inline)) static void
Main_Command_init(struct Main *main) {
  assert(main != 0);
  Main_Command_reset(main);
}
__attribute__((always_inline)) static void
Main_Command_tick(struct Main *main) {
  assert(main != 0);
  const float angle = read_real_port(main->port, PORT_ADDRESS_ANGLE, 0.);
  const float distance_top =
      read_real_port(main->port, PORT_ADDRESS_DISTANCE_TOP, 0.);
  const float distance_bottom =
      read_real_port(main->port, PORT_ADDRESS_DISTANCE_BOTTOM, 0.);
  const float distance_left =
      read_real_port(main->port, PORT_ADDRESS_DISTANCE_LEFT, 0.);
  const float distance_right =
      read_real_port(main->port, PORT_ADDRESS_DISTANCE_RIGHT, 0.);
  const bool busy = read_bit_port(main->port, PORT_ADDRESS_BUSY, 0, false);
  const bool error = read_bit_port(main->port, PORT_ADDRESS_ERROR, 0, false);
  const bool command_ready =
      read_bit_port(main->port, PORT_ADDRESS_COMMAND_READY, 0, false);
  const int command_code =
      read_int_port(main->port, PORT_ADDRESS_COMMAND_CODE, -1);
  const int command_point =
      read_int_port(main->port, PORT_ADDRESS_COMMAND_POINT, -1);
  const bool push_level =
      read_bit_port(main->port, PORT_ADDRESS_PUSH_LEVEL, 0, false);
  const int point = read_int_port(main->port, PORT_ADDRESS_POINT, -1);
  switch (main->command.state) {
  case MAIN_COMMAND_INIT: {
    main->command.code = 0;
    main->command.next_angle = 0.0;
    main->command.next_angl_up = 0.0;
    main->command.next_angl_down = 0.0;
    main->command.next_point = 0;
    main->command.next_level = -1;
    main->command.state = MAIN_COMMAND_READING;
    break;
  }
  case MAIN_COMMAND_READING: {
    if (command_ready && !busy && !error) {
      {
        main->command.code = command_code;
        write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
        main->command.next_point = command_point;
      }
      main->command.state = MAIN_COMMAND_SAVING;
    }
    break;
  }
  case MAIN_COMMAND_SAVING: {
    if (true) {
      write_bit_port(main->port, 0, PORT_ADDRESS_BUSY, true);
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
      main->command.state = MAIN_COMMAND_DECODE;
    }
    break;
  }
  case MAIN_COMMAND_DECODE: {
    if (main->command.code == 0) {
      write_bit_port(main->port, 0, PORT_ADDRESS_BUSY, true);
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
        main->reset = true;
      }
      main->command.state = MAIN_COMMAND_RESET_CODE;
    }
    if (main->command.code == 1) {
      write_bit_port(main->port, 0, PORT_ADDRESS_BUSY, true);
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
      main->command.state = MAIN_COMMAND_COMPLETE_CODE;
    }
    if (main->command.code == 2) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
      main->command.state = MAIN_COMMAND_ROTATE_LEFT_CODE;
    }
    if (main->command.code == 3) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
      main->command.state = MAIN_COMMAND_ROTATE_RIGHT_CODE;
    }
    if (main->command.code == 4) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_GAS, true);
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
      main->command.state = MAIN_COMMAND_MOVING_UP_CODE;
    }
    if (main->command.code == 5) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_BACK, true);
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
      main->command.state = MAIN_COMMAND_MOVING_DOWN_CODE;
    }
    if (main->command.code == 6) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_LEFT, true);
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
      main->command.state = MAIN_COMMAND_MOVING_SHIFT_LEFT_CODE;
    }
    if (main->command.code == 7) {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_RIGHT, true);
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
      main->command.state = MAIN_COMMAND_MOVING_SHIFT_RIGHT_CODE;
    }
    if (main->command.code == 8) {
      write_bit_port(main->port, 0, PORT_ADDRESS_PUSH_UP, true);
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
      main->command.state = MAIN_COMMAND_PUSHING_UP_CODE;
    }
    if (main->command.code == 9) {
      write_bit_port(main->port, 0, PORT_ADDRESS_PUSH_DOWN, true);
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
      main->command.state = MAIN_COMMAND_PUSHING_DOWN_CODE;
    }
    if (true) {
      write_bit_port(main->port, 0, PORT_ADDRESS_BUSY, true);
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
      main->command.state = MAIN_COMMAND_UNKNOWN_CODE;
    }
    break;
  }
  case MAIN_COMMAND_PUSHING_UP_CODE: {
    if (main->command.next_level == push_level) {
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, false);
      main->command.state = MAIN_COMMAND_READING;
    }
    break;
  }
  case MAIN_COMMAND_PUSHING_DOWN_CODE: {
    if (0 == push_level) {
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, false);
      main->command.state = MAIN_COMMAND_READING;
    }
    break;
  }
  case MAIN_COMMAND_ROTATE_LEFT_CODE: {
    if (angle >= main->command.next_angl_up ||
        angle <= main->command.next_angl_down) {
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, false);
      main->command.state = MAIN_COMMAND_READING;
    }
    break;
  }
  case MAIN_COMMAND_ROTATE_RIGHT_CODE: {
    if (angle >= main->command.next_angl_up &&
        angle <= main->command.next_angl_down) {
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, false);
      main->command.state = MAIN_COMMAND_READING;
    }
    break;
  }
  case MAIN_COMMAND_MOVING_UP_CODE: {
    if (main->command.next_point == point) {
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, false);
      main->command.state = MAIN_COMMAND_READING;
    }
    if (distance_top >= 0 && distance_top <= 2) {
      write_bit_port(main->port, 0, PORT_ADDRESS_ERROR, true);
      main->command.state = MAIN_COMMAND_EMERGENCY;
    }
    break;
  }
  case MAIN_COMMAND_MOVING_DOWN_CODE: {
    if (main->command.next_point == point) {
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, false);
      main->command.state = MAIN_COMMAND_READING;
    }
    if (distance_bottom >= 0 && distance_bottom <= 2) {
      write_bit_port(main->port, 0, PORT_ADDRESS_ERROR, true);
      main->command.state = MAIN_COMMAND_EMERGENCY;
    }
    break;
  }
  case MAIN_COMMAND_MOVING_SHIFT_LEFT_CODE: {
    if (main->command.next_point == point) {
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, false);
      main->command.state = MAIN_COMMAND_READING;
    }
    if (distance_left >= 0 && distance_left <= 2) {
      write_bit_port(main->port, 0, PORT_ADDRESS_ERROR, true);
      main->command.state = MAIN_COMMAND_EMERGENCY;
    }
    break;
  }
  case MAIN_COMMAND_MOVING_SHIFT_RIGHT_CODE: {
    if (main->command.next_point == point) {
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, false);
      main->command.state = MAIN_COMMAND_READING;
    }
    if (distance_right >= 0 && distance_right <= 2) {
      write_bit_port(main->port, 0, PORT_ADDRESS_ERROR, true);
      main->command.state = MAIN_COMMAND_EMERGENCY;
    }
    break;
  }
  case MAIN_COMMAND_UNKNOWN_CODE: {
    if (true) {
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, false);
      main->command.state = MAIN_COMMAND_READING;
    }
    break;
  }
  case MAIN_COMMAND_RESET_CODE: {
    if (true) {
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, false);
      main->command.state = MAIN_COMMAND_READING;
    }
    break;
  }
  case MAIN_COMMAND_COMPLETE_CODE: {
    if (true) {
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, false);
      main->command.state = MAIN_COMMAND_READING;
    }
    break;
  }
  case MAIN_COMMAND_EMERGENCY: {
    if (true) {
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, false);
      main->command.state = MAIN_COMMAND_READING;
    }
    break;
  }
  case MAIN_COMMAND_END: {
    break;
  }
  }
}
__attribute__((always_inline)) static bool
Main_Command_finished(const struct Main *main) {
  assert(main != 0);
  return main->command.state == MAIN_COMMAND_END;
}
// model: Controller
__attribute__((always_inline)) static void
Main_Controller_reset(struct Main *main) {
  assert(main != 0);
  main->controller.state = MAIN_CONTROLLER_INIT;
}
__attribute__((always_inline)) static void
Main_Controller_init(struct Main *main) {
  assert(main != 0);
  Main_Controller_reset(main);
}
__attribute__((always_inline)) static void
Main_Controller_tick(struct Main *main) {
  assert(main != 0);
  const bool error = read_bit_port(main->port, PORT_ADDRESS_ERROR, 0, false);
  const bool command_executing =
      read_bit_port(main->port, PORT_ADDRESS_COMMAND_EXECUTING, 0, false);
  switch (main->controller.state) {
  case MAIN_CONTROLLER_INIT: {
    main->controller.raise = 0;
    main->controller.state = MAIN_CONTROLLER_START;
    break;
  }
  case MAIN_CONTROLLER_START: {
    if (true) {
      Main_Controller_Detect_Angle_init(main);
      main->controller.state = MAIN_CONTROLLER_DETECT_ANGLE;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_ANGLE: {
    Main_Controller_Detect_Angle_tick(main);
    if (main->controller.detect_angle.state ==
            MAIN_CONTROLLER_DETECT_ANGLE_END ||
        main->reset) {
      Main_Controller_Detect_Line_init(main);
      main->controller.state = MAIN_CONTROLLER_DETECT_LINE;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_LINE: {
    Main_Controller_Detect_Line_tick(main);
    if (main->controller.detect_line.state == MAIN_CONTROLLER_DETECT_LINE_END ||
        main->reset) {
      Main_Controller_Detect_Point_init(main);
      main->controller.state = MAIN_CONTROLLER_DETECT_POINT;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_POINT: {
    Main_Controller_Detect_Point_tick(main);
    if (main->controller.detect_point.state ==
            MAIN_CONTROLLER_DETECT_POINT_END ||
        main->reset) {
      write_bit_port(main->port, 0, PORT_ADDRESS_BUSY, true);
      main->controller.state = MAIN_CONTROLLER_RESET;
    }
    break;
  }
  case MAIN_CONTROLLER_RESET: {
    if (main->reset) {
      main->controller.state = MAIN_CONTROLLER_START;
    }
    if (error) {
      debug("Сработал Watchdog");
      main->controller.state = MAIN_CONTROLLER_ERROR;
    }
    if (true) {
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_BUSY, false);
        main->watchdog = 0;
      }
      main->controller.state = MAIN_CONTROLLER_WAITING;
    }
    break;
  }
  case MAIN_CONTROLLER_WAITING: {
    if (command_executing) {
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_BUSY, true);
        main->watchdog = 0;
      }
      main->controller.state = MAIN_CONTROLLER_EXECUTING_COMMAND;
    }
    break;
  }
  case MAIN_CONTROLLER_EXECUTING_COMMAND: {
    if (!command_executing) {
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_BUSY, false);
        main->watchdog = 0;
      }
      main->controller.state = MAIN_CONTROLLER_WAITING;
    }
    break;
  }
  case MAIN_CONTROLLER_ERROR: {
    if (main->reset) {
      main->controller.state = MAIN_CONTROLLER_START;
    }
    break;
  }
  case MAIN_CONTROLLER_END: {
    break;
  }
  }
}
__attribute__((always_inline)) static bool
Main_Controller_finished(const struct Main *main) {
  assert(main != 0);
  return main->controller.state == MAIN_CONTROLLER_END;
}
// model: Main
void Main_reset(struct Main *main) {
  assert(main != 0);
  main->state = MAIN_IMPLEMENT_INIT;
  Main_Command_reset(main);
  Main_Controller_reset(main);
}

void Main_init(struct Main *main) {
  assert(main != 0);
  Main_reset(main);
}

void Main_tick(struct Main *main) {
  assert(main != 0);
  switch (main->state) {
  case MAIN_IMPLEMENT_INIT: {
    Main_Command_init(main);
    Main_Controller_init(main);
    main->state = MAIN_IMPLEMENT_TICK;
    break;
  }
  case MAIN_IMPLEMENT_TICK: {
    Main_Command_tick(main);
    Main_Controller_tick(main);
    if (Main_Command_finished(main) && Main_Controller_finished(main)) {
      main->state = MAIN_IMPLEMENT_END;
    }
    break;
  }
  case MAIN_IMPLEMENT_END: {
    break;
  }
  }
}

bool Main_finished(const struct Main *main) {
  assert(main != 0);
  return main->state == MAIN_IMPLEMENT_END;
}
