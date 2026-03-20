#include "position.h"
#include <assert.h>

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

#define MAX_EXECUTING_TICKS 1500
#define ROUND_DELTA 0.1
#define ANGLE_DELTA 90.0

// model: Detect_Line
__attribute__((always_inline)) static void
Main_Controller_Detect_Line_reset(struct Main *main) {
  assert(main != 0);
  main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_INIT;
  main->controller.detect_line.it = 0;
  main->controller.detect_line.step = 1;
}

__attribute__((always_inline)) static void
Main_Controller_Detect_Line_init(struct Main *main) {
  assert(main != 0);
  Main_Controller_Detect_Line_reset(main);
}

__attribute__((always_inline)) static void
Main_Controller_Detect_Line_tick(struct Main *main) {
  assert(main != 0);
  const int point = read_int_port(main->port, PORT_ADDRESS_POINT, 0);
  const bool on_line = read_bit_port(main->port, PORT_ADDRESS_ON_LINE, 0, false);
  const float distance_top = read_real_port(main->port, PORT_ADDRESS_DISTANCE_TOP, 0.);
  const float distance_bottom = read_real_port(main->port, PORT_ADDRESS_DISTANCE_BOTTOM, 0.);
  const float distance_left = read_real_port(main->port, PORT_ADDRESS_DISTANCE_LEFT, 0.);
  const float distance_right = read_real_port(main->port, PORT_ADDRESS_DISTANCE_RIGHT, 0.);
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
    } else if (distance_top >= 0 || distance_bottom >= 0 || distance_left >= 0 || distance_right >= 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_DETECT_WALL;
    } else if (true) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_UP;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_LINE_UP: {
    // Always
    {
      {
        main->controller.detect_line.it = main->controller.detect_line.it + 1;
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_GAS, true);
      }
    }
    //
    if (on_line || point > 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_END;
      {
        main->controller.detect_line.it = 0;
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_GAS, false);
      }
    } else if (distance_top >= 0 || distance_bottom >= 0 || distance_left >= 0 || distance_right >= 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_DETECT_WALL;
      {
        main->controller.detect_line.it = 0;
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_GAS, false);
      }
    } else if (main->controller.detect_line.it >= main->controller.detect_line.step) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_LEFT;
      {
        main->controller.detect_line.it = 0;
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_GAS, false);
      }
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_LINE_LEFT: {
    // Always
    {
      {
        main->controller.detect_line.it = main->controller.detect_line.it + 1;
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_LEFT, true);
      }
    }
    //
    if (on_line || point > 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_END;
      {
        main->controller.detect_line.it = 0;
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_LEFT, false);
      }
    } else if (distance_top >= 0 || distance_bottom >= 0 || distance_left >= 0 || distance_right >= 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_DETECT_WALL;
      {
        main->controller.detect_line.it = 0;
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_LEFT, false);
      }
    } else if (main->controller.detect_line.it >= main->controller.detect_line.step) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_BOTTOM;
      {
        main->controller.detect_line.it = 0;
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_LEFT, false);
      }
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_LINE_BOTTOM: {
    // Always
    {
      {
        main->controller.detect_line.it = main->controller.detect_line.it + 1;
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_BACK, true);
      }
    }
    //
    if (on_line || point > 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_END;
      {
        main->controller.detect_line.it = 0;
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_BACK, false);
      }
    } else if (distance_top >= 0 || distance_bottom >= 0 || distance_left >= 0 || distance_right >= 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_DETECT_WALL;
      {
        main->controller.detect_line.it = 0;
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_BACK, false);
      }
    } else if (main->controller.detect_line.it >= main->controller.detect_line.step) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_RIGHT;
      {
        main->controller.detect_line.it = 0;
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_BACK, false);
      }
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_LINE_RIGHT: {
    // Always
    {
      {
        main->controller.detect_line.it = main->controller.detect_line.it + 1;
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_RIGHT, true);
      }
    }
    //
    if (on_line || point > 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_END;
      {
        main->controller.detect_line.it = 0;
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_RIGHT, false);
        main->controller.detect_line.step = main->controller.detect_line.step + 1;
      }
    } else if (distance_top >= 0 || distance_bottom >= 0 || distance_left >= 0 || distance_right >= 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_DETECT_WALL;
      {
        main->controller.detect_line.it = 0;
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_RIGHT, false);
        main->controller.detect_line.step = main->controller.detect_line.step + 1;
      }
    } else if (main->controller.detect_line.it >= main->controller.detect_line.step) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_START;
      {
        main->controller.detect_line.it = 0;
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_RIGHT, false);
        main->controller.detect_line.step = main->controller.detect_line.step + 1;
      }
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_LINE_DETECT_WALL: {
    if (distance_left >= 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_WALL_LEFT;
    } else if (distance_right >= 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_WALL_RIGHT;
    } else if (distance_top >= 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_WALL_TOP;
    } else if (distance_bottom >= 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_WALL_BOTTOM;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_LINE_WALL_LEFT: {
    // Always
    {
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_RIGHT, true);
      }
    }
    //
    if (distance_top >= 0 || distance_bottom >= 0 || distance_right >= 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_START;
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_RIGHT, false);
      }
    } else if (on_line || point > 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_END;
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_RIGHT, false);
      }
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_LINE_WALL_RIGHT: {
    // Always
    {
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_LEFT, true);
      }
    }
    //
    if (distance_top >= 0 || distance_bottom >= 0 || distance_left >= 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_START;
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_LEFT, false);
      }
    } else if (on_line || point > 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_END;
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_LEFT, false);
      }
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_LINE_WALL_TOP: {
    // Always
    {
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_BACK, true);
      }
    }
    //
    if (distance_left >= 0 || distance_bottom >= 0 || distance_right >= 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_START;
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_BACK, false);
      }
    } else if (on_line || point > 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_END;
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_BACK, false);
      }
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_LINE_WALL_BOTTOM: {
    // Always
    {
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_GAS, true);
      }
    }
    //
    if (distance_top >= 0 || distance_left >= 0 || distance_right >= 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_START;
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_GAS, false);
      }
    } else if (on_line || point > 0) {
      main->controller.detect_line.state = MAIN_CONTROLLER_DETECT_LINE_END;
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_GAS, false);
      }
    }
    break;
  }
  default: break;
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
  const float distance_top = read_real_port(main->port, PORT_ADDRESS_DISTANCE_TOP, 0.);
  const float distance_bottom = read_real_port(main->port, PORT_ADDRESS_DISTANCE_BOTTOM, 0.);
  switch (main->controller.detect_angle.state) {
  case MAIN_CONTROLLER_DETECT_ANGLE_INIT: {
    main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_START;
    break;
  }
  case MAIN_CONTROLLER_DETECT_ANGLE_START: {
    if (angle == 0 || angle == 90 || angle == 180 || angle == 270) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_END;
      // Exit
      {
        main->watchdog = 0;
        main->reset = false;
      }
    } else if (angle > 0 && angle < 45) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_45;
    } else if (angle >= 45 && angle < 90) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_90;
    } else if (angle > 90 && angle < 135) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_135;
    } else if (angle >= 135 && angle < 180) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_180;
    } else if (angle > 180 && angle < 225) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_225;
    } else if (angle >= 225 && angle < 270) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_270;
    } else if (angle > 270 && angle < 315) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_315;
    } else if (angle >= 315 && angle < 360) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_360;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_45: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
    }
    //
    if ((distance_top >= 0 && distance_top <= 2) || (distance_bottom >= 0 && distance_bottom <= 2)) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_90;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle > 90 && angle < 135) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_135;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle >= 135 && angle < 180) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_180;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle > 180 && angle < 225) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_225;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle >= 225 && angle < 270) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_270;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle > 270 && angle < 315) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_315;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle >= 315 && angle < 360) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_360;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle == 0) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_END;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
      // Exit
      {
        main->watchdog = 0;
        main->reset = false;
      }
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_90: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
    }
    //
    if ((distance_top >= 0 && distance_top <= 2) || (distance_bottom >= 0 && distance_bottom <= 2)) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_45;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle > 90 && angle < 135) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_135;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle >= 135 && angle < 180) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_180;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle > 180 && angle < 225) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_225;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle >= 225 && angle < 270) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_270;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle > 270 && angle < 315) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_315;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle >= 315 && angle < 360) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_360;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle == 90) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_END;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
      // Exit
      {
        main->watchdog = 0;
        main->reset = false;
      }
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_135: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
    }
    //
    if ((distance_top >= 0 && distance_top <= 2) || (distance_bottom >= 0 && distance_bottom <= 2)) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_180;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle > 0 && angle < 45) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_45;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle >= 45 && angle < 90) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_90;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle > 180 && angle < 225) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_225;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle >= 225 && angle < 270) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_270;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle > 270 && angle < 315) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_315;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle >= 315 && angle < 360) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_360;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle == 90) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_END;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
      // Exit
      {
        main->watchdog = 0;
        main->reset = false;
      }
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_180: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
    }
    //
    if ((distance_top >= 0 && distance_top <= 2) || (distance_bottom >= 0 && distance_bottom <= 2)) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_135;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle > 0 && angle < 45) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_45;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle >= 45 && angle < 90) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_90;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle > 180 && angle < 225) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_225;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle >= 225 && angle < 270) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_270;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle > 270 && angle < 315) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_315;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle >= 315 && angle < 360) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_360;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle == 180) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_END;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
      // Exit
      {
        main->watchdog = 0;
        main->reset = false;
      }
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_225: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
    }
    //
    if ((distance_top >= 0 && distance_top <= 2) || (distance_bottom >= 0 && distance_bottom <= 2)) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_270;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle > 0 && angle < 45) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_45;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle >= 45 && angle < 90) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_90;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle > 90 && angle < 135) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_135;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle >= 135 && angle < 180) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_180;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle > 270 && angle < 315) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_315;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle >= 315 && angle < 360) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_360;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle == 180) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_END;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
      // Exit
      {
        main->watchdog = 0;
        main->reset = false;
      }
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_270: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
    }
    //
    if ((distance_top >= 0 && distance_top <= 2) || (distance_bottom >= 0 && distance_bottom <= 2)) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_225;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle > 0 && angle < 45) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_45;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle >= 45 && angle < 90) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_90;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle > 90 && angle < 135) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_135;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle >= 135 && angle < 180) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_180;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle > 270 && angle < 315) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_315;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle >= 315 && angle < 360) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_360;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle == 180) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_END;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
      // Exit
      {
        main->watchdog = 0;
        main->reset = false;
      }
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_315: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
    }
    //
    if ((distance_top >= 0 && distance_top <= 2) || (distance_bottom >= 0 && distance_bottom <= 2)) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_360;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle > 0 && angle < 45) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_45;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle >= 45 && angle < 90) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_90;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle > 90 && angle < 135) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_135;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle >= 135 && angle < 180) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_180;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle > 180 && angle < 225) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_225;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle >= 225 && angle < 270) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_270;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    } else if (angle == 270) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_END;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
      // Exit
      {
        main->watchdog = 0;
        main->reset = false;
      }
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_360: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
    }
    //
    if ((distance_top >= 0 && distance_top <= 2) || (distance_bottom >= 0 && distance_bottom <= 2)) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_315;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle > 0 && angle < 45) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_45;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle >= 45 && angle < 90) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_90;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle > 90 && angle < 135) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_135;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle >= 135 && angle < 180) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_180;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle > 180 && angle < 225) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_225;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle >= 225 && angle < 270) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_ANGLE_270;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    } else if (angle == 0) {
      main->controller.detect_angle.state = MAIN_CONTROLLER_DETECT_ANGLE_END;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
      // Exit
      {
        main->watchdog = 0;
        main->reset = false;
      }
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_ANGLE_END: {
    break;
  }
  default: break;
  }
}
__attribute__((always_inline)) static bool Main_Controller_Detect_Angle_finished(const struct Main *main) {
  assert(main != 0);
  return main->controller.detect_angle.state == MAIN_CONTROLLER_DETECT_ANGLE_END;
}

// model: Detect_Point
__attribute__((always_inline)) static void
Main_Controller_Detect_Point_reset(struct Main *main) {
  assert(main != 0);
  main->controller.detect_point.state = MAIN_CONTROLLER_DETECT_POINT_INIT;
  main->controller.detect_point.direct = -1;
}

__attribute__((always_inline)) static void
Main_Controller_Detect_Point_init(struct Main *main) {
  assert(main != 0);
  Main_Controller_Detect_Point_reset(main);
}

__attribute__((always_inline)) static void
Main_Controller_Detect_Point_tick(struct Main *main) {
  assert(main != 0);
  const int point = read_int_port(main->port, PORT_ADDRESS_POINT, 0);
  const bool on_line = read_bit_port(main->port, PORT_ADDRESS_ON_LINE, 0, false);
  switch (main->controller.detect_point.state) {
  case MAIN_CONTROLLER_DETECT_POINT_INIT: {
    main->controller.detect_point.direct = -1;
    main->controller.detect_point.state = MAIN_CONTROLLER_DETECT_POINT_START;
    break;
  }
  case MAIN_CONTROLLER_DETECT_POINT_START: {
    if (point > 0) {
      main->controller.detect_point.state = MAIN_CONTROLLER_DETECT_POINT_END;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_GAS, true);
      // Exit
      main->watchdog = 0;
    } else if (true) {
      main->controller.detect_point.state = MAIN_CONTROLLER_DETECT_POINT_UP;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_GAS, true);
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_POINT_UP: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_GAS, true);
    }
    //
    if (point > 0) {
      main->controller.detect_point.state = MAIN_CONTROLLER_DETECT_POINT_COMPROMISE;
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_GAS, false);
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_BACK, true);
        main->controller.detect_point.direct = 1;
      }
    } else if ((!on_line) && point < 0) {
      main->controller.detect_point.state = MAIN_CONTROLLER_DETECT_POINT_DOWN;
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_GAS, false);
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_BACK, true);
        main->controller.detect_point.direct = 1;
      }
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_POINT_DOWN: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_BACK, true);
    }
    //
    if (point > 0) {
      main->controller.detect_point.state = MAIN_CONTROLLER_DETECT_POINT_COMPROMISE;
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_BACK, false);
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_LEFT, true);
        main->controller.detect_point.direct = 2;
      }
    } else if ((!on_line) && point < 0) {
      main->controller.detect_point.state = MAIN_CONTROLLER_DETECT_POINT_LEFT;
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_BACK, false);
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_LEFT, true);
        main->controller.detect_point.direct = 2;
      }
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_POINT_LEFT: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_LEFT, true);
    }
    //
    if (point > 0) {
      main->controller.detect_point.state = MAIN_CONTROLLER_DETECT_POINT_COMPROMISE;
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_LEFT, false);
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_RIGHT, true);
        main->controller.detect_point.direct = 3;
      }
    } else if ((!on_line) && point < 0) {
      main->controller.detect_point.state = MAIN_CONTROLLER_DETECT_POINT_RIGHT;
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_LEFT, false);
        write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_RIGHT, true);
        main->controller.detect_point.direct = 3;
      }
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_POINT_RIGHT: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_RIGHT, true);
    }
    //
    if (point > 0) {
      main->controller.detect_point.state = MAIN_CONTROLLER_DETECT_POINT_COMPROMISE;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_RIGHT, false);
    } else if ((!on_line) && point < 0) {
      main->controller.detect_point.state = MAIN_CONTROLLER_DETECT_POINT_START;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_RIGHT, false);
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_POINT_COMPROMISE: {
    // Always
    {
      {
        if (main->controller.detect_point.direct == 1)         {
          write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_GAS, true);
        }
        else         if (main->controller.detect_point.direct == 2)         {
          write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_RIGHT, true);
        }
        else         if (main->controller.detect_point.direct == 3)         {
          write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_LEFT, true);
        }
      }
    }
    //
    if (true) {
      main->controller.detect_point.state = MAIN_CONTROLLER_DETECT_POINT_END;
      // Exit
      main->watchdog = 0;
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_POINT_END: {
    break;
  }
  default: break;
  }
}
__attribute__((always_inline)) static bool Main_Controller_Detect_Point_finished(const struct Main *main) {
  assert(main != 0);
  return main->controller.detect_point.state == MAIN_CONTROLLER_DETECT_POINT_END;
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
  const int point = read_int_port(main->port, PORT_ADDRESS_POINT, 0);
  const float distance_top = read_real_port(main->port, PORT_ADDRESS_DISTANCE_TOP, 0.);
  const float distance_bottom = read_real_port(main->port, PORT_ADDRESS_DISTANCE_BOTTOM, 0.);
  const float distance_left = read_real_port(main->port, PORT_ADDRESS_DISTANCE_LEFT, 0.);
  const float distance_right = read_real_port(main->port, PORT_ADDRESS_DISTANCE_RIGHT, 0.);
  const bool push_level = read_bit_port(main->port, PORT_ADDRESS_PUSH_LEVEL, 0, false);
  const bool busy = read_bit_port(main->port, PORT_ADDRESS_BUSY, 0, false);
  const bool error = read_bit_port(main->port, PORT_ADDRESS_ERROR, 0, false);
  const bool command_ready = read_bit_port(main->port, PORT_ADDRESS_COMMAND_READY, 0, false);
  const int command_code = read_int_port(main->port, PORT_ADDRESS_COMMAND_CODE, 0);
  const int command_point = read_int_port(main->port, PORT_ADDRESS_COMMAND_POINT, 0);
  const int command_level = read_int_port(main->port, PORT_ADDRESS_COMMAND_LEVEL, 0);
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
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, false);
    }
    //
    if (command_ready && (!busy) && (!error)) {
      main->command.state = MAIN_COMMAND_SAVING;
    }
    break;
  }
  case MAIN_COMMAND_SAVING: {
    // Always
    {
      {
        main->command.code = command_code;
        write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
        main->command.next_point = command_point;
      }
    }
    //
    if (true) {
      main->command.state = MAIN_COMMAND_DECODE;
    }
    break;
  }
  case MAIN_COMMAND_DECODE: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_BUSY, true);
    }
    //
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
    }
    //
    if (main->command.code == 0) {
      main->command.state = MAIN_COMMAND_RESET_CODE;
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_RESET, true);
    } else if (main->command.code == 1) {
      main->command.state = MAIN_COMMAND_COMPLETE_CODE;
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_RESET, true);
    } else if (main->command.code == 2) {
      main->command.state = MAIN_COMMAND_ROTATE_LEFT_CODE;
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_RESET, true);
      // Before
      {
        main->command.next_angl_up = angle - ANGLE_DELTA - ROUND_DELTA;
        main->command.next_angl_down = angle - ANGLE_DELTA + ROUND_DELTA;
        if (main->command.next_angl_up >= 360.0)         {
          main->command.next_angl_up = 0.0;
        }
        else         if (main->command.next_angl_up < 0.0)         {
          main->command.next_angl_up = 350.0;
        }
        if (main->command.next_angl_down >= 360.0)         {
          main->command.next_angl_down = 0.0;
        }
        else         if (main->command.next_angl_down < 0.0)         {
          main->command.next_angl_down = 360.0;
        }
      }
    } else if (main->command.code == 3) {
      main->command.state = MAIN_COMMAND_ROTATE_RIGHT_CODE;
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_RESET, true);
      // Before
      {
        main->command.next_angl_up = angle + ANGLE_DELTA - ROUND_DELTA;
        main->command.next_angl_down = angle + ANGLE_DELTA + ROUND_DELTA;
        if (main->command.next_angl_up >= 360.0)         {
          main->command.next_angl_up = 0.0;
        }
        else         if (main->command.next_angl_up < 0.0)         {
          main->command.next_angl_up = 360.0;
        }
        if (main->command.next_angl_down >= 360.0)         {
          main->command.next_angl_down = 0.0;
        }
        else         if (main->command.next_angl_down < 0.0)         {
          main->command.next_angl_down = 360.0;
        }
      }
    } else if (main->command.code == 4) {
      main->command.state = MAIN_COMMAND_MOVING_UP_CODE;
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_RESET, true);
    } else if (main->command.code == 5) {
      main->command.state = MAIN_COMMAND_MOVING_DOWN_CODE;
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_RESET, true);
    } else if (main->command.code == 6) {
      main->command.state = MAIN_COMMAND_MOVING_SHIFT_LEFT_CODE;
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_RESET, true);
    } else if (main->command.code == 7) {
      main->command.state = MAIN_COMMAND_MOVING_SHIFT_RIGHT_CODE;
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_RESET, true);
    } else if (main->command.code == 8) {
      main->command.state = MAIN_COMMAND_PUSHING_UP_CODE;
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_RESET, true);
      // Before
      main->command.next_level = command_level;
    } else if (main->command.code == 9) {
      main->command.state = MAIN_COMMAND_PUSHING_DOWN_CODE;
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_RESET, true);
    } else if (true) {
      main->command.state = MAIN_COMMAND_UNKNOWN_CODE;
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_RESET, true);
    }
    break;
  }
  case MAIN_COMMAND_PUSHING_UP_CODE: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_PUSH_UP, true);
    }
    //
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
    }
    //
    if (main->command.next_level == push_level) {
      main->command.state = MAIN_COMMAND_READING;
      write_bit_port(main->port, 0, PORT_ADDRESS_PUSH_UP, false);
    }
    break;
  }
  case MAIN_COMMAND_PUSHING_DOWN_CODE: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_PUSH_DOWN, true);
    }
    //
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
    }
    //
    if (0 == push_level) {
      main->command.state = MAIN_COMMAND_READING;
      write_bit_port(main->port, 0, PORT_ADDRESS_PUSH_DOWN, false);
    }
    break;
  }
  case MAIN_COMMAND_ROTATE_LEFT_CODE: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, true);
    }
    //
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
    }
    //
    if (angle >= main->command.next_angl_up || angle <= main->command.next_angl_down) {
      main->command.state = MAIN_COMMAND_READING;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_LEFT, false);
    }
    break;
  }
  case MAIN_COMMAND_ROTATE_RIGHT_CODE: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, true);
    }
    //
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
    }
    //
    if (angle >= main->command.next_angl_up && angle <= main->command.next_angl_down) {
      main->command.state = MAIN_COMMAND_READING;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_ROT_RIGHT, false);
    }
    break;
  }
  case MAIN_COMMAND_MOVING_UP_CODE: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_GAS, true);
    }
    //
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
    }
    //
    if (main->command.next_point == point) {
      main->command.state = MAIN_COMMAND_READING;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_GAS, false);
    } else if (distance_top >= 0 && distance_top <= 2) {
      main->command.state = MAIN_COMMAND_EMERGENCY;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_GAS, false);
    }
    break;
  }
  case MAIN_COMMAND_MOVING_DOWN_CODE: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_BACK, true);
    }
    //
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
    }
    //
    if (main->command.next_point == point) {
      main->command.state = MAIN_COMMAND_READING;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_BACK, false);
    } else if (distance_bottom >= 0 && distance_bottom <= 2) {
      main->command.state = MAIN_COMMAND_EMERGENCY;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_BACK, false);
    }
    break;
  }
  case MAIN_COMMAND_MOVING_SHIFT_LEFT_CODE: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_LEFT, true);
    }
    //
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
    }
    //
    if (main->command.next_point == point) {
      main->command.state = MAIN_COMMAND_READING;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_LEFT, false);
    } else if (distance_left >= 0 && distance_left <= 2) {
      main->command.state = MAIN_COMMAND_EMERGENCY;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_LEFT, false);
    }
    break;
  }
  case MAIN_COMMAND_MOVING_SHIFT_RIGHT_CODE: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_RIGHT, true);
    }
    //
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
    }
    //
    if (main->command.next_point == point) {
      main->command.state = MAIN_COMMAND_READING;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_RIGHT, false);
    } else if (distance_right >= 0 && distance_right <= 2) {
      main->command.state = MAIN_COMMAND_EMERGENCY;
      write_bit_port(main->port, 0, PORT_ADDRESS_MACHINE_SHIFT_RIGHT, false);
    }
    break;
  }
  case MAIN_COMMAND_UNKNOWN_CODE: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_BUSY, true);
    }
    //
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
    }
    //
    if (true) {
      main->command.state = MAIN_COMMAND_READING;
    }
    break;
  }
  case MAIN_COMMAND_RESET_CODE: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_BUSY, true);
    }
    //
    // Always
    {
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
        main->reset = true;
      }
    }
    //
    if (true) {
      main->command.state = MAIN_COMMAND_READING;
    }
    break;
  }
  case MAIN_COMMAND_COMPLETE_CODE: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_BUSY, true);
    }
    //
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_COMMAND_EXECUTING, true);
    }
    //
    if (true) {
      main->command.state = MAIN_COMMAND_READING;
    }
    break;
  }
  case MAIN_COMMAND_EMERGENCY: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_ERROR, true);
    }
    //
    if (true) {
      main->command.state = MAIN_COMMAND_READING;
    }
    break;
  }
  case MAIN_COMMAND_END: {
    break;
  }
  default: break;
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
  const bool command_executing = read_bit_port(main->port, PORT_ADDRESS_COMMAND_EXECUTING, 0, false);
  switch (main->controller.state) {
  case MAIN_CONTROLLER_INIT: {
    main->controller.raise = 0;
    main->controller.state = MAIN_CONTROLLER_START;
    break;
  }
  case MAIN_CONTROLLER_START: {
    if (true) {
      main->controller.state = MAIN_CONTROLLER_DETECT_ANGLE;
      {
        main->reset = false;
        main->controller.raise = main->controller.raise + 1;
      }
      Main_Controller_Detect_Angle_init(main);
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_ANGLE: {
    Main_Controller_Detect_Angle_tick(main);
    if (main->controller.detect_angle.state == MAIN_CONTROLLER_DETECT_ANGLE_END || main->reset) {
      main->controller.state = MAIN_CONTROLLER_DETECT_LINE;
      Main_Controller_Detect_Line_init(main);
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_LINE: {
    Main_Controller_Detect_Line_tick(main);
    if (main->controller.detect_line.state == MAIN_CONTROLLER_DETECT_LINE_END || main->reset) {
      main->controller.state = MAIN_CONTROLLER_DETECT_POINT;
      Main_Controller_Detect_Point_init(main);
    }
    break;
  }
  case MAIN_CONTROLLER_DETECT_POINT: {
    Main_Controller_Detect_Point_tick(main);
    if (main->controller.detect_point.state == MAIN_CONTROLLER_DETECT_POINT_END || main->reset) {
      main->controller.state = MAIN_CONTROLLER_RESET;
    }
    break;
  }
  case MAIN_CONTROLLER_RESET: {
    // Always
    {
      write_bit_port(main->port, 0, PORT_ADDRESS_BUSY, true);
    }
    //
    if (main->reset) {
      main->controller.state = MAIN_CONTROLLER_START;
    } else if (error) {
      main->controller.state = MAIN_CONTROLLER_ERROR;
    } else if (true) {
      main->controller.state = MAIN_CONTROLLER_WAITING;
    }
    break;
  }
  case MAIN_CONTROLLER_WAITING: {
    // Always
    {
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_BUSY, false);
        main->watchdog = 0;
      }
    }
    //
    if (command_executing) {
      main->controller.state = MAIN_CONTROLLER_EXECUTING_COMMAND;
    }
    break;
  }
  case MAIN_CONTROLLER_EXECUTING_COMMAND: {
    // Always
    {
      {
        write_bit_port(main->port, 0, PORT_ADDRESS_BUSY, true);
        main->watchdog = 0;
      }
    }
    //
    if ((!command_executing)) {
      main->controller.state = MAIN_CONTROLLER_WAITING;
    }
    break;
  }
  case MAIN_CONTROLLER_ERROR: {
    // Always
    {
      debug("Сработал Watchdog");
    }
    //
    if (main->reset) {
      main->controller.state = MAIN_CONTROLLER_START;
    }
    break;
  }
  case MAIN_CONTROLLER_END: {
    break;
  }
  default: break;
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
