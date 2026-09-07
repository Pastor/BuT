// Тестбенч цели sv для порождённого elevator_mini.sv.
//
// не порождается taktc: тестбенч - принадлежность проверки, а не продукта
// (решение 0045-07). Лежит в подкаталоге tb/, поэтому глоб `*.sv` гейта цели sv
// его не подхватывает.
//
// Задача - та же, что у stacker_tb.sv:
//   1. проверка функционирования через assert (провал -> $fatal -> красный
//      предкоммит).
//   2. осциллограмма elevator_mini.vcd для gtkwave.
//
// Ручной прогон (из examples/generated/sv/):
//   $ verilator --binary --timing --trace --top-module tb tb/elevator_mini_tb.sv elevator_mini.sv -o simtb
//   (cd tb && ../obj_dir/simtb)   # пишет tb/elevator_mini.vcd
//   gtkwave tb/elevator_mini.vcd
`timescale 1ns / 1ps

module tb;
    // Служебные сигналы цели sv.
    logic clk = 0;
    logic rst_n = 0;

    // Сценарные входы. Нажимаем кнопку кабины "этаж 5"; остальные - 0.
    logic cabin_button_f5 = 0;

    // Датчики концевиков двигателя не используем - двигатель останавливается
    // по команде STOP (приезд на этаж), а не по концевику.
    logic elevator_motor_sensor_d = 0;
    logic elevator_motor_sensor_u = 0;

    // Выходы модуля.
    logic door_open, elevator_motor_down, elevator_motor_stop, elevator_motor_up;
    logic is_done;

    // замкнутая среда "физика лифта". Кабина ползёт к целевому этажу по одному
    // этажу за такт: пока идёт движение и цель не достигнута, "срабатывает"
    // концевик следующего этажа в сторону цели. Внутренние текущий/целевой
    // этаж и состояние читаются иерархической ссылкой (verilator --binary её
    // разрешает).
    wire [7:0] cf = dut.elevator_mini_current_floor;
    wire [7:0] tf = dut.elevator_mini_target_floor;
    wire moving = (dut.elevator_mini_cabin_state == 2'd2); // CABIN_MOVING
    wire [7:0] nf = (tf > cf) ? (cf + 8'd1) : (tf < cf) ? (cf - 8'd1) : cf;
    wire walk = moving && (cf != tf);

    // Одногорячий набор концевиков этажей F1..F9 из "следующего этажа" nf.
    wire floor_sensor_f1_bottom = walk && (nf == 8'd1);
    wire floor_sensor_f2_bottom = walk && (nf == 8'd2);
    wire floor_sensor_f3_bottom = walk && (nf == 8'd3);
    wire floor_sensor_f4_bottom = walk && (nf == 8'd4);
    wire floor_sensor_f5_bottom = walk && (nf == 8'd5);
    wire floor_sensor_f6_bottom = walk && (nf == 8'd6);
    wire floor_sensor_f7_bottom = walk && (nf == 8'd7);
    wire floor_sensor_f8_bottom = walk && (nf == 8'd8);
    wire floor_sensor_f9_bottom = walk && (nf == 8'd9);

    elevator_mini dut (
        .clk(clk),
        .rst_n(rst_n),
        // Неиспользуемые кнопки/датчики тянем в 0 прямо в списке портов.
        .cabin_button_dc(1'b0),
        .cabin_button_f1(1'b0),
        .cabin_button_f2(1'b0),
        .cabin_button_f3(1'b0),
        .cabin_button_f4(1'b0),
        .cabin_button_f5(cabin_button_f5),
        .cabin_button_f6(1'b0),
        .cabin_button_f7(1'b0),
        .cabin_button_f8(1'b0),
        .cabin_button_f9(1'b0),
        .floor_button_f1(1'b0),
        .floor_button_f2(1'b0),
        .floor_button_f3(1'b0),
        .floor_button_f4(1'b0),
        .floor_button_f5(1'b0),
        .floor_button_f6(1'b0),
        .floor_button_f7(1'b0),
        .floor_button_f8(1'b0),
        .floor_button_f9(1'b0),
        .floor_sensor_f1_bottom(floor_sensor_f1_bottom),
        .floor_sensor_f2_bottom(floor_sensor_f2_bottom),
        .floor_sensor_f3_bottom(floor_sensor_f3_bottom),
        .floor_sensor_f4_bottom(floor_sensor_f4_bottom),
        .floor_sensor_f5_bottom(floor_sensor_f5_bottom),
        .floor_sensor_f6_bottom(floor_sensor_f6_bottom),
        .floor_sensor_f7_bottom(floor_sensor_f7_bottom),
        .floor_sensor_f8_bottom(floor_sensor_f8_bottom),
        .floor_sensor_f9_bottom(floor_sensor_f9_bottom),
        .elevator_motor_sensor_d(elevator_motor_sensor_d),
        .elevator_motor_sensor_u(elevator_motor_sensor_u),
        .door_open(door_open),
        .elevator_motor_down(elevator_motor_down),
        .elevator_motor_stop(elevator_motor_stop),
        .elevator_motor_up(elevator_motor_up),
        .is_done(is_done)
    );

    always #5 clk = ~clk;

    // Накопители событий за прогон (устойчивы к сдвигу такта на один).
    logic saw_up = 0, saw_door = 0, saw_at_floor = 0;
    always @(posedge clk) begin
        if (elevator_motor_up) saw_up <= 1;
        if (door_open) saw_door <= 1;
        // CABIN_AT_FLOOR = 2'd0: кабина доехала до целевого этажа.
        if (dut.elevator_mini_cabin_state == 2'd0) saw_at_floor <= 1;
    end

    integer i;
    initial begin
        $dumpfile("elevator_mini.vcd");
        $dumpvars(0, tb);

        // Первый фронт снимает сброс.
        @(posedge clk);
        rst_n <= 1'b1;

        // Нажимаем кнопку кабины "этаж 5" на несколько тактов (стартуем с
        // этажа 1 - таково значение регистра после сброса).
        cabin_button_f5 <= 1'b1;
        repeat (3) @(posedge clk);
        cabin_button_f5 <= 1'b0;

        // "Физика лифта" довозит кабину до этажа 5; 30 тактов с запасом.
        for (i = 0; i < 30; i = i + 1) @(posedge clk);

        // Проверка функционирования: двигатель поехал вверх (elevator_motor_up),
        // дверь открывалась (door_open) и кабина доехала до этажа (AT_FLOOR).
        if (!saw_up)       $error("elevator: elevator_motor_up не поднялся — кабина не поехала вверх");
        if (!saw_door)     $error("elevator: door_open не поднялся — дверь не открывалась");
        if (!saw_at_floor) $error("elevator: кабина не доехала до целевого этажа (AT_FLOOR)");

        if (saw_up && saw_door && saw_at_floor)
            $display("elevator_mini_tb: OK (наблюдались elevator_motor_up, door_open, приезд на этаж)");
        else
            $fatal(1, "elevator_mini_tb: ПРОВАЛ функциональной проверки");

        $finish;
    end
endmodule
