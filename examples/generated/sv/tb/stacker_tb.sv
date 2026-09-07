// Тестбенч цели sv для порождённого stacker.sv.
//
// не порождается taktc: тестбенч - принадлежность проверки, а не продукта
// Генератор не должен уметь то, что нужно
// только тестам. Файл лежит в подкаталоге tb/, поэтому глоб `*.sv` проверки цели
// sv (в scripts/precheck.sh) его не подхватывает и не пытается линтовать/
// синтезировать как самостоятельный модуль.
//
// Задача тестбенча - двойная:
//   1. проверка функционирования. Прогнать модуль на осмысленном сценарии и
//      убедиться assert-ами, что наблюдаемое поведение верно (провал -> $fatal
//      -> ненулевой код выхода -> красный предкоммит).
//   2. осциллограмма для gtkwave. $dumpvars пишет stacker.vcd - снимок всех
//      сигналов на каждом такте.
//
// Ручной прогон (из examples/generated/sv/):
//   $ verilator --binary --timing --trace --top-module tb tb/stacker_tb.sv stacker.sv -o simtb
//   (cd tb && ../obj_dir/simtb)   # пишет tb/stacker.vcd
//   gtkwave tb/stacker.vcd
//
// В предкоммите шаг выполняется автоматически (проверка тестбенчей цели sv).
`timescale 1ns / 1ps

module tb;
    // Служебные сигналы цели sv.
    logic clk = 0;
    logic rst_n = 0;

    // Сценарные входы - их задаёт среда.
    logic sense_battery_low = 0;
    logic sense_at_charge = 0;
    logic task_type = 0; // 0 — забор груза: сценарий проходит весь цикл
    logic task_valid = 0;
    logic [7:0] task_stack_no = 0;
    logic [7:0] task_row_no = 0;
    logic [7:0] task_section_no = 0;

    // Выходы модуля.
    logic cmd_ack, cmd_done, cmd_fork;
    logic [7:0] cmd_target_row, cmd_target_section, cmd_target_stack;
    logic is_done;

    // замкнутая среда. Тележка "телепортируется" туда, куда её послали:
    // положение на следующем такте равно скомандованной цели (регистровый
    // выход cmd_target_*). Так автомат проходит весь цикл (забор -> ячейка ->
    // завершение) без ручной подгонки тактов под каждое движение.
    //
    // Датчик груза следует фазе вил: на заборе (lift_op = 0) груз есть, на
    // укладке (lift_op = 1) груза уже нет - иначе укладка бы не завершилась.
    // Внутренний сигнал читается иерархической ссылкой (verilator --binary её
    // разрешает - тот же приём, что в conformance_sv_tests.rs).
    wire [7:0] pos_stack = cmd_target_stack;
    wire [7:0] pos_row = cmd_target_row;
    wire [7:0] pos_section = cmd_target_section;
    wire sense_loaded = ~dut.stacker_lift_op;

    stacker dut (
        .clk(clk),
        .rst_n(rst_n),
        .pos_row(pos_row),
        .pos_section(pos_section),
        .pos_stack(pos_stack),
        .sense_at_charge(sense_at_charge),
        .sense_battery_low(sense_battery_low),
        .sense_loaded(sense_loaded),
        .task_row_no(task_row_no),
        .task_section_no(task_section_no),
        .task_stack_no(task_stack_no),
        .task_type(task_type),
        .task_valid(task_valid),
        .cmd_ack(cmd_ack),
        .cmd_done(cmd_done),
        .cmd_fork(cmd_fork),
        .cmd_target_row(cmd_target_row),
        .cmd_target_section(cmd_target_section),
        .cmd_target_stack(cmd_target_stack),
        .is_done(is_done)
    );

    always #5 clk = ~clk;

    // Накопители событий: за прогон каждое обязано случиться хоть раз. Проверка
    // "событие когда-нибудь произошло" устойчива к сдвигу такта на один - в
    // отличие от привязки к конкретному номеру такта (потактовую точность даёт
    // отдельная сверка, conformance_sv_tests.rs).
    logic saw_ack = 0, saw_fork = 0, saw_done = 0;
    always @(posedge clk) begin
        if (cmd_ack) saw_ack <= 1;
        if (cmd_fork) saw_fork <= 1;
        if (cmd_done) saw_done <= 1;
    end

    integer i;
    initial begin
        $dumpfile("stacker.vcd");
        $dumpvars(0, tb);

        // Первый фронт снимает сброс: стартовые состояния всех уровней уже в
        // регистрах, поэтому следующий фронт - такт 1 модели.
        @(posedge clk);
        rst_n <= 1'b1;

        // Заявка на забор груза из ячейки (stack=5, row=1, section=1).
        task_stack_no   <= 8'd5;
        task_row_no     <= 8'd1;
        task_section_no <= 8'd1;
        task_type       <= 1'b0;
        task_valid      <= 1'b1;

        // Замкнутая среда ведёт автомат через весь цикл; 40 тактов с запасом.
        for (i = 0; i < 40; i = i + 1) @(posedge clk);

        // Проверка функционирования: заявка принята (cmd_ack), вилы отработали
        // (cmd_fork), задача завершена (cmd_done).
        if (!saw_ack)  $error("stacker: cmd_ack не поднялся — заявка не принята");
        if (!saw_fork) $error("stacker: cmd_fork не поднялся — вилы не сработали");
        if (!saw_done) $error("stacker: cmd_done не поднялся — задача не завершена");

        if (saw_ack && saw_fork && saw_done)
            $display("stacker_tb: OK (наблюдались cmd_ack, cmd_fork, cmd_done)");
        else
            $fatal(1, "stacker_tb: ПРОВАЛ функциональной проверки");

        $finish;
    end
endmodule
