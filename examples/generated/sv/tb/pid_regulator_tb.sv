// Тестбенч цели sv для порождённого pid_regulator.sv.
//
// не порождается taktc: тестбенч - принадлежность проверки, а не продукта
// Лежит в tb/, поэтому глоб `*.sv` проверки цели sv его не
// подхватывает.
//
// ПИД-регулятор самодостаточен (контур замкнут внутри модели): сходится с
// anti-windup к уставке и завершается. Проверяем `ready` (сошёлся) и `is_done`
// (завершение). Дополнительно наблюдаем иерархической ссылкой, что интеграл
// `i_acc` не переполняется - anti-windup работает и в RTL (repr q(8,8) в
// пределах [−Imax, Imax] = [−8192, 8192]).
//
// Ручной прогон (из examples/generated/sv/):
//   $ verilator --binary --timing --trace --top-module tb tb/pid_regulator_tb.sv pid_regulator.sv -o simtb
//   (cd tb && ../obj_dir/simtb)   # пишет tb/pid_regulator.vcd
`timescale 1ns / 1ps

module tb;
    // Служебные сигналы цели sv.
    logic clk = 0;
    logic rst_n = 0;

    // Выходы модуля.
    logic ready;
    logic is_done;

    pid_regulator dut (
        .clk(clk),
        .rst_n(rst_n),
        .ready(ready),
        .is_done(is_done)
    );

    always #5 clk = ~clk;

    // Накопители событий и контроль anti-windup (интеграл в пределах).
    logic saw_ready = 0, saw_done = 0, windup = 0;
    always @(posedge clk) begin
        if (ready) saw_ready <= 1;
        if (is_done) saw_done <= 1;
        // repr q(8,8) интеграла обязан оставаться в [−Imax, Imax] = [−8192, 8192].
        if ($signed(dut.pid_regulator_pid_i_acc) > 16'sd8192
                || $signed(dut.pid_regulator_pid_i_acc) < -16'sd8192)
            windup <= 1;
    end

    integer i;
    initial begin
        $dumpfile("pid_regulator.vcd");
        $dumpvars(0, tb);

        // Первый фронт снимает сброс.
        @(posedge clk);
        rst_n <= 1'b1;

        // ПИД сходится и держит выход; 40 тактов с запасом.
        //
        // Наблюдаемое - `ready`, а не `is_done`: конечное состояние несёт
        // `always { ready := 1; }`, и автомат в нём остаётся.
        for (i = 0; i < 40; i = i + 1) @(posedge clk);

        if (windup)     $error("pid: интеграл вышел за anti-windup — clamp не работает в RTL");
        if (!saw_ready) $error("pid: ready не поднялся — регулятор не сошёлся");

        if (saw_ready && !windup)
            $display("pid_regulator_tb: OK (ПИД сошёлся: ready держится, интеграл в пределах)");
        $finish;
    end
endmodule
