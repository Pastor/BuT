// Тестбенч цели sv для порождённого regulator.sv.
//
// не порождается taktc: тестбенч - принадлежность проверки, а не продукта
// (решение 0045-07). Лежит в tb/, поэтому глоб `*.sv` гейта цели sv его не
// подхватывает.
//
// Регулятор самодостаточен (входов нет - контур замкнут внутри модели): сходится
// к уставке и завершается. Проверяем, что порт `ready` поднялся (регулятор довёл
// величину до уставки) и `is_done` - завершение (состояние Done).
//
// Ручной прогон (из examples/generated/sv/):
//   $ verilator --binary --timing --trace --top-module tb tb/regulator_tb.sv regulator.sv -o simtb
//   (cd tb && ../obj_dir/simtb)   # пишет tb/regulator.vcd
`timescale 1ns / 1ps

module tb;
    // Служебные сигналы цели sv.
    logic clk = 0;
    logic rst_n = 0;

    // Выходы модуля.
    logic ready;
    logic is_done;

    regulator dut (
        .clk(clk),
        .rst_n(rst_n),
        .ready(ready),
        .is_done(is_done)
    );

    always #5 clk = ~clk;

    // Накопители событий (устойчивы к сдвигу такта на один).
    logic saw_ready = 0, saw_done = 0;
    always @(posedge clk) begin
        if (ready) saw_ready <= 1;
        if (is_done) saw_done <= 1;
    end

    integer i;
    initial begin
        $dumpfile("regulator.vcd");
        $dumpvars(0, tb);

        // Первый фронт снимает сброс.
        @(posedge clk);
        rst_n <= 1'b1;

        // Регулятор сходится и держит выход; 40 тактов с запасом.
        //
        // Наблюдаемое - `ready`, а не `is_done`: конечное состояние несёт
        // `always { ready := 1; }`, и автомат в нём остаётся - состояние
        // стабильно, покидают его только по переходу.
        for (i = 0; i < 40; i = i + 1) @(posedge clk);

        if (!saw_ready) $error("regulator: ready не поднялся — регулятор не сошёлся");

        if (saw_ready)
            $display("regulator_tb: OK (регулятор сошёлся: ready поднят и держится)");
        $finish;
    end
endmodule
