// Тестбенч адаптера шины APB для порождённого stacker_apb.sv.
//
// не порождается taktc: тестбенч - принадлежность проверки, а не продукта
// Лежит в tb/, поэтому глоб `*.sv` проверки цели sv-mmio его не
// подхватывает.
//
// Что здесь доказывается - и почему этого не доказывают линт и синтез:
// оба инструмента SV принимают модуль, который не работает.
// Поэтому тестбенч ведёт настоящие циклы APB и сверяет значения.
//
// внимание: слово "verilator" в начале строки комментария недопустимо -
// инструмент принимает такую строку за свою прагму и отвергает файл
// (`BADVLTPRAGMA`). Проверки:
//
//   1. запись во входной регистр (0x300 - sense_loaded, один бит) и чтение его
//      обратно: значение, положенное шиной, должно вернуться;
//   2. то же для широкого регистра (0x102 - task_stack_no, 8 бит);
//   3. чтение выходного регистра (0x600 - cmd_ack): он принадлежит автомату, а
//      шина обязана его видеть;
//   4. запись по адресу без порта (0x7ff) не должна ничего портить;
//   5. pready и pslverr - константы контракта адаптера.
//
// Ручной прогон (из examples/generated/sv-mmio/):
//   $ verilator --binary --timing --trace --top-module tb \
//       tb/stacker_apb_tb.sv stacker_apb.sv stacker.sv -o simtb
//   (cd tb && ../obj_dir/simtb)   # пишет tb/stacker_apb.vcd
`timescale 1ns / 1ps

module tb;
    // Сигналы APB (ширины - те же, что у порождённого адаптера).
    logic        pclk = 0;
    logic        presetn = 0;
    logic [10:0] paddr = '0;
    logic        psel = 0;
    logic        penable = 0;
    logic        pwrite = 0;
    logic [7:0]  pwdata = '0;
    logic [7:0]  prdata;
    logic        pready;
    logic        pslverr;
    logic        en = 1'b1;
    logic        is_done;

    stacker_apb dut (
        .pclk(pclk),
        .presetn(presetn),
        .paddr(paddr),
        .psel(psel),
        .penable(penable),
        .pwrite(pwrite),
        .pwdata(pwdata),
        .prdata(prdata),
        .pready(pready),
        .pslverr(pslverr),
        .en(en),
        .is_done(is_done)
    );

    always #5 pclk = ~pclk;

    // Классический цикл APB: setup-фаза (psel, !penable) -> access-фаза
    // (psel, penable). При pready = 1 access длится один такт, поэтому строб
    // записи ядра поднимается ровно однажды.
    task automatic apb_write(input logic [10:0] a, input logic [7:0] d);
        @(posedge pclk);
        paddr <= a;
        pwdata <= d;
        pwrite <= 1'b1;
        psel <= 1'b1;
        penable <= 1'b0;
        @(posedge pclk);
        penable <= 1'b1;
        @(posedge pclk);
        psel <= 1'b0;
        penable <= 1'b0;
        pwrite <= 1'b0;
    endtask

    task automatic apb_read(input logic [10:0] a, output logic [7:0] d);
        @(posedge pclk);
        paddr <= a;
        pwrite <= 1'b0;
        psel <= 1'b1;
        penable <= 1'b0;
        @(posedge pclk);
        penable <= 1'b1;
        // Данные чтения комбинационные; снимаем их в середине access-фазы.
        @(negedge pclk);
        d = prdata;
        @(posedge pclk);
        psel <= 1'b0;
        penable <= 1'b0;
    endtask

    logic [7:0] got;

    initial begin
        $dumpfile("stacker_apb.vcd");
        $dumpvars(0, tb);

        repeat (3) @(posedge pclk);
        presetn <= 1'b1;
        repeat (2) @(posedge pclk);

        if (pready !== 1'b1) $fatal(1, "pready обязан быть 1 (состояний ожидания нет)");
        if (pslverr !== 1'b0) $fatal(1, "pslverr обязан быть 0");

        // 1. Однобитный входной регистр.
        apb_write(11'h300, 8'h01);
        apb_read(11'h300, got);
        if (got !== 8'h01)
            $fatal(1, "0x300: ожидалось 0x01, получено 0x%02h", got);
        $display("OK: sense_loaded (0x300) = 0x%02h", got);

        // 2. Восьмибитный входной регистр.
        apb_write(11'h102, 8'h07);
        apb_read(11'h102, got);
        if (got !== 8'h07)
            $fatal(1, "0x102: ожидалось 0x07, получено 0x%02h", got);
        $display("OK: task_stack_no (0x102) = 0x%02h", got);

        // 3. Выходной регистр автомата виден шиной.
        apb_read(11'h600, got);
        $display("OK: cmd_ack (0x600) = 0x%02h", got);

        // 4. Запись по адресу без порта игнорируется и не портит соседей.
        apb_write(11'h7ff, 8'hff);
        apb_read(11'h102, got);
        if (got !== 8'h07)
            $fatal(1, "запись по чужому адресу испортила 0x102: 0x%02h", got);
        $display("OK: запись по 0x7ff проигнорирована, 0x102 цел");

        $display("TICK: адаптер APB прошёл все проверки");
        $finish;
    end
endmodule
