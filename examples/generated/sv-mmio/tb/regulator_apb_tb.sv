// Тестбенч адаптера шины APB для порождённого regulator_apb.sv - ядро без
// записи.
//
// не порождается taktc: тестбенч - принадлежность проверки, а не продукта
// (решение 0045-07). Лежит в tb/, поэтому глоб `*.sv` гейта цели sv-mmio его не
// подхватывает.
//
// Чем этот тестбенч отличается от stacker_apb_tb.sv: у `regulator` все порты
// выходные, поэтому ядро доступно шине только на чтение и сигналов `reg_wdata`
// / `reg_wen` у него нет вовсе. Проверяется ровно это:
//
//   1. шина видит выходной регистр (0x600 - ready) до завершения: значение 0;
//   2. после завершения автомата шина видит 1 - регистр принадлежит модели;
//   3. цикл записи по тому же адресу завершается штатно (pready = 1,
//      pslverr = 0) и значение не меняет: выход принадлежит автомату, а не
//      шине. Это и есть смысл ядра без записи;
//   4. чтение по адресу без порта даёт 0.
//
// Порядок проверок 2 и 3 не случаен: `regulator` сходится за считанные такты, и
// попытка записи до завершения ничего бы не доказывала - значение сменил бы сам
// автомат за три такта цикла APB (проверено: первая редакция тестбенча падала
// именно на этом).
//
// Ни линт, ни синтез этого не доказывают: оба инструмента SV принимают модуль,
// который не работает (урок ADR 0045). Здесь идут настоящие циклы APB.
//
// внимание: слово "verilator" в начале строки комментария недопустимо -
// инструмент принимает такую строку за свою прагму и отвергает файл
// (`BADVLTPRAGMA`).

`timescale 1ns / 1ps

module tb;
    logic        pclk = 1'b0;
    logic        presetn = 1'b0;
    logic [10:0] paddr = '0;
    logic        psel = 1'b0;
    logic        penable = 1'b0;
    logic        pwrite = 1'b0;
    logic [0:0]  pwdata = '0;
    logic [0:0]  prdata;
    logic        pready;
    logic        pslverr;
    logic        en = 1'b1;
    logic        is_done;

    regulator_apb dut (
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
    // (psel, penable). При pready = 1 access длится один такт.
    task automatic apb_write(input logic [10:0] a, input logic [0:0] d);
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

    task automatic apb_read(input logic [10:0] a, output logic [0:0] d);
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

    logic [0:0] got;

    initial begin
        $dumpfile("regulator_apb.vcd");
        $dumpvars(0, tb);

        repeat (3) @(posedge pclk);
        presetn <= 1'b1;
        repeat (2) @(posedge pclk);

        if (pready !== 1'b1) $fatal(1, "pready обязан быть 1 (состояний ожидания нет)");
        if (pslverr !== 1'b0) $fatal(1, "pslverr обязан быть 0");

        // 1. Пока регулятор не сошёлся, выходной регистр читается нулём.
        apb_read(11'h600, got);
        if (got !== 1'b0)
            $fatal(1, "0x600 до завершения: ожидался 0, получено %0d", got);
        $display("OK: ready (0x600) до завершения = %0d", got);

        // 2. Даём автомату сойтись - шина обязана увидеть новое значение.
        //
        // Наблюдаемое - значение регистра, а не `is_done`: конечное
        // состояние регулятора несёт `always { ready := 1; }`, и автомат в нём
        // остаётся - состояние стабильно, покидают его только по переходу.
        repeat (64) @(posedge pclk);
        apb_read(11'h600, got);
        if (got !== 1'b1)
            $fatal(1, "0x600 после завершения: ожидался 1, получено %0d", got);
        $display("OK: ready (0x600) после завершения = %0d", got);

        // 3. Цикл записи по выходному регистру: писать нечего, но шина вправе
        //    попытаться - обёртка обязана завершить цикл штатно, а значение
        //    остаться тем, что вычислил автомат. Это и есть смысл ядра без
        //    записи: выход принадлежит модели, а не шине.
        apb_write(11'h600, 1'b0);
        if (pready !== 1'b1) $fatal(1, "pready после записи обязан остаться 1");
        if (pslverr !== 1'b0) $fatal(1, "pslverr после записи обязан остаться 0");
        apb_read(11'h600, got);
        if (got !== 1'b1)
            $fatal(1, "0x600 после записи: шина не вправе менять выходной регистр");
        $display("OK: запись в выходной регистр значение не изменила");

        // 4. Адрес без порта читается нулём.
        apb_read(11'h7ff, got);
        if (got !== 1'b0)
            $fatal(1, "0x7ff: адрес без порта обязан читаться нулём");
        $display("OK: адрес без порта (0x7ff) = %0d", got);

        $display("regulator_apb_tb: ВСЕ ПРОВЕРКИ ПРОЙДЕНЫ");
        $finish;
    end
endmodule
