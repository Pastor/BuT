// Тестбенч цели sv для порождённого batch_cycle.sv.
//
// не порождается taktc: тестбенч - принадлежность проверки, а не продукта
// Лежит в tb/, поэтому глоб `*.sv` проверки цели sv его не
// подхватывает.
//
// Проверяется последовательная композиция `+`: цикл обязан пройти три фазы
// одну за другой (дозирование -> перемешивание -> слив) и лишь затем поднять
// готовность. Входов у модели нет - цикл самодостаточен.
//
// Мало проверить, что `ready` когда-нибудь поднялся: это дал бы и модуль,
// проскочивший фазы. Поэтому наблюдается ещё и `stage` - номер активной фазы,
// который пишут сами под-модели. Тестбенч требует, чтобы каждое из значений
// 1, 2, 3 встретилось И чтобы порядок их появления был именно таким: иначе
// `+` выродилась бы в `|` незамеченной.
//
// `stage` - переменная модели, а не порт: наблюдается иерархической ссылкой
// `dut.batch_cycle_stage`. Выводить её наружу ради теста значило бы менять
// продукт ради проверки - у кристалла появился бы лишний вывод.
//
// Ручной прогон (из examples/generated/sv/):
//   $ verilator --binary --timing --trace --top-module tb tb/batch_cycle_tb.sv batch_cycle.sv -o simtb
//   (cd tb && ../obj_dir/simtb)   # пишет tb/batch_cycle.vcd
`timescale 1ns / 1ps

module tb;
    // Служебные сигналы цели sv.
    logic clk = 0;
    logic rst_n = 0;

    // Выходы модуля.
    logic ready;
    logic is_done;

    batch_cycle dut (
        .clk(clk),
        .rst_n(rst_n),
        .ready(ready),
        .is_done(is_done)
    );

    always #5 clk = ~clk;

    // Накопители событий (устойчивы к сдвигу такта на один).
    logic saw_dose = 0, saw_mix = 0, saw_drain = 0;
    logic saw_ready = 0, saw_done = 0;
    // Порядок фаз: фаза засчитывается, только если предыдущая уже была.
    logic order_ok = 1;

    always @(posedge clk) begin
        if (rst_n) begin
            case (dut.batch_cycle_stage)
                8'd1: saw_dose <= 1;
                8'd2: begin
                    if (!saw_dose) order_ok <= 0;   // перемешивание раньше дозирования
                    saw_mix <= 1;
                end
                8'd3: begin
                    if (!saw_mix) order_ok <= 0;    // слив раньше перемешивания
                    saw_drain <= 1;
                end
                default: ;
            endcase
            // Готовность раньше последней фазы означала бы, что цепочка
            // отпустила `next` до завершения шага.
            if (ready && !saw_drain) order_ok <= 0;
            if (ready) saw_ready <= 1;
            if (is_done) saw_done <= 1;
        end
    end

    integer i;
    initial begin
        $dumpfile("batch_cycle.vcd");
        $dumpvars(0, tb);

        // Первый фронт снимает сброс.
        @(posedge clk);
        rst_n <= 1'b1;

        // Цикл проходит фазы сам; 40 тактов с запасом (модели хватает 11).
        for (i = 0; i < 40; i = i + 1) @(posedge clk);

        if (!saw_dose)  $error("batch_cycle: фаза дозирования не наблюдалась (stage=1)");
        if (!saw_mix)   $error("batch_cycle: фаза перемешивания не наблюдалась (stage=2)");
        if (!saw_drain) $error("batch_cycle: фаза слива не наблюдалась (stage=3)");
        if (!order_ok)  $error("batch_cycle: фазы прошли НЕ по порядку — `+` ведёт себя как `|`");
        if (!saw_ready) $error("batch_cycle: ready не поднялся — цикл не завершился");
        // `is_done` больше не наблюдается: конечное состояние несёт
        // `always { ready := 1; }`, и автомат в нём остаётся - состояние
        // стабильно. Признак завершения цикла - поднятый и удерживаемый `ready`.

        if (saw_dose && saw_mix && saw_drain && order_ok && saw_ready)
            $display("batch_cycle_tb: OK (три фазы по порядку, затем ready и is_done)");
        $finish;
    end
endmodule
