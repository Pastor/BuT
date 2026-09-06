// Порождено компилятором Takt (taktc) — цель: SystemVerilog (IEEE 1800).
// Не редактировать вручную: файл перезаписывается при каждой генерации.

module regulator (
    input  logic clk,
    input  logic rst_n,
    input  logic en = 1'b1,
    output logic ready,
    output logic is_done
);
    typedef enum logic [1:0] {
        REGULATOR_REGULATOR_ADJUST = 2'd0,
        REGULATOR_REGULATOR_DONE = 2'd1,
        REGULATOR_REGULATOR_SETTLED = 2'd2,
        REGULATOR_REGULATOR_END = 2'd3
    } regulator_regulator_state_e;

    typedef enum logic [0:0] {
        REGULATOR_MAIN = 1'd0,
        REGULATOR_END = 1'd1
    } regulator_state_e;

    regulator_regulator_state_e regulator_regulator_state;
    regulator_regulator_state_e regulator_regulator_state_next;
    logic signed [15:0] regulator_regulator_half;
    logic signed [15:0] regulator_regulator_half_next;
    logic signed [15:0] regulator_regulator_near;
    logic signed [15:0] regulator_regulator_near_next;
    logic signed [15:0] regulator_regulator_setpoint;
    logic signed [15:0] regulator_regulator_setpoint_next;
    logic signed [15:0] regulator_regulator_value;
    logic signed [15:0] regulator_regulator_value_next;
    regulator_state_e state;
    regulator_state_e state_next;
    logic ready_next;

    always_comb begin
        regulator_regulator_state_next = regulator_regulator_state;
        regulator_regulator_half_next = regulator_regulator_half;
        regulator_regulator_near_next = regulator_regulator_near;
        regulator_regulator_setpoint_next = regulator_regulator_setpoint;
        regulator_regulator_value_next = regulator_regulator_value;
        state_next = state;
        ready_next = ready;

        unique case (state)
            REGULATOR_MAIN: begin
                unique case (regulator_regulator_state)
                    REGULATOR_REGULATOR_ADJUST: begin
                        regulator_regulator_value_next = (16'($signed(regulator_regulator_value_next) + $signed((16'(((32'($signed(((16'($signed(regulator_regulator_setpoint_next) - $signed(regulator_regulator_value_next)))))) * 32'($signed(regulator_regulator_half_next))) >>> 8))))));
                        if ((regulator_regulator_value_next >= regulator_regulator_near_next)) begin
                            regulator_regulator_state_next = REGULATOR_REGULATOR_SETTLED;
                        end
                    end
                    REGULATOR_REGULATOR_DONE: begin
                        ready_next = 1;
                    end
                    REGULATOR_REGULATOR_SETTLED: begin
                        regulator_regulator_value_next = regulator_regulator_setpoint_next;
                        begin
                            regulator_regulator_state_next = REGULATOR_REGULATOR_DONE;
                        end
                    end
                    REGULATOR_REGULATOR_END: begin end
                endcase
                if ((regulator_regulator_state_next == REGULATOR_REGULATOR_END)) begin
                    state_next = REGULATOR_END;
                end
            end
            REGULATOR_END: begin end
        endcase
    end

    always_ff @(posedge clk) begin
        if (!rst_n) begin
            regulator_regulator_state <= REGULATOR_REGULATOR_ADJUST;
            regulator_regulator_half <= 128;
            regulator_regulator_near <= 2432;
            regulator_regulator_setpoint <= 2560;
            regulator_regulator_value <= 0;
            state <= REGULATOR_MAIN;
            ready <= '0;
        end else if (en) begin
            regulator_regulator_state <= regulator_regulator_state_next;
            regulator_regulator_half <= regulator_regulator_half_next;
            regulator_regulator_near <= regulator_regulator_near_next;
            regulator_regulator_setpoint <= regulator_regulator_setpoint_next;
            regulator_regulator_value <= regulator_regulator_value_next;
            state <= state_next;
            ready <= ready_next;
        end
    end

    assign is_done = (state == REGULATOR_END);
endmodule
