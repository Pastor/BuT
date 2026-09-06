// Порождено компилятором Takt (taktc) — цель: SystemVerilog (IEEE 1800).
// Не редактировать вручную: файл перезаписывается при каждой генерации.

module lift (
    input  logic clk,
    input  logic rst_n,
    input  logic en = 1'b1,
    input  logic [7:0] at_floor,
    input  logic [7:0] call,
    output logic brake,
    output logic [7:0] display,
    output logic doors_open,
    output logic motor_down,
    output logic motor_up,
    output logic is_done
);
    localparam logic [7:0] lift_DWELL_TICKS = 3;

    typedef enum logic [2:0] {
        LIFT_BOARDING = 3'd0,
        LIFT_GOING_DOWN = 3'd1,
        LIFT_GOING_UP = 3'd2,
        LIFT_LEAVING = 3'd3,
        LIFT_STOPPING = 3'd4,
        LIFT_WAITING = 3'd5,
        LIFT_END = 3'd6
    } lift_state_e;

    lift_state_e state;
    lift_state_e state_next;
    logic lift_doors;
    logic lift_doors_next;
    logic [7:0] lift_dwell;
    logic [7:0] lift_dwell_next;
    logic lift_moving;
    logic lift_moving_next;
    logic brake_next;
    logic [7:0] display_next;
    logic doors_open_next;
    logic motor_down_next;
    logic motor_up_next;

    always_comb begin
        state_next = state;
        lift_doors_next = lift_doors;
        lift_dwell_next = lift_dwell;
        lift_moving_next = lift_moving;
        brake_next = brake;
        display_next = display;
        doors_open_next = doors_open;
        motor_down_next = motor_down;
        motor_up_next = motor_up;

        assert (((lift_moving_next == 0) || (lift_doors_next == 0)));
        unique case (state)
            LIFT_BOARDING: begin
                lift_dwell_next = (lift_dwell_next + 1);
                if ((lift_dwell_next >= lift_DWELL_TICKS)) begin
                    lift_doors_next = 0;
                    doors_open_next = 0;
                    state_next = LIFT_LEAVING;
                end
            end
            LIFT_GOING_DOWN: begin
                display_next = at_floor;
                if ((at_floor <= call)) begin
                    lift_moving_next = 0;
                    motor_up_next = 0;
                    motor_down_next = 0;
                    brake_next = 1;
                    state_next = LIFT_STOPPING;
                end
            end
            LIFT_GOING_UP: begin
                display_next = at_floor;
                if ((at_floor >= call)) begin
                    lift_moving_next = 0;
                    motor_up_next = 0;
                    motor_down_next = 0;
                    brake_next = 1;
                    state_next = LIFT_STOPPING;
                end
            end
            LIFT_LEAVING: begin
                begin
                    lift_moving_next = 0;
                    motor_up_next = 0;
                    motor_down_next = 0;
                    brake_next = 1;
                    lift_doors_next = 0;
                    doors_open_next = 0;
                    state_next = LIFT_WAITING;
                end
            end
            LIFT_STOPPING: begin
                begin
                    lift_doors_next = 1;
                    doors_open_next = 1;
                    lift_dwell_next = 0;
                    state_next = LIFT_BOARDING;
                end
            end
            LIFT_WAITING: begin
                display_next = at_floor;
                if ((call == at_floor)) begin
                    lift_doors_next = 1;
                    doors_open_next = 1;
                    lift_dwell_next = 0;
                    state_next = LIFT_BOARDING;
                end
                else if ((call > at_floor)) begin
                    lift_moving_next = 1;
                    brake_next = 0;
                    motor_up_next = 1;
                    state_next = LIFT_GOING_UP;
                end
                else if (((call > 0) && (call < at_floor))) begin
                    lift_moving_next = 1;
                    brake_next = 0;
                    motor_down_next = 1;
                    state_next = LIFT_GOING_DOWN;
                end
            end
            LIFT_END: begin end
        endcase
    end

    always_ff @(posedge clk) begin
        if (!rst_n) begin
            state <= LIFT_WAITING;
            lift_doors <= 0;
            lift_dwell <= 0;
            lift_moving <= 0;
            brake <= '0;
            display <= '0;
            doors_open <= '0;
            motor_down <= '0;
            motor_up <= '0;
            lift_moving <= 0;
            motor_up <= 0;
            motor_down <= 0;
            brake <= 1;
            lift_doors <= 0;
            doors_open <= 0;
        end else if (en) begin
            state <= state_next;
            lift_doors <= lift_doors_next;
            lift_dwell <= lift_dwell_next;
            lift_moving <= lift_moving_next;
            brake <= brake_next;
            display <= display_next;
            doors_open <= doors_open_next;
            motor_down <= motor_down_next;
            motor_up <= motor_up_next;
        end
    end

    assign is_done = (state == LIFT_END);
endmodule
