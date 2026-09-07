(* П9. Внешняя функция без тела - аналог `extern fn` языка Takt
   (examples/elevator.takt:93-115). Решает РИ5: выразим ли extern в ST.
   В IEC 61131-3 FUNCTION обязана иметь тело; проба выясняет, что скажет iec2c
   на объявление без тела. *)
FUNCTION ExternalCalc : USINT
VAR_INPUT
    arg : USINT;
END_VAR
END_FUNCTION

FUNCTION_BLOCK Demo
VAR
    r : USINT := 0;
END_VAR
    r := ExternalCalc(arg := 5);
END_FUNCTION_BLOCK
