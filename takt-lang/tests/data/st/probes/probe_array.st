(* П5. ARRAY в VAR блока FB - подтверждает T12. *)
FUNCTION_BLOCK Demo
VAR
    data : ARRAY [0..3] OF USINT;
    i : USINT := 0;
END_VAR
    data[0] := 7;
    data[1] := data[0] + 1;
END_FUNCTION_BLOCK
