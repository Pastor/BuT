(* П6. LREAL (64-битное вещественное) - подтверждает T11.
   Симулятор Takt считает в f64, поэтому LREAL, а не REAL. *)
FUNCTION_BLOCK Demo
VAR
    x : LREAL := 0.5;
END_VAR
    x := x * 2.0;
END_FUNCTION_BLOCK
