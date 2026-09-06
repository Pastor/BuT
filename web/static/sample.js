// Модель, с которой открывается пустой редактор.

/** Стартовая модель: термореле на двух состояниях. */
export const SAMPLE = `// Термореле: греет, пока холодно, и ждёт, пока не остынет.
in temperature: u8;
out heater: bit;

const HOT := 24;
const COLD := 20;

start Heating {
    always {
        heater := 1;
    }

    ref Cooling: temperature >= HOT;
}

state Cooling {
    always {
        heater := 0;
    }

    ref Heating: temperature <= COLD;
}
`;
