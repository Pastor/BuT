#include "engine.h"

int main(void) {
    struct Engine engine;

    Engine_init(&engine);
    while (!Engine_finished(&engine)) {
        Engine_tick(&engine);
    }
    Engine_reset(&engine);
    {
        printf("Engine: %lu\n", sizeof(engine));
        printf("Engine.state: %lu\n", sizeof(engine.state));
        printf("Engine.producer: %lu\n", sizeof(engine.producer));
        printf("Engine.producer.state: %lu\n", sizeof(engine.producer.state));
        printf("Engine.producer.ports: %lu\n", sizeof(engine.producer.ports));
    }
    return 0;
}