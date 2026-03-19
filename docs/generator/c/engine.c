#include "engine.h"

//model: Producer
__attribute__((always_inline)) static void Engine_Producer_reset(struct Engine *engine) {
}
__attribute__((always_inline)) static void Engine_Producer_init(struct Engine *engine)  {
    Engine_Producer_reset(engine);
}
__attribute__((always_inline)) static void Engine_Producer_tick(struct Engine *engine) {
    switch (engine->producer.state) {
        case ENGINE_PRODUCER_INIT: {
            engine->consumer.ports[0] = 0x01;
            engine->consumer.ports[1] = 0x00;
            engine->producer.state = ENGINE_PRODUCER_START;
            break;
        }
        case ENGINE_PRODUCER_START: {
            if (engine->producer.ports[0] == 0xFE) {
                engine->producer.ports[1] = 0xFF;
                engine->producer.state = ENGINE_PRODUCER_END;
            }
            break;
        }
        case ENGINE_PRODUCER_END: {
            break;
        }
    }
}
__attribute__((always_inline)) static bool Engine_Producer_finished(struct Engine *engine) {
    return engine->producer.state == ENGINE_PRODUCER_END;
}
//model: Consumer
__attribute__((always_inline)) static void Engine_Consumer_reset(struct Engine *engine) {
}
__attribute__((always_inline)) static void Engine_Consumer_init(struct Engine *engine) {
    Engine_Consumer_reset(engine);
}
__attribute__((always_inline)) static void Engine_Consumer_tick(struct Engine *engine) {
    switch (engine->consumer.state) {
        case ENGINE_CONSUMER_INIT: {
            engine->consumer.ports[0] = 0x00;
            engine->consumer.ports[1] = 0x01;
            engine->consumer.state = ENGINE_CONSUMER_START;
            break;
        }
        case ENGINE_CONSUMER_START: {
            if (true) {
                engine->consumer.ports[0] = 0xFF;
                engine->consumer.state = ENGINE_CONSUMER_END;
            }
            break;
        }
        case ENGINE_CONSUMER_END: {
            break;
        }
    }
}
__attribute__((always_inline)) static bool Engine_Consumer_finished(struct Engine *engine) {
    return engine->consumer.state == ENGINE_CONSUMER_END;
}
//model: Engine_Acceptor
__attribute__((always_inline)) static void Engine_Acceptor_reset(struct Engine *engine) {
    Engine_Producer_init(engine);
    Engine_Consumer_init(engine);
}
__attribute__((always_inline)) static void Engine_Acceptor_init(struct Engine *engine) {
    Engine_Acceptor_reset(engine);
}

__attribute__((always_inline)) static void Engine_Acceptor_tick(struct Engine *engine) {
    switch (engine->state) {
        case ENGINE_ACCEPTOR_INIT: {
            engine->acceptor.ports[0] = 0x01;
            engine->acceptor.ports[1] = 0x01;
            engine->acceptor.state = ENGINE_ACCEPTOR_START;
            break;
        }
        case ENGINE_ACCEPTOR_START: {
            if (engine->producer.state == ENGINE_PRODUCER_END) {
                engine->acceptor.ports[1] = 0xFF;
                engine->acceptor.state = ENGINE_ACCEPTOR_END;
            }
            break;
        }
        case ENGINE_ACCEPTOR_END: {
            break;
        }
    }
}

__attribute__((always_inline)) static bool Engine_Acceptor_finished(struct Engine *engine) {
    return engine->acceptor.state == ENGINE_ACCEPTOR_END;
}


//model: Engine
void Engine_init(struct Engine *engine) {
    Engine_reset(engine);
}

void Engine_tick(struct Engine *engine) {
    switch (engine->state) {
        case ENGINE_IMPLEMENT_INIT: {
            Engine_Producer_init(engine);
            Engine_Consumer_init(engine);
            Engine_Acceptor_init(engine);
            engine->state = ENGINE_IMPLEMENT_TICK;
            break;
        }
        case ENGINE_IMPLEMENT_TICK: {
            if (!(Engine_Producer_finished(engine) && Engine_Consumer_finished(engine))) {
                Engine_Producer_tick(engine);
                Engine_Consumer_tick(engine);
            } else if (!Engine_Acceptor_finished(engine)) {
                Engine_Acceptor_tick(engine);
            } else {
                engine->state = ENGINE_IMPLEMENT_END;
            }
            break;
        }
        case ENGINE_IMPLEMENT_END: {
            break;
        }
    }
}

bool Engine_finished(struct Engine *engine) {
    return engine->state == ENGINE_IMPLEMENT_END;
}

void Engine_reset(struct Engine *engine) {
    engine->state = ENGINE_IMPLEMENT_INIT;
    engine->producer.state = ENGINE_PRODUCER_START;
    engine->producer.ports[0] = 0x00;
    engine->producer.ports[1] = 0x01;
    engine->consumer.state = ENGINE_CONSUMER_START;
    engine->consumer.ports[0] = 0x01;
    engine->consumer.ports[1] = 0x00;
    Engine_Producer_reset(engine);
    Engine_Consumer_reset(engine);
    Engine_Acceptor_reset(engine);
}
