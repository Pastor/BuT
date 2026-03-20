#pragma once
#ifndef ENGINE_H__
#define ENGINE_H__

#include <stdbool.h>
#include <stdint.h>

typedef uint8_t u8;

struct Engine {
  struct {
    enum {
      ENGINE_PRODUCER_INIT,
      ENGINE_PRODUCER_START,
      ENGINE_PRODUCER_END,
    } state;
    uint8_t ports[2];
  } producer;
  struct {
    enum {
      ENGINE_CONSUMER_INIT,
      ENGINE_CONSUMER_START,
      ENGINE_CONSUMER_END,
    } state;
    uint8_t ports[2];
  } consumer;
  struct {
    enum {
      ENGINE_ACCEPTOR_INIT,
      ENGINE_ACCEPTOR_START,
      ENGINE_ACCEPTOR_END,
    } state;
    uint8_t ports[2];
  } acceptor;
  enum {
    ENGINE_IMPLEMENT_INIT,
    ENGINE_IMPLEMENT_TICK,
    ENGINE_IMPLEMENT_END
  } state;
  uint8_t ports;
};

void Engine_init(struct Engine *engine);
void Engine_tick(struct Engine *engine);
bool Engine_finished(struct Engine *engine);
void Engine_reset(struct Engine *engine);

#endif /* ENGINE_H__ */
