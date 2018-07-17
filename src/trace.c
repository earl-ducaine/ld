
#include <stdarg.h>
#include <stdio.h>
#include <stdbool.h>

#include "trace.h"


// int debugging_enabled = global_debugging_sdu;
int debugging_enabled = false;

void trace_log(const char* message) {
  if (debugging_enabled) {
    trace_log(message);
  }
}

void trace_log_1s(const char* message, char* s) {
  if (debugging_enabled) {
    // Sinner!
    char buf[4096] = {'\0'};
    sprintf(buf, message, s);
    trace_log(buf);
  }
}

void trace_log_1u(const char* message, unsigned i) {
  if (debugging_enabled) {
    // Sinner!
    char buf[4096] = {'\0'};
    sprintf(buf, message, i);
    trace_log(buf);
  }
}

void trace_log_2u(const char* message, unsigned i, unsigned j) {
  if (debugging_enabled) {
    // Sinner!
    char buf[4096] = {'\0'};
    sprintf(buf, message, i, j);
    trace_log(buf);
  }
}

void trace_log_3u(const char* message, unsigned i, unsigned j, unsigned k) {
  if (debugging_enabled) {
    // Sinner!
    char buf[4096] = {'\0'};
    sprintf(buf, message, i, j, k);
    trace_log(buf);
  }
}

void trace_log_4u(const char* message, unsigned i, unsigned j, unsigned k,
		 unsigned l) {
  if (debugging_enabled) {
    // Sinner!
    char buf[4096] = {'\0'};
    sprintf(buf, message, i, j, k, l);
    trace_log(buf);
  }
}

void trace_log_5u(const char* message, unsigned i, unsigned j, unsigned k,
		 unsigned l, unsigned m) {
  if (debugging_enabled) {
    // Sinner!
    char buf[4096] = {'\0'};
    sprintf(buf, message, i, j, k, l, m);
    trace_log(buf);
  }
}

void trace_log_6u(const char* message, unsigned i, unsigned j, unsigned k,
		 unsigned l, unsigned m, unsigned n) {
  if (debugging_enabled) {
    // Sinner!
    char buf[4096] = {'\0'};
    sprintf(buf, message, i, j, k, l, m, n);
    trace_log(buf);
  }
}
