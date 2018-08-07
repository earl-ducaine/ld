
// #define DEBUGGING_ENABLED

#ifndef TRACE_H
#define TRACE_H


// bool debugging_enabled = false;
#define DEBUGGING_ENABLED

// e.g.  log1d("With on number %d ~n" "1")
void __trace_log(const char* message);
void __trace_log_1s(const char* message, char* s);
void __trace_log_1u(const char* message, unsigned i);
void __trace_log_2u(const char* message, unsigned i, unsigned j);
void __trace_log_3u(const char* message, unsigned i, unsigned j, unsigned k);

void __trace_log_4u(const char* message, unsigned i, unsigned j, unsigned k,
		  unsigned l);

void __trace_log_5u(const char* message, unsigned i, unsigned j, unsigned k,
		  unsigned l, unsigned m);

void __trace_log_6u(const char* message, unsigned i, unsigned j, unsigned k,
		  unsigned l, unsigned m, unsigned n);

#ifdef DEBUGGING_ENABLED

#define trace_log(message) __trace_log(message)
#define trace_log_1s(message, s) __trace_log_1s(message, s)
#define trace_log_1u(message, i) __trace_log_1u(message, i)
#define trace_log_2u(message, i, j) __trace_log_2u(message, i, j)
#define trace_log_3u(message, i, j, k) __trace_log_3u(message, i, j, k)
#define trace_log_4u(message, i, j, k, l) __trace_log_4u(message, i, j, k, l)

#define trace_log_5u(message, i, j, k, l, m) \
  __trace_log_5u(message, i, j, k, l, m)

#define trace_log_6u(message, i, j, k, l, m, n) \
  __trace_log_6u(message, i, j, k, l, m, n)

#else

#define trace_log(message)
#define trace_log_1s(message, s)
#define trace_log_1u(message, i)
#define trace_log_2u(message, i, j)
#define trace_log_3u(message, i, j, k)
#define trace_log_4u(message, i, j, k, l)
#define trace_log_5u(message, i, j, k, l, m)
#define trace_log_6u(message, i, j, k, l, m, n)

#endif

#endif // TRACE_H
