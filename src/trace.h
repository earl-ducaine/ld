

#ifndef TRACE_H
#define TRACE_H

// probably we'll need one for each number of parameters and type,
// e.g.  log1d("With on number %d ~n" "1")
void trace_log(const char* message);
void trace_log_1s(const char* message, char* s);
void trace_log_1u(const char* message, unsigned i);
void trace_log_2u(const char* message, unsigned i, unsigned j);
void trace_log_3u(const char* message, unsigned i, unsigned j, unsigned k);

void trace_log_4u(const char* message, unsigned i, unsigned j, unsigned k,
		  unsigned l);

void trace_log_5u(const char* message, unsigned i, unsigned j, unsigned k,
		  unsigned l, unsigned m);

void trace_log_6u(const char* message, unsigned i, unsigned j, unsigned k,
		  unsigned l, unsigned m, unsigned n);

#endif // TRACE_H
