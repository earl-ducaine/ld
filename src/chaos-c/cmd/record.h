
struct	record_stream {
	FILE	*r_rfp;
	FILE	*r_wfp;
	int	r_rlen;
	int	r_wlen;
};
#define RECMAGIC "RECORD STREAM VERSION 1\215"
extern struct record_stream *recopen();
