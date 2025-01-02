
#define	nelem(x)	(sizeof(x)/sizeof((x)[0]))

/* the rest */
#include "lib.h"
#include "user.h"

/*
 * Time-of-day
 */

typedef
struct Tm
{
	int	sec;
	int	min;
	int	hour;
	int	mday;
	int	mon;
	int	year;
	int	wday;
	int	yday;
	char	zone[4];
	int	tzoff;
} Tm;

extern	Tm*	p9gmtime(long);
extern	Tm*	p9localtime(long);
extern	char*	p9asctime(Tm*);
extern	char*	p9ctime(long);
extern	double	p9cputime(void);
extern	long	p9times(long*);
extern	long	p9tm2sec(Tm*);
// extern	vlong	p9nsec(void);

#ifndef NOPLAN9DEFINES
#define	gmtime		p9gmtime
#define	localtime	p9localtime
#define	asctime		p9asctime
#define	ctime		p9ctime
#define	cputime		p9cputime
#define	times		p9times
#define	tm2sec		p9tm2sec
// #define	nsec		p9nsec
#endif

/*
 * one-of-a-kind
 */
enum
{
	PNPROC		= 1,
	PNGROUP		= 2,
};

extern	int	atexit(void(*)(void));
extern	void	exits(char*);
extern	int	postnote(int, int, char *);
extern	vlong	strtoll(char*, char**, int);


/*
 *  network services
 */
typedef struct NetConnInfo NetConnInfo;
struct NetConnInfo
{
	char	*dir;		/* connection directory */
	char	*root;		/* network root */
	char	*spec;		/* binding spec */
	char	*lsys;		/* local system */
	char	*lserv;		/* local service */
	char	*rsys;		/* remote system */
	char	*rserv;		/* remote service */
	char	*laddr;		/* local address */
	char	*raddr;		/* remote address */
};
extern	NetConnInfo*	getnetconninfo(char*, int);
extern	void		freenetconninfo(NetConnInfo*);

/*
 * system calls
 *
 */
#define	STATMAX	65535U	/* max length of machine-independent stat structure */
#define	DIRMAX	(sizeof(Dir)+STATMAX)	/* max length of Dir structure */
#define	ERRMAX	128	/* max length of error string */



extern	Dir*	dirstat(char*);
extern	Dir*	dirfstat(int);
extern	int	dirwstat(char*, Dir*);
extern	int	dirfwstat(int, Dir*);
extern	long	dirread(int, Dir**);
extern	void	nulldir(Dir*);
extern	long	dirreadall(int, Dir**);
extern	int	getpid(void);
extern	int	getppid(void);
extern	void	rerrstr(char*, uint);
extern	void	werrstr(char*, ...);

extern	long	p9time(long*);
