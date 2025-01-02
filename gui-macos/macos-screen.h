
void	setmouse(Point);

void	setcursors(Cursor*, Cursor2*);
void	setcursor(Cursor*);
static void setcursor0(NSCursor*);

void	setlabel(char*);

char 	*getsnarf(void);
void	putsnarf(char*);

void	topwin(void);

void	keystroke(int);
void	kicklabel(char*);

void	zlock(void);
void	zunlock(void);

void resizeimg(void);

Rectangle mouserect;

int mouseresized;
void resizewindow(Rectangle);

void initcpu(void);
