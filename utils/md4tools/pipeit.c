/*
**  Utility routines to interface to the MD4 program as a filter.
*/
#include <stdio.h>
#include "md4_def.h"

#ifdef	USE_STRCHR
#define RDX	strrchr
#else
#define RDX	rindex
#endif	/* USE_STRCHR */

static char	OutputFile[] = "/tmp/hashcodeXXXXXX";
static char	ChecksumBuffer[HDRTEXTSIZE + 2];
static FILE	*Stream;

extern char	*RDX();
extern char	*mktemp();
#ifdef	CHARPSPRINTF
extern char	*sprintf();
#endif	/* CHARPSPRINTF */


/*
**  Spawn a MD4 that has its output redirected.
*/
FILE *
Md4Open()
{
    char	buff[sizeof OutputFile + 20];

    /* Open stream to md4. */
    (void)mktemp(OutputFile);
    (void)sprintf(buff, "md4 >%s", OutputFile);
    if ((Stream = popen(buff, "w")) == NULL)
	(void)unlink(OutputFile);
    return Stream;
}


/*
**  Close the pipe and read in the Md4's output.
*/
char *
Md4Close()
{
    FILE	*F;
    char	*p;

    (void)pclose(Stream);

    /* Open the output file, read the one line. */
    if ((F = fopen(OutputFile, "r")) == NULL)
	return NULL;
    p = fgets(ChecksumBuffer, sizeof ChecksumBuffer, F);
    (void)fclose(F);
    (void)unlink(OutputFile);
    if (p == NULL)
	return NULL;

    /* Kill the newline. */
    if ((p = RDX(ChecksumBuffer, '\n')) == NULL)
	return NULL;
    *p = '\0';
    return ChecksumBuffer;
}
