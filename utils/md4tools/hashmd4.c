/*
**  Call MD4 on something about to be feed into INEWS.  Then, rewrite
**  the file to add the X-Md4-Signature header.
*/
#include <stdio.h>
#include <pwd.h>
#include "md4_def.h"

#ifdef	USE_STRCHR
#define IDX	strchr
#else
#define IDX	index
#endif	/* USE_STRCHR */

#ifndef	SEEK_ABS
#define SEEK_ABS	0
#endif	/* SEEK_ABS */

extern char	*optarg;
extern int	optind;

extern char		*getenv();
extern char		*IDX();
extern char		*mktemp();
extern char		*Md4Close();
extern char		*strcpy();
extern FILE		*Md4Open();
extern long		ftell();
extern struct passwd	*getpwuid();
#ifdef	CHARPSPRINTF
extern char	*sprintf();
#endif	/* CHARPSPRINTF */

static void
Usage()
{
    (void)fprintf(stderr, "Usage: hashmd4 articlename\n");
    exit(1);
}


/*
**  Simulate what B2.11 inews does for appending signatures.
*/
static int
AppendSignature(Md4)
    FILE		*Md4;
{
    char		*p;
    char		buff[256];
    FILE		*F;
    int			i;
    struct passwd	*pwd;

    if ((p = getenv("HOME")) == NULL
     && (p = getenv("LOGDIR")) == NULL) {
	if ((pwd = getpwuid(getuid())) == NULL)
	    return 0;
	p = pwd->pw_dir;
    }
    (void)sprintf(buff, "%s/.signature", p);
    if ((F = fopen(buff, "r")) == NULL)
	return 0;
    for (i = 0; fgets(buff, sizeof buff, F); i++)
	if (IDX(buff, '\n') == NULL) {
	    i = 0;
	    break;
	}
    if (i > 5 || i == 0) {
	(void)fclose(F);
	return 0;
    }
    (void)fprintf(Md4, "-- \n");
    rewind(F);
    while (fgets(buff, sizeof buff, F))
	(void)fputs(buff, Md4);
    (void)fclose(F);
    return i;
}


main(ac, av)
    int		ac;
    char	*av[];
{
    int		i;
    int		CheckSignature;
    FILE	*Input;
    FILE	*Md4;
    FILE	*Output;
    FILE	*Body;
    char	buff[BUFSIZ];
    char	*p;
    char	tempfile[20];
    char	bodyfile[20];
    long	cookie;

    /* Set defaults. */
    CheckSignature = TRUE;

    /* Parse JCL. */
    while ((i = getopt(ac, av, "n")) != EOF)
	switch (i) {
	default:
	    Usage();
	case 'n':
	    CheckSignature = FALSE;
	    break;
	}

    /* Get input. */
    ac -= optind;
    av += optind;
    switch (ac) {
    default:
	Usage();
	/* NOTREACHED */
    case 0:
	/* We're being piped into.  Create a temp file to hold the
	 * article body. */
	Input = stdin;
	(void)strcpy(bodyfile, "/tmp/hashBXXXXXX");
	(void)mktemp(bodyfile);
	if ((Body = fopen(bodyfile, "w")) == NULL) {
	    perror("No temporary");
	    (void)fprintf(stderr, "Can't open \"%s\" for writing.\n",
		    bodyfile);
	    exit(1);
	}
	break;
    case 1:
	if ((Input = fopen(av[0], "r")) == NULL) {
	    perror("No input");
	    (void)fprintf(stderr, "Can't open \"%s\" for reading.\n", av[0]);
	    exit(1);
	}
	Body = NULL;
	break;
    }

    /* Get output file. */
    (void)strcpy(tempfile, "/tmp/hashHXXXXXX");
    (void)mktemp(tempfile);
    if ((Output = fopen(tempfile, "w")) == NULL) {
	perror("No output");
	(void)fprintf(stderr, "Can't open \"%s\" for writing.\n", tempfile);
	exit(1);
    }

    /* Open stream to md4. */
    if ((Md4 = Md4Open()) == NULL) {
	perror("Can't open pipe to md4");
	(void)fclose(Output);
	(void)unlink(tempfile);
	exit(1);
    }

    /* Read article, skipping headers. */
    while (fgets(buff, sizeof buff, Input)) {
	if (buff[strlen(buff) - 1] != '\n')
	    (void)fprintf(stderr, "Warning, line truncated:\n%s\n",
		    buff);
	if (buff[0] == '\n')
	    break;
	(void)fputs(buff, Output);
    }

    /* If not from stdin we can seek, so remember where the headers end. */
    if (Body == NULL)
	cookie = ftell(Input);

    /* Send rest of article to md4. */
    while (fgets(buff, sizeof buff, Input)) {
	if (buff[strlen(buff) - 1] != '\n')
	    (void)fprintf(stderr, "Warning, line truncated:\n%s\n",
		    buff);
	(void)fputs(buff, Md4);
	if (Body)
	    (void)fputs(buff, Body);
    }

    /* Do the signature? */
    if (CheckSignature) {
	if ((i = AppendSignature(Md4)) == 0)
	    (void)fprintf(stderr, ".signature unreadable or too long...\n");
    }

    (void)fclose(Input);

    /* Write the checksum. */
    if (p = Md4Close())
	(void)fprintf(Output, "%s: %s\n", CHECKSUMHDR, p);
    else
	(void)fprintf(stderr, "Md4 checksum lost!?\n");

    /* Send the article body. */
    if (Body) {
	(void)fclose(Body);
	Input = fopen(bodyfile, "r");
    }
    else {
	Input = fopen(av[0], "r");
	(void)fseek(Input, cookie, SEEK_ABS);
    }
    (void)fputs("\n", Output);
    while (fgets(buff, sizeof buff, Input))
	(void)fputs(buff, Output);
    (void)fclose(Output);

    if (Input == stdin)
	/* Input is stdin, so send output to stdout. */
	Output = stdout;
    else if ((Output = fopen(av[0], "w")) == NULL) {
	perror("Can't rewrite file");
	(void)fprintf(stderr,
		"Can't overwrite \"%s\", output is in \"%s\".\n",
		av[0], tempfile);
	exit(1);
    }

    Input = fopen(tempfile, "r");
    while (fgets(buff, sizeof buff, Input))
	(void)fputs(buff, Output);

    if (Output != stdout);
	(void)unlink(tempfile);
    (void)fclose(Output);
    exit(0);
}
