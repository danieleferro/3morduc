// log file

#include <windows.h>
#include <stdio.h>
#include <time.h>

#include "gl_log.h"

static BOOL bFirstTime = TRUE;
static FILE * fp = NULL;
//////////////////////////
void GLLogMsg(const char * szMessage)
{
	if (bFirstTime)
	{
      time_t ltime;
		char szBuffer[256];

		fp = fopen("gl_log.log", "w");
		bFirstTime = FALSE;
		
		time( &ltime );
		strcpy(szBuffer, ctime(&ltime));
		strcat(szBuffer, "\n");
		fwrite(szBuffer, strlen(szBuffer), 1, fp);
	}
	else if (fp != NULL)
	{
		fp = fopen("gl_log.log", "a+");		
	}
	if (fp != NULL && !bFirstTime)
	{
		
		fwrite(szMessage, strlen(szMessage), 1, fp);

		fclose(fp);
	}
}
