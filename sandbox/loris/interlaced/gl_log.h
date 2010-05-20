#ifndef __GL_LOG
	#define __GL_LOG

	#ifdef __cplusplus
		extern "C" {
	#endif

	#define MAXQUEUEDMESSAGEBYTES		(4096*256)

	void GLLogMsg(const char * szMessage);

	#ifdef __cplusplus
		}
	#endif

#endif
