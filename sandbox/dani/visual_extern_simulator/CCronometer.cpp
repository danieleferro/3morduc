#include "stdafx.h"
#include "CCronometer.h"


extern "C" {
	#include <windows.h>
}

// Daniele Caltabiano
// 29/08/2002

CCronometer::CCronometer(void)
{
	Reset();
}

void CCronometer::Reset(void)
{
	LARGE_INTEGER li;

	QueryPerformanceFrequency(&li); // 3579545 (P4@1700MHz)
	Frequency = li.QuadPart;
	QueryPerformanceCounter(&li);
	StartTime = li.QuadPart;
	LastTime = StartTime;
}

double CCronometer::CurrentTime(void)
{
	LARGE_INTEGER li;
	double App;

	QueryPerformanceCounter(&li);
	App = (double)(li.QuadPart - StartTime);
	App = App / Frequency;
	return App;
}

double CCronometer::ParzialTime(void)
{
	LARGE_INTEGER li;
	double App;

	QueryPerformanceCounter(&li);
	App = (double)(li.QuadPart - LastTime);
	App /= Frequency;
	LastTime = li.QuadPart;
	return App;
}
