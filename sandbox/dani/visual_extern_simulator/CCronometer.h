#ifndef CRONOMETER_H
#define CRONOMETER_H

// Daniele Caltabiano
// 29/08/2002

class CCronometer {
private:
	__int64 Frequency;
	__int64 StartTime;
	__int64 LastTime;
public:
	CCronometer(void);
	void Reset(void);
	double CurrentTime(void);
	double ParzialTime(void);
};

#endif