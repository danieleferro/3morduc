struct TRPCStructLaser {
	double TimeStamp;
	int RemainingItems;
	int Distances[181];
	char Reflections[181];
};

struct TRPCStructEncoder {
	double TimeStamp;
	int RemainingItems;
	int EncDx;
	int EncSx;
};

struct TRPCStructSonar {
	double TimeStamp;
	int RemainingItems;
	int Distances[8];
};

struct TRPCStructIR {
	double TimeStamp;
	int RemainingItems;
	int Distances[16];
};

struct TRPCStructBumpers {
	double TimeStamp;
	int Bumpers0_31;
};

struct TRPCStructWebCam {
	double TimeStamp;
	char Number;
	int Dimension;
	char Data[10000];
};

struct TRPCStructRobotCMD {
	double TimeStamp;
	char EmergencyStop;
	double V;
	double W;
	char ShowWebCAM;
	char UseWebCAM;
	char UseSonars;
	char UseLaser;
	char UseBumpers;
	char AvoidObstaclesSonars;
	char AvoidObstaclesLaser;
	char AvoidObstaclesBumpers;
};

struct TRPCStructIO_IN {
	double TimeStamp;
	unsigned int DigInput0_31;
	double AnInput[8];
	double Temperature;
};

struct TRPCStructIO_OUT {
	double TimeStamp;
	unsigned int DigOutput0_31;
	double AnOutput[2];
};

struct TRPCStructLoc {
	double X;
	double Y;
	double Z;
	double Theta;
	double Pitch;
	double Roll;
	double Lin_Vel;
	double Ang_Vel;
	double SigmaX;
	double SigmaY;
	double SigmaZ;
	double SigmaTheta;
	double SigmaPitch;
	double SigmaRoll;
	double TimeStamp;
	unsigned int Flags;
};

struct TRPCStructMap {
	double TimeStamp;
	double TopPosX;
	double TopPosY;
	double ScalePPM;
	int DimX;
	int DimY;
	char Map[10000];
};

typedef struct TRPCStructLaser 		TRPCLaser;
typedef struct TRPCStructEncoder 	TRPCEncoder;
typedef struct TRPCStructSonar 		TRPCSonar;
typedef struct TRPCStructWebCam 	TRPCWebCam;
typedef struct TRPCStructRobotCMD	TRPCRobotCMD;
typedef struct TRPCStructIR 		TRPCIR;
typedef struct TRPCStructBumpers	TRPCBumpers;
typedef struct TRPCStructIO_IN 		TRPCIO_IN;
typedef struct TRPCStructIO_OUT 	TRPCIO_OUT;
typedef struct TRPCStructLoc		TRPCLoc;
typedef struct TRPCStructMap		TRPCMap;

program INT_RPC {
	version INT_RPC_VERS {
		int          REBOOT_PC     (void)			= 1;
		int          SHUTDOWN_PC   (void)			= 2;
		TRPCLaser    READ_LASER    (int)			= 3;
		int          WRITE_LASER   (TRPCLaser)		= 4;
		TRPCSonar    READ_SONAR    (int)			= 5;
		int          WRITE_SONAR   (TRPCSonar)		= 6;
		TRPCEncoder  READ_ENCODER  (void)			= 7;
		int          WRITE_ENCODER (TRPCEncoder)	= 8;
		TRPCIR       READ_IR       (int)			= 9;
		int          WRITE_IR      (TRPCIR)			= 10;
		TRPCBumpers  READ_BUMPERS  (void)			= 11;
		int          WRITE_BUMPERS (TRPCBumpers)	= 12;
		TRPCWebCam   READ_CAM      (char)			= 13;
		int          WRITE_CAM     (TRPCWebCam)		= 14;
		TRPCRobotCMD READ_CMD      (void)			= 15;
		int          WRITE_CMD     (TRPCRobotCMD)	= 16;
		TRPCIO_IN    READ_IO_IN    (void)			= 17;
		int          WRITE_IO_IN   (TRPCIO_IN)		= 18;
		TRPCIO_OUT   READ_IO_OUT   (void)			= 19;
		int          WRITE_IO_OUT  (TRPCIO_OUT)		= 20;
		TRPCLoc	     READ_LOC 	   (void)			= 21;
		int		     WRITE_LOC 	   (TRPCLoc)		= 22;
		TRPCMap	     READ_MAP 	   (void)			= 23;
		int		     WRITE_MAP 	   (TRPCMap)		= 24;
	} = 11;
} = 0x4000126;

