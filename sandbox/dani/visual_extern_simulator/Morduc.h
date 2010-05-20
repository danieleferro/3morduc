#include "rpc/types.h"

struct TRPCStructLaser {
	double TimeStamp;
	int RemainingItems;
	int Distances[181];
	char Reflections[181];
};
typedef struct TRPCStructLaser TRPCStructLaser;
#ifdef __cplusplus
extern "C" {
bool_t xdr_TRPCStructLaser(...);
}
#else
bool_t xdr_TRPCStructLaser();
#endif


struct TRPCStructEncoder {
	double TimeStamp;
	int RemainingItems;
	int EncDx;
	int EncSx;
};
typedef struct TRPCStructEncoder TRPCStructEncoder;
#ifdef __cplusplus
extern "C" {
bool_t xdr_TRPCStructEncoder(...);
}
#else
bool_t xdr_TRPCStructEncoder();
#endif


struct TRPCStructSonar {
	double TimeStamp;
	int RemainingItems;
	int Distances[8];
};
typedef struct TRPCStructSonar TRPCStructSonar;
#ifdef __cplusplus
extern "C" {
bool_t xdr_TRPCStructSonar(...);
}
#else
bool_t xdr_TRPCStructSonar();
#endif


struct TRPCStructIR {
	double TimeStamp;
	int RemainingItems;
	int Distances[16];
};
typedef struct TRPCStructIR TRPCStructIR;
#ifdef __cplusplus
extern "C" {
bool_t xdr_TRPCStructIR(...);
}
#else
bool_t xdr_TRPCStructIR();
#endif


struct TRPCStructBumpers {
	double TimeStamp;
	int Bumpers0_31;
};
typedef struct TRPCStructBumpers TRPCStructBumpers;
#ifdef __cplusplus
extern "C" {
bool_t xdr_TRPCStructBumpers(...);
}
#else
bool_t xdr_TRPCStructBumpers();
#endif


struct TRPCStructWebCam {
	double TimeStamp;
	char Number;
	int Dimension;
	char Data[10000];
};
typedef struct TRPCStructWebCam TRPCStructWebCam;
#ifdef __cplusplus
extern "C" {
bool_t xdr_TRPCStructWebCam(...);
}
#else
bool_t xdr_TRPCStructWebCam();
#endif


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
typedef struct TRPCStructRobotCMD TRPCStructRobotCMD;
#ifdef __cplusplus
extern "C" {
bool_t xdr_TRPCStructRobotCMD(...);
}
#else
bool_t xdr_TRPCStructRobotCMD();
#endif


struct TRPCStructIO_IN {
	double TimeStamp;
	u_int DigInput0_31;
	double AnInput[8];
	double Temperature;
};
typedef struct TRPCStructIO_IN TRPCStructIO_IN;
#ifdef __cplusplus
extern "C" {
bool_t xdr_TRPCStructIO_IN(...);
}
#else
bool_t xdr_TRPCStructIO_IN();
#endif


struct TRPCStructIO_OUT {
	double TimeStamp;
	u_int DigOutput0_31;
	double AnOutput[2];
};
typedef struct TRPCStructIO_OUT TRPCStructIO_OUT;
#ifdef __cplusplus
extern "C" {
bool_t xdr_TRPCStructIO_OUT(...);
}
#else
bool_t xdr_TRPCStructIO_OUT();
#endif


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
	u_int Flags;
};
typedef struct TRPCStructLoc TRPCStructLoc;
#ifdef __cplusplus
extern "C" {
bool_t xdr_TRPCStructLoc(...);
}
#else
bool_t xdr_TRPCStructLoc();
#endif


struct TRPCStructMap {
	double TimeStamp;
	double TopPosX;
	double TopPosY;
	double ScalePPM;
	int DimX;
	int DimY;
	char Map[10000];
};
typedef struct TRPCStructMap TRPCStructMap;
#ifdef __cplusplus
extern "C" {
bool_t xdr_TRPCStructMap(...);
}
#else
bool_t xdr_TRPCStructMap();
#endif


typedef TRPCStructLaser TRPCLaser;
#ifdef __cplusplus
extern "C" {
bool_t xdr_TRPCLaser(...);
}
#else
bool_t xdr_TRPCLaser();
#endif


typedef TRPCStructEncoder TRPCEncoder;
#ifdef __cplusplus
extern "C" {
bool_t xdr_TRPCEncoder(...);
}
#else
bool_t xdr_TRPCEncoder();
#endif


typedef TRPCStructSonar TRPCSonar;
#ifdef __cplusplus
extern "C" {
bool_t xdr_TRPCSonar(...);
}
#else
bool_t xdr_TRPCSonar();
#endif


typedef TRPCStructWebCam TRPCWebCam;
#ifdef __cplusplus
extern "C" {
bool_t xdr_TRPCWebCam(...);
}
#else
bool_t xdr_TRPCWebCam();
#endif


typedef TRPCStructRobotCMD TRPCRobotCMD;
#ifdef __cplusplus
extern "C" {
bool_t xdr_TRPCRobotCMD(...);
}
#else
bool_t xdr_TRPCRobotCMD();
#endif


typedef TRPCStructIR TRPCIR;
#ifdef __cplusplus
extern "C" {
bool_t xdr_TRPCIR(...);
}
#else
bool_t xdr_TRPCIR();
#endif


typedef TRPCStructBumpers TRPCBumpers;
#ifdef __cplusplus
extern "C" {
bool_t xdr_TRPCBumpers(...);
}
#else
bool_t xdr_TRPCBumpers();
#endif


typedef TRPCStructIO_IN TRPCIO_IN;
#ifdef __cplusplus
extern "C" {
bool_t xdr_TRPCIO_IN(...);
}
#else
bool_t xdr_TRPCIO_IN();
#endif


typedef TRPCStructIO_OUT TRPCIO_OUT;
#ifdef __cplusplus
extern "C" {
bool_t xdr_TRPCIO_OUT(...);
}
#else
bool_t xdr_TRPCIO_OUT();
#endif


typedef TRPCStructLoc TRPCLoc;
#ifdef __cplusplus
extern "C" {
bool_t xdr_TRPCLoc(...);
}
#else
bool_t xdr_TRPCLoc();
#endif


typedef TRPCStructMap TRPCMap;
#ifdef __cplusplus
extern "C" {
bool_t xdr_TRPCMap(...);
}
#else
bool_t xdr_TRPCMap();
#endif


#define INT_RPC ((u_long)0x4000126)
#define INT_RPC_VERS ((u_long)11)
#define REBOOT_PC ((u_long)1)
#ifdef __cplusplus
extern "C" {
extern int *reboot_pc_11(...);
}
#else
extern int *reboot_pc_11();
#endif /* __cplusplus */
#define SHUTDOWN_PC ((u_long)2)
#ifdef __cplusplus
extern "C" {
extern int *shutdown_pc_11(...);
}
#else
extern int *shutdown_pc_11();
#endif /* __cplusplus */
#define READ_LASER ((u_long)3)
#ifdef __cplusplus
extern "C" {
extern TRPCLaser *read_laser_11(...);
}
#else
extern TRPCLaser *read_laser_11();
#endif /* __cplusplus */
#define WRITE_LASER ((u_long)4)
#ifdef __cplusplus
extern "C" {
extern int *write_laser_11(...);
}
#else
extern int *write_laser_11();
#endif /* __cplusplus */
#define READ_SONAR ((u_long)5)
#ifdef __cplusplus
extern "C" {
extern TRPCSonar *read_sonar_11(...);
}
#else
extern TRPCSonar *read_sonar_11();
#endif /* __cplusplus */
#define WRITE_SONAR ((u_long)6)
#ifdef __cplusplus
extern "C" {
extern int *write_sonar_11(...);
}
#else
extern int *write_sonar_11();
#endif /* __cplusplus */
#define READ_ENCODER ((u_long)7)
#ifdef __cplusplus
extern "C" {
extern TRPCEncoder *read_encoder_11(...);
}
#else
extern TRPCEncoder *read_encoder_11();
#endif /* __cplusplus */
#define WRITE_ENCODER ((u_long)8)
#ifdef __cplusplus
extern "C" {
extern int *write_encoder_11(...);
}
#else
extern int *write_encoder_11();
#endif /* __cplusplus */
#define READ_IR ((u_long)9)
#ifdef __cplusplus
extern "C" {
extern TRPCIR *read_ir_11(...);
}
#else
extern TRPCIR *read_ir_11();
#endif /* __cplusplus */
#define WRITE_IR ((u_long)10)
#ifdef __cplusplus
extern "C" {
extern int *write_ir_11(...);
}
#else
extern int *write_ir_11();
#endif /* __cplusplus */
#define READ_BUMPERS ((u_long)11)
#ifdef __cplusplus
extern "C" {
extern TRPCBumpers *read_bumpers_11(...);
}
#else
extern TRPCBumpers *read_bumpers_11();
#endif /* __cplusplus */
#define WRITE_BUMPERS ((u_long)12)
#ifdef __cplusplus
extern "C" {
extern int *write_bumpers_11(...);
}
#else
extern int *write_bumpers_11();
#endif /* __cplusplus */
#define READ_CAM ((u_long)13)
#ifdef __cplusplus
extern "C" {
extern TRPCWebCam *read_cam_11(...);
}
#else
extern TRPCWebCam *read_cam_11();
#endif /* __cplusplus */
#define WRITE_CAM ((u_long)14)
#ifdef __cplusplus
extern "C" {
extern int *write_cam_11(...);
}
#else
extern int *write_cam_11();
#endif /* __cplusplus */
#define READ_CMD ((u_long)15)
#ifdef __cplusplus
extern "C" {
extern TRPCRobotCMD *read_cmd_11(...);
}
#else
extern TRPCRobotCMD *read_cmd_11();
#endif /* __cplusplus */
#define WRITE_CMD ((u_long)16)
#ifdef __cplusplus
extern "C" {
extern int *write_cmd_11(...);
}
#else
extern int *write_cmd_11();
#endif /* __cplusplus */
#define READ_IO_IN ((u_long)17)
#ifdef __cplusplus
extern "C" {
extern TRPCIO_IN *read_io_in_11(...);
}
#else
extern TRPCIO_IN *read_io_in_11();
#endif /* __cplusplus */
#define WRITE_IO_IN ((u_long)18)
#ifdef __cplusplus
extern "C" {
extern int *write_io_in_11(...);
}
#else
extern int *write_io_in_11();
#endif /* __cplusplus */
#define READ_IO_OUT ((u_long)19)
#ifdef __cplusplus
extern "C" {
extern TRPCIO_OUT *read_io_out_11(...);
}
#else
extern TRPCIO_OUT *read_io_out_11();
#endif /* __cplusplus */
#define WRITE_IO_OUT ((u_long)20)
#ifdef __cplusplus
extern "C" {
extern int *write_io_out_11(...);
}
#else
extern int *write_io_out_11();
#endif /* __cplusplus */
#define READ_LOC ((u_long)21)
#ifdef __cplusplus
extern "C" {
extern TRPCLoc *read_loc_11(...);
}
#else
extern TRPCLoc *read_loc_11();
#endif /* __cplusplus */
#define WRITE_LOC ((u_long)22)
#ifdef __cplusplus
extern "C" {
extern int *write_loc_11(...);
}
#else
extern int *write_loc_11();
#endif /* __cplusplus */
#define READ_MAP ((u_long)23)
#ifdef __cplusplus
extern "C" {
extern TRPCMap *read_map_11(...);
}
#else
extern TRPCMap *read_map_11();
#endif /* __cplusplus */
#define WRITE_MAP ((u_long)24)
#ifdef __cplusplus
extern "C" {
extern int *write_map_11(...);
}
#else
extern int *write_map_11();
#endif /* __cplusplus */

