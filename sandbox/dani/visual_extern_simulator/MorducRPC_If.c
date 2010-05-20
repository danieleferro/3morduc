
#include "./rpc/rpc.h"
#include "MorducRPC_If.h"

CLIENT *cl = 0;

int RPC_OpenConn(char *ServerName)
{
	char ServerName1[30];

	// Check connection status
	if (cl!=0) return 0;

	//init rpc;
	rpc_nt_init();

	//Creation Object Client and open connection
	strcpy(ServerName1, ServerName);
	cl = clnt_create(&ServerName1, INT_RPC, INT_RPC_VERS, "tcp");

	return (cl!=0);
}



int RPC_Read_Laser(TRPCLaser *DestData, int FromList)
{
	TRPCLaser *Result;
	static int i;

	i=FromList;
	Result = read_laser_11(&i,cl);
	if (Result->TimeStamp == 0) return 0;
	memcpy(DestData, Result, sizeof(TRPCLaser));
	return 1;
}

int RPC_Write_Laser(TRPCLaser *SourceData)
{
	int *Result;
	static TRPCLaser Data;

	memcpy(&Data, SourceData, sizeof(TRPCLaser));
	Result = write_laser_11(&Data,cl);
	if (Result == 0) return 0;
	return 1;
}

int RPC_Read_Sonar(TRPCSonar *DestData,int FromList)
{
	TRPCSonar *Result;
	static int i;

	i=FromList;
	Result = read_sonar_11(&i,cl);
	if (Result->TimeStamp == 0) return 0;
	memcpy(DestData, Result, sizeof(TRPCSonar));
	return 1;
}

int RPC_Write_Sonar(TRPCSonar *SourceData)
{
	int *Result;
	static TRPCSonar Data;

	memcpy(&Data, SourceData, sizeof(TRPCSonar));
	Result = write_sonar_11(&Data,cl);
	if (Result == 0) return 0;
	return 1;
}

int RPC_Read_Encoder(TRPCEncoder *DestData)
{
	TRPCEncoder *Result;

	Result = read_encoder_11(0,cl);
	if (Result->TimeStamp == 0) return 0;
	memcpy(DestData, Result, sizeof(TRPCEncoder));
	return 1;
}

int RPC_Write_Encoder(TRPCEncoder *SourceData)
{
	int *Result;
	static TRPCEncoder Data;

	memcpy(&Data, SourceData, sizeof(TRPCEncoder));
	Result = write_encoder_11(&Data,cl);
	if (Result == 0) return 0;
	return 1;
}

int RPC_Read_Cam(TRPCWebCam *DestData,char cam)
{
	TRPCWebCam *Result;
	char N;

	N=cam;
	Result = read_cam_11(&N,cl);
	if (Result->TimeStamp == 0) return 0;
	memcpy(DestData, Result, sizeof(TRPCWebCam));
	return 1;
}

int RPC_Write_Cam(TRPCWebCam *SourceData, char cam)
{
	int *Result;
	static TRPCWebCam Data;

	memcpy(&Data, SourceData, sizeof(TRPCWebCam));
	Data.Number=cam;
	Result = write_cam_11(&Data,cl);
	if (Result == 0) return 0;
	return 1;
}

int RPC_Read_CMD(TRPCRobotCMD *DestData)
{
	TRPCRobotCMD *Result;

	Result = read_cmd_11(0,cl);
	if (Result == 0) return 0; //Vedere se va bene
	memcpy(DestData, Result, sizeof(TRPCRobotCMD));
	return 1;
}

int RPC_Write_CMD(TRPCRobotCMD *SourceData)
{
	int *Result;
	static TRPCRobotCMD Data;

	memcpy(&Data, SourceData, sizeof(TRPCRobotCMD));
	Result = write_cmd_11(&Data,cl);
	if (Result == 0) return 0;
	return 1;
}

int RPC_Reboot(void)
{
	int *Result;

	Result = reboot_pc_11(0,cl);
	if (Result == 0) return 0;
	return 1;
}

int RPC_ShutDown(void)
{
	int *Result;

	Result = shutdown_pc_11(0,cl);
	if (Result == 0) return 0;
	return 1;
}

int RPC_CloseConn(void)
{
	// Check connection status
	if (cl==0) return 0;

	//close connection and destroy object client
	clnt_destroy(cl);
	cl=0;

	//close rpc
	rpc_nt_exit();

	//cl = 0;
	return 1;
}




int RPC_Read_IR(TRPCIR *DestData, int FromList)
{
	TRPCIR *Result;
	static int i;

	i=FromList;
	Result = read_ir_11(&i,cl);
	if (Result->TimeStamp == 0) return 0;
	memcpy(DestData, Result, sizeof(TRPCIR));
	return 1;
}

int RPC_Write_IR(TRPCIR *SourceData)
{
	int *Result;
	static TRPCIR Data;

	memcpy(&Data, SourceData, sizeof(TRPCIR));
	Result = write_ir_11(&Data,cl);
	if (Result == 0) return 0;
	return 1;
}

int RPC_Read_Map(TRPCMap *DestData)
{
	TRPCMap *Result;

	Result = read_map_11(0,cl);
	if (Result->TimeStamp == 0) return 0;
	memcpy(DestData, Result, sizeof(TRPCMap));
	return 1;
}

int RPC_Write_Map(TRPCMap *SourceData)
{
	int *Result;
	static TRPCMap Data;

	memcpy(&Data, SourceData, sizeof(TRPCMap));
	Result = write_map_11(&Data,cl);
	if (Result == 0) return 0;
	return 1;
}


int RPC_Read_Bumpers(TRPCBumpers *DestData)
{
	TRPCBumpers *Result;

	Result = read_bumpers_11(0,cl);
	if (Result->TimeStamp == 0) return 0;
	memcpy(DestData, Result, sizeof(TRPCBumpers));
	return 1;
}

int RPC_Write_Bumpers(TRPCBumpers *SourceData)
{
	int *Result;
	static TRPCBumpers Data;

	memcpy(&Data, SourceData, sizeof(TRPCBumpers));
	Result = write_bumpers_11(&Data,cl);
	if (Result == 0) return 0;
	return 1;
}


int RPC_Read_Loc(TRPCLoc *DestData)
{
	TRPCLoc *Result;

	Result = read_loc_11(0,cl);
	if (Result->TimeStamp == 0) return 0;
	memcpy(DestData, Result, sizeof(TRPCLoc));
	return 1;
}

int RPC_Write_Loc(TRPCLoc *SourceData)
{
	int *Result;
	static TRPCLoc Data;

	memcpy(&Data, SourceData, sizeof(TRPCLoc));
	Result = write_loc_11(&Data,cl);
	if (Result == 0) return 0;
	return 1;
}

int RPC_Read_IO_IN(TRPCIO_IN *DestData)
{
	TRPCIO_IN *Result;

	Result = read_io_in_11(0,cl);
	if (Result->TimeStamp == 0) return 0;
	memcpy(DestData, Result, sizeof(TRPCIO_IN));
	return 1;
}

int RPC_Write_IO_IN(TRPCIO_IN *SourceData)
{
	int *Result;
	static TRPCIO_IN Data;

	memcpy(&Data, SourceData, sizeof(TRPCIO_IN));
	Result = write_io_in_11(&Data,cl);
	if (Result == 0) return 0;
	return 1;
}


int RPC_Read_IO_OUT(TRPCIO_OUT *DestData)
{
	TRPCIO_OUT *Result;

	Result = read_io_out_11(0,cl);
	if (Result->TimeStamp == 0)
			return 0;
	memcpy(DestData, Result, sizeof(TRPCIO_OUT));
	return 1;
}

int RPC_Write_IO_OUT(TRPCIO_OUT *SourceData)
{
	int *Result;
	static TRPCIO_OUT Data;

	memcpy(&Data, SourceData, sizeof(TRPCIO_OUT));
	Result = write_io_out_11(&Data,cl);
	if (Result == 0) return 0;
	return 1;
}
