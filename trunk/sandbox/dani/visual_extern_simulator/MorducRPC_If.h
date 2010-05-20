#ifndef MORDUC_RPC_IF_H
#define MORDUC_RPC_IF_H

#include "Morduc.h"

int RPC_OpenConn(char *ServerName);
int RPC_Read_Laser(TRPCLaser *DestData, int FromList);
int RPC_Write_Laser(TRPCLaser *SourceData);
int RPC_Read_Sonar(TRPCSonar *DestData, int FromList);
int RPC_Write_Sonar(TRPCSonar *SourceData);
int RPC_Read_Encoder(TRPCEncoder *DestData);
int RPC_Write_Encoder(TRPCEncoder *SourceData);
int RPC_Read_CMD(TRPCRobotCMD *DestData);
int RPC_Write_CMD(TRPCRobotCMD *SourceData);
int RPC_Read_Cam(TRPCWebCam *DestData,char cam);
int RPC_Write_Cam(TRPCWebCam *SourceData, char cam);
int RPC_CloseConn(void);
int RPC_Reboot(void);
int RPC_ShutDown(void);
int RPC_Read_IR(TRPCIR *DestData, int Fromlist);
int RPC_Write_IR(TRPCIR *SourceData);
int RPC_Read_Map(TRPCMap *DestData);
int RPC_Write_Map(TRPCMap *SourceData);
int RPC_Read_Bumpers(TRPCBumpers *DestData);
int RPC_Write_Bumpers(TRPCBumpers *SourceData);
int RPC_Read_Loc(TRPCLoc *DestData);
int RPC_Write_Loc(TRPCLoc *SourceData);
int RPC_Read_IO_IN(TRPCIO_IN *DestData);
int RPC_Write_IO_IN(TRPCIO_IN *SourceData);
int RPC_Read_IO_OUT(TRPCIO_OUT *DestData);
int RPC_Write_IO_OUT(TRPCIO_OUT *SourceData);
#endif