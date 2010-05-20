#include "rpc/rpc.h"
#include "Morduc.h"


bool_t
xdr_TRPCStructLaser(xdrs, objp)
	XDR *xdrs;
	TRPCStructLaser *objp;
{
	if (!xdr_double(xdrs, &objp->TimeStamp)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->RemainingItems)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->Distances, 181, sizeof(int), xdr_int)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->Reflections, 181, sizeof(char), xdr_char)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_TRPCStructEncoder(xdrs, objp)
	XDR *xdrs;
	TRPCStructEncoder *objp;
{
	if (!xdr_double(xdrs, &objp->TimeStamp)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->RemainingItems)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->EncDx)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->EncSx)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_TRPCStructSonar(xdrs, objp)
	XDR *xdrs;
	TRPCStructSonar *objp;
{
	if (!xdr_double(xdrs, &objp->TimeStamp)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->RemainingItems)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->Distances, 8, sizeof(int), xdr_int)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_TRPCStructIR(xdrs, objp)
	XDR *xdrs;
	TRPCStructIR *objp;
{
	if (!xdr_double(xdrs, &objp->TimeStamp)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->RemainingItems)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->Distances, 16, sizeof(int), xdr_int)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_TRPCStructBumpers(xdrs, objp)
	XDR *xdrs;
	TRPCStructBumpers *objp;
{
	if (!xdr_double(xdrs, &objp->TimeStamp)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->Bumpers0_31)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_TRPCStructWebCam(xdrs, objp)
	XDR *xdrs;
	TRPCStructWebCam *objp;
{
	if (!xdr_double(xdrs, &objp->TimeStamp)) {
		return (FALSE);
	}
	if (!xdr_char(xdrs, &objp->Number)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->Dimension)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->Data, 10000, sizeof(char), xdr_char)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_TRPCStructRobotCMD(xdrs, objp)
	XDR *xdrs;
	TRPCStructRobotCMD *objp;
{
	if (!xdr_double(xdrs, &objp->TimeStamp)) {
		return (FALSE);
	}
	if (!xdr_char(xdrs, &objp->EmergencyStop)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->V)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->W)) {
		return (FALSE);
	}
	if (!xdr_char(xdrs, &objp->ShowWebCAM)) {
		return (FALSE);
	}
	if (!xdr_char(xdrs, &objp->UseWebCAM)) {
		return (FALSE);
	}
	if (!xdr_char(xdrs, &objp->UseSonars)) {
		return (FALSE);
	}
	if (!xdr_char(xdrs, &objp->UseLaser)) {
		return (FALSE);
	}
	if (!xdr_char(xdrs, &objp->UseBumpers)) {
		return (FALSE);
	}
	if (!xdr_char(xdrs, &objp->AvoidObstaclesSonars)) {
		return (FALSE);
	}
	if (!xdr_char(xdrs, &objp->AvoidObstaclesLaser)) {
		return (FALSE);
	}
	if (!xdr_char(xdrs, &objp->AvoidObstaclesBumpers)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_TRPCStructIO_IN(xdrs, objp)
	XDR *xdrs;
	TRPCStructIO_IN *objp;
{
	if (!xdr_double(xdrs, &objp->TimeStamp)) {
		return (FALSE);
	}
	if (!xdr_u_int(xdrs, &objp->DigInput0_31)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->AnInput, 8, sizeof(double), xdr_double)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->Temperature)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_TRPCStructIO_OUT(xdrs, objp)
	XDR *xdrs;
	TRPCStructIO_OUT *objp;
{
	if (!xdr_double(xdrs, &objp->TimeStamp)) {
		return (FALSE);
	}
	if (!xdr_u_int(xdrs, &objp->DigOutput0_31)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->AnOutput, 2, sizeof(double), xdr_double)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_TRPCStructLoc(xdrs, objp)
	XDR *xdrs;
	TRPCStructLoc *objp;
{
	if (!xdr_double(xdrs, &objp->X)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->Y)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->Z)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->Theta)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->Pitch)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->Roll)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->Lin_Vel)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->Ang_Vel)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->SigmaX)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->SigmaY)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->SigmaZ)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->SigmaTheta)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->SigmaPitch)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->SigmaRoll)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->TimeStamp)) {
		return (FALSE);
	}
	if (!xdr_u_int(xdrs, &objp->Flags)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_TRPCStructMap(xdrs, objp)
	XDR *xdrs;
	TRPCStructMap *objp;
{
	if (!xdr_double(xdrs, &objp->TimeStamp)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->TopPosX)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->TopPosY)) {
		return (FALSE);
	}
	if (!xdr_double(xdrs, &objp->ScalePPM)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->DimX)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->DimY)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->Map, 10000, sizeof(char), xdr_char)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_TRPCLaser(xdrs, objp)
	XDR *xdrs;
	TRPCLaser *objp;
{
	if (!xdr_TRPCStructLaser(xdrs, objp)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_TRPCEncoder(xdrs, objp)
	XDR *xdrs;
	TRPCEncoder *objp;
{
	if (!xdr_TRPCStructEncoder(xdrs, objp)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_TRPCSonar(xdrs, objp)
	XDR *xdrs;
	TRPCSonar *objp;
{
	if (!xdr_TRPCStructSonar(xdrs, objp)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_TRPCWebCam(xdrs, objp)
	XDR *xdrs;
	TRPCWebCam *objp;
{
	if (!xdr_TRPCStructWebCam(xdrs, objp)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_TRPCRobotCMD(xdrs, objp)
	XDR *xdrs;
	TRPCRobotCMD *objp;
{
	if (!xdr_TRPCStructRobotCMD(xdrs, objp)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_TRPCIR(xdrs, objp)
	XDR *xdrs;
	TRPCIR *objp;
{
	if (!xdr_TRPCStructIR(xdrs, objp)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_TRPCBumpers(xdrs, objp)
	XDR *xdrs;
	TRPCBumpers *objp;
{
	if (!xdr_TRPCStructBumpers(xdrs, objp)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_TRPCIO_IN(xdrs, objp)
	XDR *xdrs;
	TRPCIO_IN *objp;
{
	if (!xdr_TRPCStructIO_IN(xdrs, objp)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_TRPCIO_OUT(xdrs, objp)
	XDR *xdrs;
	TRPCIO_OUT *objp;
{
	if (!xdr_TRPCStructIO_OUT(xdrs, objp)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_TRPCLoc(xdrs, objp)
	XDR *xdrs;
	TRPCLoc *objp;
{
	if (!xdr_TRPCStructLoc(xdrs, objp)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_TRPCMap(xdrs, objp)
	XDR *xdrs;
	TRPCMap *objp;
{
	if (!xdr_TRPCStructMap(xdrs, objp)) {
		return (FALSE);
	}
	return (TRUE);
}


