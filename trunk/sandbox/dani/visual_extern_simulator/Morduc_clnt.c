#include "rpc/rpc.h"
#include "Morduc.h"

/* Default timeout can be changed using clnt_control() */
static struct timeval TIMEOUT = { 25, 0 };

int *
reboot_pc_11(argp, clnt)
	void *argp;
	CLIENT *clnt;
{
	static int res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, REBOOT_PC, xdr_void, argp, xdr_int, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


int *
shutdown_pc_11(argp, clnt)
	void *argp;
	CLIENT *clnt;
{
	static int res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, SHUTDOWN_PC, xdr_void, argp, xdr_int, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


TRPCLaser *
read_laser_11(argp, clnt)
	int *argp;
	CLIENT *clnt;
{
	static TRPCLaser res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, READ_LASER, xdr_int, argp, xdr_TRPCLaser, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


int *
write_laser_11(argp, clnt)
	TRPCLaser *argp;
	CLIENT *clnt;
{
	static int res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, WRITE_LASER, xdr_TRPCLaser, argp, xdr_int, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


TRPCSonar *
read_sonar_11(argp, clnt)
	int *argp;
	CLIENT *clnt;
{
	static TRPCSonar res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, READ_SONAR, xdr_int, argp, xdr_TRPCSonar, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


int *
write_sonar_11(argp, clnt)
	TRPCSonar *argp;
	CLIENT *clnt;
{
	static int res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, WRITE_SONAR, xdr_TRPCSonar, argp, xdr_int, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


TRPCEncoder *
read_encoder_11(argp, clnt)
	void *argp;
	CLIENT *clnt;
{
	static TRPCEncoder res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, READ_ENCODER, xdr_void, argp, xdr_TRPCEncoder, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


int *
write_encoder_11(argp, clnt)
	TRPCEncoder *argp;
	CLIENT *clnt;
{
	static int res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, WRITE_ENCODER, xdr_TRPCEncoder, argp, xdr_int, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


TRPCIR *
read_ir_11(argp, clnt)
	int *argp;
	CLIENT *clnt;
{
	static TRPCIR res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, READ_IR, xdr_int, argp, xdr_TRPCIR, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


int *
write_ir_11(argp, clnt)
	TRPCIR *argp;
	CLIENT *clnt;
{
	static int res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, WRITE_IR, xdr_TRPCIR, argp, xdr_int, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


TRPCBumpers *
read_bumpers_11(argp, clnt)
	void *argp;
	CLIENT *clnt;
{
	static TRPCBumpers res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, READ_BUMPERS, xdr_void, argp, xdr_TRPCBumpers, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


int *
write_bumpers_11(argp, clnt)
	TRPCBumpers *argp;
	CLIENT *clnt;
{
	static int res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, WRITE_BUMPERS, xdr_TRPCBumpers, argp, xdr_int, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


TRPCWebCam *
read_cam_11(argp, clnt)
	char *argp;
	CLIENT *clnt;
{
	static TRPCWebCam res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, READ_CAM, xdr_char, argp, xdr_TRPCWebCam, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


int *
write_cam_11(argp, clnt)
	TRPCWebCam *argp;
	CLIENT *clnt;
{
	static int res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, WRITE_CAM, xdr_TRPCWebCam, argp, xdr_int, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


TRPCRobotCMD *
read_cmd_11(argp, clnt)
	void *argp;
	CLIENT *clnt;
{
	static TRPCRobotCMD res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, READ_CMD, xdr_void, argp, xdr_TRPCRobotCMD, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


int *
write_cmd_11(argp, clnt)
	TRPCRobotCMD *argp;
	CLIENT *clnt;
{
	static int res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, WRITE_CMD, xdr_TRPCRobotCMD, argp, xdr_int, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


TRPCIO_IN *
read_io_in_11(argp, clnt)
	void *argp;
	CLIENT *clnt;
{
	static TRPCIO_IN res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, READ_IO_IN, xdr_void, argp, xdr_TRPCIO_IN, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


int *
write_io_in_11(argp, clnt)
	TRPCIO_IN *argp;
	CLIENT *clnt;
{
	static int res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, WRITE_IO_IN, xdr_TRPCIO_IN, argp, xdr_int, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


TRPCIO_OUT *
read_io_out_11(argp, clnt)
	void *argp;
	CLIENT *clnt;
{
	static TRPCIO_OUT res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, READ_IO_OUT, xdr_void, argp, xdr_TRPCIO_OUT, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


int *
write_io_out_11(argp, clnt)
	TRPCIO_OUT *argp;
	CLIENT *clnt;
{
	static int res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, WRITE_IO_OUT, xdr_TRPCIO_OUT, argp, xdr_int, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


TRPCLoc *
read_loc_11(argp, clnt)
	void *argp;
	CLIENT *clnt;
{
	static TRPCLoc res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, READ_LOC, xdr_void, argp, xdr_TRPCLoc, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


int *
write_loc_11(argp, clnt)
	TRPCLoc *argp;
	CLIENT *clnt;
{
	static int res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, WRITE_LOC, xdr_TRPCLoc, argp, xdr_int, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


TRPCMap *
read_map_11(argp, clnt)
	void *argp;
	CLIENT *clnt;
{
	static TRPCMap res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, READ_MAP, xdr_void, argp, xdr_TRPCMap, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


int *
write_map_11(argp, clnt)
	TRPCMap *argp;
	CLIENT *clnt;
{
	static int res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, WRITE_MAP, xdr_TRPCMap, argp, xdr_int, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

