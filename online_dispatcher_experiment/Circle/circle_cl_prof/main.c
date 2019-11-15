#include "libabl.h"
#include "time.h"
#include "stdio.h"
#include "CL/cl.h"

#include "omp.h"
#define PROFILING_ITERATION 20
#define PROFILING_INTERVAL 100

typedef struct {
    float2 pos;

    int envId;
}Point;
static const type_info Point_info[] = {
    { TYPE_FLOAT2, offsetof(Point, pos), "pos", true },
    { TYPE_END, sizeof(Point), NULL }
};

typedef struct { 
    int mem_start;
    int mem_end;
} env;

struct agent_struct {
    dyn_array agents_Point;
    dyn_array agents_Point_dbuf;
};
struct agent_struct agents;
static const agent_info agents_info[] = {
    { Point_info, offsetof(struct agent_struct, agents_Point), "Point" },
    { NULL, 0, NULL }
};

double PI = 3.14159;
int num_timesteps = 1000;
int num_agents = 65536;
double rho = 0.05;
double k_rep = 0.05;
double k_att = 0.01;
double r = 5.0;
double W = 1144.87;
float2 random_1(float2 min, float2 max) {
    return float2_create(random_float(min.x, max.x), random_float(min.y, max.y));
}
float3 random_2(float3 min, float3 max) {
    return float3_create(random_float(min.x, max.x), random_float(min.y, max.y), random_float(min.z, max.z));
}
double random_3(double max) {
    return random_float(0, max);
}
float2 random_4(float2 max) {
    return random_1(float2_fill(0), max);
}
float3 random_5(float3 max) {
    return random_2(float3_fill(0), max);
}
int randomInt_1(int max) {
    return random_int(0, max);
}
double clam(double pos, double min, double max) {
    return ((pos < min) ? min : ((pos > max) ? max : pos));
}
float2 clam_1(float2 pos, float2 min, float2 max) {
    return float2_create(((pos.x < min.x) ? min.x : ((pos.x > max.x) ? max.x : pos.x)), ((pos.y < min.y) ? min.y : ((pos.y > max.y) ? max.y : pos.y)));
}
float3 clam_2(float3 pos, float3 min, float3 max) {
    return float3_create(((pos.x < min.x) ? min.x : ((pos.x > max.x) ? max.x : pos.x)), ((pos.y < min.y) ? min.y : ((pos.y > max.y) ? max.y : pos.y)), ((pos.z < min.z) ? min.z : ((pos.z > max.z) ? max.z : pos.z)));
}
double clam_3(double pos, double max) {
    return clam(pos, 0, max);
}
float2 clam_4(float2 pos, float2 max) {
    return clam_1(pos, float2_fill(0), max);
}
float3 clam_5(float3 pos, float3 max) {
    return clam_2(pos, float3_fill(0), max);
}
double wraparound(double pos, double max) {
    return ((pos < 0) ? (max + pos) : ((pos >= max) ? (pos - max) : pos));
}
float2 wraparound_1(float2 pos, float2 max) {
    return float2_create(((pos.x < 0) ? (max.x + pos.x) : ((pos.x >= max.x) ? (pos.x - max.x) : pos.x)), ((pos.y < 0) ? (max.y + pos.y) : ((pos.y >= max.y) ? (pos.y - max.y) : pos.y)));
}
float3 wraparound_2(float3 pos, float3 max) {
    return float3_create(((pos.x < 0) ? (max.x + pos.x) : ((pos.x >= max.x) ? (pos.x - max.x) : pos.x)), ((pos.y < 0) ? (max.y + pos.y) : ((pos.y >= max.y) ? (pos.y - max.y) : pos.y)), ((pos.z < 0) ? (max.z + pos.z) : ((pos.z >= max.z) ? (pos.z - max.z) : pos.z)));
}
bool is_inside(float2 pos, float2 max) {
    return ((((pos.x >= 0) && (pos.y >= 0)) && (pos.x <= max.x)) && (pos.y <= max.y));
}
bool isSortNeeded(int p1, int p2) {
    if ((p1 > p2)) return true;
    return false;
}
bool isSortNeeded_1(float2 p1, float2 p2) {
    if ((p1.x > p2.x)) return true;
    if ((p1.x == p2.x)) {
        if ((p1.y > p2.y)) {
            return true;
        }
    }
    return false;
}

int main() {
    double wtime = omp_get_wtime();
    for (int i = 0, _var0 = num_agents; i < _var0; ++i) {
        *DYN_ARRAY_PLACE(&agents.agents_Point, Point) = (Point) {
            .pos = random_4(float2_fill(W)),
        };
    }
    env *envArray = calloc(sizeof(env), 13225);
    for (int i = 0; i < 13225; i++) {
        env envElement = {0,0};
        envArray[i] = envElement;
    }
    cl_int ret;
    cl_device_id device_id = NULL;
    cl_uint num_of_platforms;
    cl_uint num_of_devices=0;
    clGetPlatformIDs(0, NULL, &num_of_platforms);
    cl_platform_id platform_ids[num_of_platforms];
    ret = clGetPlatformIDs( num_of_platforms, platform_ids, NULL );
    if (!agents.agents_Point_dbuf.values) {
        agents.agents_Point_dbuf = DYN_ARRAY_COPY_FIXED(Point, &agents.agents_Point);
    }
    size_t _var1 = sizeof(Point)*agents.agents_Point.len;
    size_t _var1_dbuf = sizeof(Point)*agents.agents_Point.len;
    cl_command_queue command_queues[2];
    cl_kernel kernels[2];
    cl_kernel st_kernels[2];
    cl_kernel interaction_rate_kernels[2];
    cl_kernel mem_kernels[2];
    cl_kernel upenv_kernels[2];
    cl_mem _var1MemObjs[2];
    cl_mem _var1MemObjDbufs[2];
    cl_mem _var1MemObjLens[2];
    cl_mem _var1MemObjEnvs[2];
    cl_mem _var1MemObjRedos[2];
    cl_mem _var1MemObjInteractionCurrents[2];
    cl_mem _var1MemObjInteractionLasts[2];

    int activeDevice[2]={0,1};

    char fileName[] = "kernel.cl";
    char *source_str;
    size_t source_size;
    FILE *fp;
    fp = fopen(fileName, "r");
    source_str = (char*)malloc(0x100000);
    source_size = fread(source_str, 1, 0x100000, fp);
    fclose(fp);
    for (int deviceIter = 0 ; deviceIter < 2 ; deviceIter++){
        int devId = activeDevice[deviceIter];
        ret = clGetDeviceIDs( platform_ids[devId], CL_DEVICE_TYPE_ALL, 1, &device_id, &ret );
        cl_context_properties contextProperties[] =
        {
            CL_CONTEXT_PLATFORM,
            (cl_context_properties) platform_ids[devId],
            0
        };
        cl_context context = clCreateContextFromType(contextProperties, CL_DEVICE_TYPE_ALL, NULL, NULL, &ret);
        cl_command_queue command_queue = clCreateCommandQueueWithProperties(context, device_id, 0, &ret);
        command_queues[deviceIter] = command_queue;
        cl_mem _var1MemObj = clCreateBuffer(context, CL_MEM_READ_WRITE, _var1, NULL , &ret);
        _var1MemObjs[deviceIter] = _var1MemObj;
        cl_mem _var1MemObjDbuf  = clCreateBuffer(context, CL_MEM_READ_WRITE, _var1_dbuf, NULL , &ret);
        _var1MemObjDbufs[deviceIter] = _var1MemObjDbuf;
        cl_mem _var1MemObjLen  = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(int), NULL , &ret);
        _var1MemObjLens[deviceIter] = _var1MemObjLen;
        cl_mem _var1MemObjEnv  = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(env)*13225, NULL , &ret);
        _var1MemObjEnvs[deviceIter] = _var1MemObjEnv;
        cl_mem _var1MemObjRedo  = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(bool), NULL , &ret);
        _var1MemObjRedos[deviceIter] = _var1MemObjRedo;
        cl_mem _var1MemObjInteractionCurrent  = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(int), NULL , &ret);
        _var1MemObjInteractionCurrents[deviceIter] = _var1MemObjInteractionCurrent;
        cl_mem _var1MemObjInteractionLast  = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(int), NULL , &ret);
        _var1MemObjInteractionLasts[deviceIter] = _var1MemObjInteractionLast;

        cl_program program = clCreateProgramWithSource(context, 1, (const char **)&source_str, NULL, &ret);
        ret = clBuildProgram(program, 1, &device_id, NULL, NULL, NULL);
        char buildLog[655360];
        clGetProgramBuildInfo(program, device_id, CL_PROGRAM_BUILD_LOG,
                              sizeof(buildLog), buildLog, NULL);

        printf("Error in kernel: %s\n",buildLog);
        char* kernel_name_prefix = "compute_kernel_";
        char number[2];
        char kernel_name[20];
        sprintf(number,"%d", deviceIter);
        sprintf(kernel_name,"%s%s",kernel_name_prefix,number);
        cl_kernel kernel = clCreateKernel(program, kernel_name, &ret);
        kernels[deviceIter] = kernel;
        cl_kernel st_kernel = clCreateKernel(program, "sorting", &ret);
        st_kernels[deviceIter] = st_kernel;
        cl_kernel mem_kernel = clCreateKernel(program, "mem_update", &ret);
        mem_kernels[deviceIter] = mem_kernel;
        cl_kernel upenv_kernel = clCreateKernel(program, "update_envId", &ret);
        upenv_kernels[deviceIter] = upenv_kernel;
        cl_kernel interaction_rate_kernel = clCreateKernel(program, "check_interaction_rate", &ret);
        interaction_rate_kernels[deviceIter] = interaction_rate_kernel;

        ret = clSetKernelArg(kernel, 0, sizeof(cl_mem), &_var1MemObj);
        ret |= clSetKernelArg(kernel, 1, sizeof(cl_mem), &_var1MemObjDbuf);
        ret |= clSetKernelArg(kernel, 2, sizeof(cl_mem), &_var1MemObjLen);
        ret |= clSetKernelArg(kernel, 3, sizeof(cl_mem), &_var1MemObjEnv);
        ret |= clSetKernelArg(kernel, 4, sizeof(cl_mem), &_var1MemObjInteractionCurrent);

        ret = clSetKernelArg(upenv_kernel, 0, sizeof(cl_mem), &_var1MemObj);
        ret |= clSetKernelArg(upenv_kernel, 1, sizeof(cl_mem), &_var1MemObjLen);
        ret |= clSetKernelArg(upenv_kernel, 2, sizeof(cl_mem), &_var1MemObjInteractionCurrent);

        ret = clSetKernelArg(st_kernel, 0, sizeof(cl_mem), &_var1MemObj);
        ret |= clSetKernelArg(st_kernel, 1, sizeof(cl_mem), &_var1MemObjDbuf);
        ret |= clSetKernelArg(st_kernel, 2, sizeof(cl_mem), &_var1MemObjLen);
        ret = clSetKernelArg(mem_kernel, 0, sizeof(cl_mem), &_var1MemObj);
        ret |= clSetKernelArg(mem_kernel, 1, sizeof(cl_mem), &_var1MemObjDbuf);
        ret |= clSetKernelArg(mem_kernel, 2, sizeof(cl_mem), &_var1MemObjLen);
        ret |= clSetKernelArg(mem_kernel, 3, sizeof(cl_mem), &_var1MemObjEnv);
        ret = clSetKernelArg(interaction_rate_kernel, 0, sizeof(cl_mem), &_var1MemObjInteractionCurrent);
        ret |= clSetKernelArg(interaction_rate_kernel, 1, sizeof(cl_mem), &_var1MemObjInteractionLast);
        ret |= clSetKernelArg(interaction_rate_kernel, 2, sizeof(cl_mem), &_var1MemObjRedo);

        int tmp0 = 0;

        ret = clEnqueueWriteBuffer(command_queue, _var1MemObj, CL_TRUE, 0, _var1, agents.agents_Point.values, 0, NULL, NULL);
        ret = clEnqueueWriteBuffer(command_queue, _var1MemObjDbuf, CL_TRUE, 0, _var1_dbuf, agents.agents_Point_dbuf.values, 0, NULL, NULL);
        ret = clEnqueueWriteBuffer(command_queue, _var1MemObjLen, CL_TRUE, 0, sizeof(int), &agents.agents_Point.len, 0, NULL, NULL);
        ret = clEnqueueWriteBuffer(command_queue, _var1MemObjInteractionCurrent, CL_TRUE, 0, sizeof(int), &tmp0, 0, NULL, NULL);
        ret = clEnqueueWriteBuffer(command_queue, _var1MemObjInteractionLast, CL_TRUE, 0, sizeof(int), &tmp0, 0, NULL, NULL);
        ret = clEnqueueWriteBuffer(command_queue, _var1MemObjRedo, CL_TRUE, 0, sizeof(bool), &tmp0, 0, NULL, NULL);
        ret = clEnqueueWriteBuffer(command_queue, _var1MemObjEnv, CL_TRUE, 0, sizeof(env)*13225, envArray, 0, NULL, NULL);
    }
    size_t localWorkSize = 128;
    size_t globalWorkSize = roundWorkSizeUp(128, agents.agents_Point.len);

    size_t stlocalWorkSize = 1;
    size_t stglobalWorkSize = 1;

    ret = clEnqueueNDRangeKernel(command_queues[0], upenv_kernels[0], 1, NULL, &globalWorkSize, &localWorkSize, 0, NULL, NULL);
    for (int length = 1; length < globalWorkSize; length <<= 1)
        for (int inc = length; inc > 0; inc >>= 1) {
            int dir = length << 1;
            clSetKernelArg(st_kernels[0], 3, sizeof(int), &inc);
            clSetKernelArg(st_kernels[0], 4, sizeof(int), &dir);
            clEnqueueNDRangeKernel(command_queues[0], st_kernels[0], 1, NULL, &globalWorkSize, NULL, 0, NULL, NULL);
            clFinish(command_queues[0]);
        }
    ret |= clEnqueueNDRangeKernel(command_queues[0], mem_kernels[0], 1, NULL, &globalWorkSize, &localWorkSize, 0, NULL, NULL);
    clFinish(command_queues[0]);
    ret = clEnqueueReadBuffer(command_queues[0], _var1MemObjs[0], CL_TRUE, 0, _var1, agents.agents_Point.values, 0, NULL, NULL);
    ret |= clEnqueueReadBuffer(command_queues[0], _var1MemObjEnvs[0], CL_TRUE, 0, sizeof(env)*13225, envArray, 0, NULL, NULL);
    ret |= clEnqueueWriteBuffer(command_queues[1], _var1MemObjs[1], CL_TRUE, 0, _var1, agents.agents_Point.values, 0, NULL, NULL);
    ret |= clEnqueueWriteBuffer(command_queues[1], _var1MemObjEnvs[1], CL_TRUE, 0, sizeof(env)*13225, envArray, 0, NULL, NULL);

    double stime = omp_get_wtime();

    for (int _var3 = 0; _var3 < PROFILING_ITERATION; _var3++) {
        ret = clEnqueueNDRangeKernel(command_queues[0], kernels[0], 1, NULL, &globalWorkSize, &localWorkSize, 0, NULL, NULL);
        ret = clEnqueueNDRangeKernel(command_queues[1], kernels[1], 1, NULL, &globalWorkSize, &localWorkSize, 0, NULL, NULL);

        if (_var3 == PROFILING_ITERATION - 1){
            clEnqueueNDRangeKernel(command_queues[0], interaction_rate_kernels[0], 1, NULL,
                                   &stglobalWorkSize, &stlocalWorkSize, 0,
                                   NULL, NULL);
            clEnqueueNDRangeKernel(command_queues[1], interaction_rate_kernels[1], 1, NULL,
                                   &stglobalWorkSize, &stlocalWorkSize, 0,
                                   NULL, NULL);

        }

        ret = clEnqueueNDRangeKernel(command_queues[0], upenv_kernels[0], 1, NULL, &globalWorkSize, &localWorkSize, 0, NULL, NULL);
        ret = clEnqueueNDRangeKernel(command_queues[1], upenv_kernels[1], 1, NULL, &globalWorkSize, &localWorkSize, 0, NULL, NULL);

        for (int length = 1; length < globalWorkSize; length <<= 1)
            for (int inc = length; inc > 0; inc >>= 1) {
                int dir = length << 1;
                clSetKernelArg(st_kernels[0], 3, sizeof(int), &inc);
                clSetKernelArg(st_kernels[0], 4, sizeof(int), &dir);
                clEnqueueNDRangeKernel(command_queues[0], st_kernels[0], 1, NULL, &globalWorkSize, NULL, 0, NULL, NULL);

                clSetKernelArg(st_kernels[1], 3, sizeof(int), &inc);
                clSetKernelArg(st_kernels[1], 4, sizeof(int), &dir);
                clEnqueueNDRangeKernel(command_queues[1], st_kernels[1], 1, NULL, &globalWorkSize, NULL, 0, NULL, NULL);
            }
        ret |= clEnqueueNDRangeKernel(command_queues[0], mem_kernels[0], 1, NULL, &globalWorkSize, &localWorkSize, 0, NULL, NULL);
        ret |= clEnqueueNDRangeKernel(command_queues[1], mem_kernels[1], 1, NULL, &globalWorkSize, &localWorkSize, 0, NULL, NULL);
    }
    clFinish(command_queues[0]);
    double gtime = omp_get_wtime() - stime;
    printf("GPU finishes %f seconds\n",gtime);

    clFinish(command_queues[1]);
    double ctime = omp_get_wtime() - stime;
    printf("CPU finishes %f seconds\n",ctime);

    int device = -1;
    int undevice = -1;

    if (gtime <= ( ctime - 0.001) ) {
        printf("GPU\n");
        device = 0;
        undevice = 1;
    } else {
        printf("CPU\n");
        device = 1;
        undevice = 0;
    }

    bool redoProfiling = false;
    int profilingStart = -1;
    int profilingEnd = -1;

    for (int _var3 = PROFILING_ITERATION; _var3 < num_timesteps; _var3++) {
        if (redoProfiling) {
            if (profilingStart == -1) {
                ret = clEnqueueReadBuffer(command_queues[device], _var1MemObjs[device], CL_FALSE, 0, _var1,
                                          agents.agents_Point.values, 0, NULL, NULL);
                ret = clEnqueueReadBuffer(command_queues[device], _var1MemObjDbufs[device], CL_FALSE, 0, _var1_dbuf,
                                          agents.agents_Point_dbuf.values, 0, NULL, NULL);
                ret = clEnqueueReadBuffer(command_queues[device], _var1MemObjEnvs[device], CL_TRUE, 0,
                                          sizeof(env) * 13225,
                                          envArray, 0, NULL, NULL);
                ret = clEnqueueWriteBuffer(command_queues[undevice], _var1MemObjs[undevice], CL_FALSE, 0, _var1,
                                           agents.agents_Point.values, 0, NULL, NULL);
                ret = clEnqueueWriteBuffer(command_queues[undevice], _var1MemObjDbufs[undevice], CL_FALSE, 0,
                                           _var1_dbuf, agents.agents_Point_dbuf.values, 0, NULL, NULL);
                ret = clEnqueueWriteBuffer(command_queues[undevice], _var1MemObjEnvs[undevice], CL_TRUE, 0,
                                           sizeof(env) * 13225,
                                           envArray, 0, NULL, NULL);
                profilingStart = _var3;
                profilingEnd = profilingStart + PROFILING_ITERATION;
                stime = omp_get_wtime();
            }

            ret = clEnqueueNDRangeKernel(command_queues[0], kernels[0], 1, NULL, &globalWorkSize, &localWorkSize, 0, NULL, NULL);
            ret = clEnqueueNDRangeKernel(command_queues[1], kernels[1], 1, NULL, &globalWorkSize, &localWorkSize, 0, NULL, NULL);
            ret = clEnqueueNDRangeKernel(command_queues[0], upenv_kernels[0], 1, NULL, &globalWorkSize, &localWorkSize, 0, NULL, NULL);
            ret = clEnqueueNDRangeKernel(command_queues[1], upenv_kernels[1], 1, NULL, &globalWorkSize, &localWorkSize, 0, NULL, NULL);

            for (int length = 1; length < globalWorkSize; length <<= 1)
                for (int inc = length; inc > 0; inc >>= 1) {
                    int dir = length << 1;
                    clSetKernelArg(st_kernels[0], 3, sizeof(int), &inc);
                    clSetKernelArg(st_kernels[0], 4, sizeof(int), &dir);
                    clEnqueueNDRangeKernel(command_queues[0], st_kernels[0], 1, NULL, &globalWorkSize, NULL, 0, NULL,
                                           NULL);

                    clSetKernelArg(st_kernels[1], 3, sizeof(int), &inc);
                    clSetKernelArg(st_kernels[1], 4, sizeof(int), &dir);
                    clEnqueueNDRangeKernel(command_queues[1], st_kernels[1], 1, NULL, &globalWorkSize, NULL, 0, NULL,
                                           NULL);
                    clFinish(command_queues[0]);
                    clFinish(command_queues[1]);
                }
            ret |= clEnqueueNDRangeKernel(command_queues[0], mem_kernels[0], 1, NULL, &globalWorkSize, &localWorkSize, 0, NULL, NULL);
            ret |= clEnqueueNDRangeKernel(command_queues[1], mem_kernels[1], 1, NULL, &globalWorkSize, &localWorkSize, 0, NULL, NULL);


            if (_var3 == profilingEnd) {
                clFinish(command_queues[0]);
                double gtime = omp_get_wtime() - stime;
                clFinish(command_queues[1]);
                double ctime = omp_get_wtime() - stime;
                if (gtime <= (ctime - 0.01)) {
                    device = 0;
                    undevice = 1;
                } else {
                    device = 1;
                    undevice = 0;
                }
                redoProfiling = false;
                profilingStart = profilingEnd = -1;
            }
        } else {
            ret = clEnqueueNDRangeKernel(command_queues[device], kernels[device], 1, NULL, &globalWorkSize, &localWorkSize, 0, NULL, NULL);
            if (_var3 % PROFILING_INTERVAL == 0) {
                clEnqueueNDRangeKernel(command_queues[device], interaction_rate_kernels[device], 1, NULL,
                                       &stglobalWorkSize, &stlocalWorkSize, 0,
                                       NULL, NULL);
                clFinish(command_queues[device]);
                ret = clEnqueueReadBuffer(command_queues[device], _var1MemObjRedos[device], CL_TRUE, 0, sizeof(bool),
                                          &redoProfiling, 0, NULL, NULL);
                if (redoProfiling)
                    printf("Request to redo profiling\n");
                else
                    printf("No redo profiling\n");
            }

            ret = clEnqueueNDRangeKernel(command_queues[device], upenv_kernels[device], 1, NULL, &globalWorkSize, &localWorkSize, 0, NULL, NULL);

            for (int length = 1; length < globalWorkSize; length <<= 1)
                for (int inc = length; inc > 0; inc >>= 1) {
                    int dir = length << 1;
                    clSetKernelArg(st_kernels[device], 3, sizeof(int), &inc);
                    clSetKernelArg(st_kernels[device], 4, sizeof(int), &dir);
                    clEnqueueNDRangeKernel(command_queues[device], st_kernels[device], 1, NULL, &globalWorkSize, NULL,
                                           0, NULL, NULL);
                    clFinish(command_queues[device]);
                }
            ret |= clEnqueueNDRangeKernel(command_queues[device], mem_kernels[device], 1, NULL, &globalWorkSize,
                                          &localWorkSize, 0, NULL, NULL);
        }
    }
    ret = clEnqueueReadBuffer(command_queues[0], _var1MemObjs[0], CL_TRUE, 0, _var1, agents.agents_Point.values, 0, NULL, NULL);

    wtime = omp_get_wtime() - wtime;
    printf("Time elapsed is %f seconds\n", wtime);
    return 0;
}
