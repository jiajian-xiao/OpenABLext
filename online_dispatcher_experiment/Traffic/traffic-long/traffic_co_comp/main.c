#include "libabl_int.h"
#include "time.h"
#include "stdio.h"
#include "CL/cl.h"

#include "omp.h"

typedef struct {
    double velocity;
    float2 pos;
    int id;
    bool isFinished;
    bool forwardOnLane;
    double milage;
}Point;
static const type_info Point_info[] = {
    { TYPE_FLOAT, offsetof(Point, velocity), "velocity", false },
    { TYPE_FLOAT2, offsetof(Point, pos), "pos", true },
    { TYPE_INT, offsetof(Point, id), "id", false },
    { TYPE_BOOL, offsetof(Point, isFinished), "isFinished", false },
    { TYPE_BOOL, offsetof(Point, forwardOnLane), "forwardOnLane", false },
    { TYPE_FLOAT, offsetof(Point, milage), "milage", false },
    { TYPE_END, sizeof(Point), NULL }
};

typedef struct {
    int laneId;
    double length;
    int speedLimit;
    int nextLaneId;
    int leftLaneId;
    int rightLaneId;

    int mem_start;
    int mem_end;
} Lane ;
static const type_info Lane_info[] = {
    { TYPE_INT, offsetof(Lane, laneId), "laneId", false },
    { TYPE_FLOAT, offsetof(Lane, length), "length", false },
    { TYPE_INT, offsetof(Lane, speedLimit), "speedLimit", false },
    { TYPE_INT, offsetof(Lane, nextLaneId), "nextLaneId", false },
    { TYPE_INT, offsetof(Lane, leftLaneId), "leftLaneId", false },
    { TYPE_INT, offsetof(Lane, rightLaneId), "rightLaneId", false },
    { TYPE_END, sizeof(Lane), NULL }
};

Lane roads[4096];
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
int num_agents = 8192;
double dt = 250.0;
double size = 3.5;
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


bool tie_breaking(Point* ag1, Point* ag2) {
    if (((ag2->pos.y > ag1->pos.y) && ((ag2->pos.y - ag1->pos.y) <= size))) return true; else return false;
}
Point* coexecution_merge_Point(Point* A0, Point* A1) {
    if ((!A1->forwardOnLane)) {
        A1->pos.x = A0->pos.x;
    }
    return A1;
}
int main() {
    double wtime = omp_get_wtime();
    int lan = 1;
    double posit = 4.0;
    for (int i = 0, _var0 = num_agents; i < _var0; ++i) {
        *DYN_ARRAY_PLACE(&agents.agents_Point, Point) = (Point) {
            .velocity = 1.0,
            .pos = float2_create(lan, posit),
            .id = i,
            .isFinished = false,
            .forwardOnLane = false,
            .milage = 0.0,
        };
        posit = ((posit + 4.0) + random_3(1.0));
        if ((posit > 200)) {
            posit = 0;
            lan = (lan + 1);
        }
    }
    for (int p = 1, _var1 = 2048; p < _var1; ++p) {
        if ((p < 2046)) {
            int next = (p + 3);
            int left = (((p % 3) == 1) ? (-1) : (p - 1));
            int right = (((p % 3) == 0) ? (-1) : (p + 1));
            Lane _var2 = (Lane) {
                .laneId = p,
                .length = 500.0,
                .speedLimit = 30,
                .nextLaneId = next,
                .leftLaneId = left,
                .rightLaneId = right,
            };
            Lane* lane = &_var2;
            roads[p] = *lane;
        } else {
            Lane _var3 = (Lane) {
                .laneId = p,
                .length = 500.0,
                .speedLimit = 30,
                .nextLaneId = (-1),
                .leftLaneId = (-1),
                .rightLaneId = (-1),
            };
            Lane* lane = &_var3;
            roads[p] = *lane;
        }
    }
    cl_int ret;
    cl_device_id device_id = NULL;
    cl_uint num_of_platforms;
    cl_uint num_of_devices=0;
    clGetPlatformIDs(0, NULL, &num_of_platforms);
    cl_platform_id platform_ids[num_of_platforms];
    ret = clGetPlatformIDs( num_of_platforms, platform_ids, NULL );
    cl_command_queue command_queues[2];
    if (!agents.agents_Point_dbuf.values) {
        agents.agents_Point_dbuf = DYN_ARRAY_COPY_FIXED(Point, &agents.agents_Point);
    }
    size_t _var4_Point = sizeof(Point)*agents.agents_Point.len;
    size_t _var4_Point_dbuf = sizeof(Point)*agents.agents_Point.len;
    cl_kernel kernel_Point_0s[2];
    cl_kernel kernel_Point_1s[2];
    cl_kernel st_kernel_Points[2];
    cl_kernel mem_kernel_Points[2];
    cl_mem _var4MemObj_Points[2];
    cl_mem _var4MemObjDbuf_Points[2];
    cl_mem _var4MemObjLen_Points[2];
    cl_mem _var4MemObjEnv_Points[2];
    int activeDevice[2]={0,1};
    cl_mem _var4MemObjRng_Points[2];
    cl_mem _var4MemObjConflictFlag_Points[2];
    cl_kernel cr_kernel_Points[2];
    
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
        
        cl_program program = clCreateProgramWithSource(context, 1, (const char **)&source_str, NULL, &ret);
        ret = clBuildProgram(program, 1, &device_id, NULL, NULL, NULL);
        char* kernel_name_prefix = "compute_kernel_";
        char number[2];
        char kernel_name[50];
        cl_mem _var4MemObj_Point = clCreateBuffer(context, CL_MEM_READ_WRITE, _var4_Point, NULL , &ret);
        _var4MemObj_Points[deviceIter] = _var4MemObj_Point;
        cl_mem _var4MemObjDbuf_Point = clCreateBuffer(context, CL_MEM_READ_WRITE, _var4_Point_dbuf, NULL , &ret);
        _var4MemObjDbuf_Points[deviceIter] = _var4MemObjDbuf_Point;
        cl_mem _var4MemObjLen_Point = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(int), NULL , &ret);
        _var4MemObjLen_Points[deviceIter] = _var4MemObjLen_Point;
        cl_mem _var4MemObjEnv_Point = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(Lane)*4096, NULL , &ret);
        _var4MemObjEnv_Points[deviceIter] = _var4MemObjEnv_Point;
        cl_mem _var4MemObjConflictFlag_Point = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(bool), NULL , &ret);
        _var4MemObjConflictFlag_Points[deviceIter] = _var4MemObjConflictFlag_Point;
        cl_uint2 *rngState_Point;
        rngState_Point = (cl_uint2 *)calloc(agents.agents_Point.len, sizeof(cl_uint2));
        for ( int _var5 = 0 ; _var5 < agents.agents_Point.len ; _var5++ ) {
            cl_uint2 _var6 = { (uint32_t)(_var5 * 2), (uint32_t)(_var5 * 2 + 1) };
            rngState_Point[_var5] = _var6;
        }
        cl_mem _var4MemObjRng_Point = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(cl_uint2)*agents.agents_Point.len, NULL , &ret);
        _var4MemObjRng_Points[deviceIter] = _var4MemObjRng_Point;
        sprintf(number,"%d", deviceIter);
        sprintf(kernel_name,"%s%s_%s_%s", kernel_name_prefix, number, "Point","0");
        cl_kernel kernel_Point_0 = clCreateKernel(program, kernel_name, &ret);
        kernel_Point_0s[deviceIter] = kernel_Point_0;
        sprintf(number,"%d", deviceIter);
        sprintf(kernel_name,"%s%s_%s_%s", kernel_name_prefix, number, "Point","1");
        cl_kernel kernel_Point_1 = clCreateKernel(program, kernel_name, &ret);
        kernel_Point_1s[deviceIter] = kernel_Point_1;
        cl_kernel st_kernel_Point = clCreateKernel(program, "sorting_Point", &ret);
        st_kernel_Points[deviceIter] = st_kernel_Point;
        cl_kernel mem_kernel_Point = clCreateKernel(program, "mem_update_Point", &ret);
        mem_kernel_Points[deviceIter] = mem_kernel_Point;
        cl_kernel cr_kernel_Point = clCreateKernel(program, "conflict_resolver_Point", &ret);
        cr_kernel_Points[deviceIter] = cr_kernel_Point;
        ret = clSetKernelArg(kernel_Point_0, 0, sizeof(cl_mem), &_var4MemObj_Point);
        ret |= clSetKernelArg(kernel_Point_0, 1, sizeof(cl_mem), &_var4MemObjDbuf_Point);
        ret |= clSetKernelArg(kernel_Point_0, 2, sizeof(cl_mem), &_var4MemObjLen_Point);
        ret |= clSetKernelArg(kernel_Point_0, 3, sizeof(cl_mem), &_var4MemObjEnv_Point);
        ret |= clSetKernelArg(kernel_Point_0, 4, sizeof(cl_mem), &_var4MemObjRng_Point);
        ret = clSetKernelArg(kernel_Point_1, 0, sizeof(cl_mem), &_var4MemObj_Point);
        ret |= clSetKernelArg(kernel_Point_1, 1, sizeof(cl_mem), &_var4MemObjDbuf_Point);
        ret |= clSetKernelArg(kernel_Point_1, 2, sizeof(cl_mem), &_var4MemObjLen_Point);
        ret |= clSetKernelArg(kernel_Point_1, 3, sizeof(cl_mem), &_var4MemObjEnv_Point);
        ret |= clSetKernelArg(kernel_Point_1, 4, sizeof(cl_mem), &_var4MemObjRng_Point);
        ret = clSetKernelArg(st_kernel_Point, 0, sizeof(cl_mem), &_var4MemObj_Point);
        ret |= clSetKernelArg(st_kernel_Point, 1, sizeof(cl_mem), &_var4MemObjDbuf_Point);
        ret |= clSetKernelArg(st_kernel_Point, 2, sizeof(cl_mem), &_var4MemObjLen_Point);
        ret = clSetKernelArg(mem_kernel_Point, 0, sizeof(cl_mem), &_var4MemObj_Point);
        ret |= clSetKernelArg(mem_kernel_Point, 1, sizeof(cl_mem), &_var4MemObjDbuf_Point);
        ret |= clSetKernelArg(mem_kernel_Point, 2, sizeof(cl_mem), &_var4MemObjLen_Point);
        ret |= clSetKernelArg(mem_kernel_Point, 3, sizeof(cl_mem), &_var4MemObjEnv_Point);
        ret |= clSetKernelArg(mem_kernel_Point, 4, sizeof(cl_mem), &_var4MemObjConflictFlag_Point);
        ret = clSetKernelArg(cr_kernel_Point, 0, sizeof(cl_mem), &_var4MemObj_Point);
        ret |= clSetKernelArg(cr_kernel_Point, 1, sizeof(cl_mem), &_var4MemObjDbuf_Point);
        ret |= clSetKernelArg(cr_kernel_Point, 2, sizeof(cl_mem), &_var4MemObjLen_Point);
        ret |= clSetKernelArg(cr_kernel_Point, 3, sizeof(cl_mem), &_var4MemObjEnv_Point);
        ret |= clSetKernelArg(cr_kernel_Point, 4, sizeof(cl_mem), &_var4MemObjConflictFlag_Point);
        
        ret = clEnqueueWriteBuffer(command_queue, _var4MemObj_Point, CL_TRUE, 0, _var4_Point, agents.agents_Point.values, 0, NULL, NULL);
        ret = clEnqueueWriteBuffer(command_queue, _var4MemObjDbuf_Point, CL_TRUE, 0, _var4_Point_dbuf, agents.agents_Point_dbuf.values, 0, NULL, NULL);
        ret = clEnqueueWriteBuffer(command_queue, _var4MemObjLen_Point, CL_TRUE, 0, sizeof(int), &agents.agents_Point.len, 0, NULL, NULL);
        ret = clEnqueueWriteBuffer(command_queue, _var4MemObjEnv_Point, CL_TRUE, 0, sizeof(Lane)*4096, &roads, 0, NULL, NULL);
        ret = clEnqueueWriteBuffer(command_queue, _var4MemObjRng_Point, CL_TRUE, 0, sizeof(cl_uint2)*agents.agents_Point.len, rngState_Point, 0, NULL, NULL);
        
    }
    size_t localWorkSize = 128;
    Point* _var7buff_Point0 = calloc(sizeof(Point), agents.agents_Point.len);
    Point* _var7buff_Point1 = calloc(sizeof(Point), agents.agents_Point.len);
    Point* _var7buffMerge_Point = calloc(sizeof(Point), agents.agents_Point.len);
    Lane *_var7Env_Point = calloc(sizeof(Lane),4096);
    size_t globalWorkSize_Point = roundWorkSizeUp(128, agents.agents_Point.len);
    size_t cr_globalWorkSize_Point = 4096;
    for (int length = 1; length < globalWorkSize_Point; length <<= 1)
    for (int inc = length; inc > 0; inc >>= 1) {
        int dir = length << 1;
        clSetKernelArg(st_kernel_Points[0], 3, sizeof(int), &inc);
        clSetKernelArg(st_kernel_Points[0], 4, sizeof(int), &dir);
        clEnqueueNDRangeKernel(command_queues[0], st_kernel_Points[0], 1, NULL, &globalWorkSize_Point, NULL, 0, NULL, NULL);
        clFinish(command_queues[0]);
    }
    ret |= clEnqueueNDRangeKernel(command_queues[0], mem_kernel_Points[0], 1, NULL, &globalWorkSize_Point, &localWorkSize, 0, NULL, NULL);
    clFinish(command_queues[0]);
    clEnqueueReadBuffer(command_queues[0], _var4MemObj_Points[0], CL_TRUE, 0, _var4_Point, _var7buff_Point0, 0, NULL, NULL);
    clEnqueueReadBuffer(command_queues[0], _var4MemObjEnv_Points[0], CL_TRUE, 0, sizeof(Lane)*4096, _var7Env_Point, 0, NULL, NULL);
    clEnqueueWriteBuffer(command_queues[1], _var4MemObj_Points[1], CL_TRUE, 0, _var4_Point, _var7buff_Point0, 0, NULL, NULL);
    clEnqueueWriteBuffer(command_queues[1], _var4MemObjEnv_Points[1], CL_TRUE, 0, sizeof(Lane)*4096, _var7Env_Point, 0, NULL, NULL);
    for (int _var8 = 0; _var8 < num_timesteps; _var8++) {
        bool conflictFlag = false;
        ret = clEnqueueNDRangeKernel(command_queues[0], kernel_Point_0s[0], 1, NULL, &globalWorkSize_Point, &localWorkSize, 0, NULL, NULL);
        ret |= clEnqueueNDRangeKernel(command_queues[1], kernel_Point_0s[1], 1, NULL, &globalWorkSize_Point, &localWorkSize, 0, NULL, NULL);
        printf("iter %d \n", _var8);
        clFinish(command_queues[1]);
        clFinish(command_queues[0]);
        clEnqueueReadBuffer(command_queues[1], _var4MemObj_Points[1], CL_TRUE, 0, _var4_Point, _var7buff_Point1, 0, NULL, NULL);
        clEnqueueReadBuffer(command_queues[0], _var4MemObj_Points[0], CL_TRUE, 0, _var4_Point, _var7buff_Point0, 0, NULL, NULL);
        #pragma omp for schedule(static, 128)
        for (int _var9 = 0; _var9 < agents.agents_Point.len; _var9++) {
            Point *_agent = coexecution_merge_Point(&_var7buff_Point0[_var9], &_var7buff_Point1[_var9]);
            _var7buffMerge_Point[_var9] = *_agent;
        }
        clEnqueueWriteBuffer(command_queues[0], _var4MemObj_Points[0], CL_TRUE, 0, _var4_Point, _var7buffMerge_Point, 0, NULL, NULL);
        do {
            bool conflictFlag_Point = false;
            for (int length = 1; length < globalWorkSize_Point; length <<= 1)
            for (int inc = length; inc > 0; inc >>= 1) {
                int dir = length << 1;
                clSetKernelArg(st_kernel_Points[0], 3, sizeof(int), &inc);
                clSetKernelArg(st_kernel_Points[0], 4, sizeof(int), &dir);
                clEnqueueNDRangeKernel(command_queues[0], st_kernel_Points[0], 1, NULL, &globalWorkSize_Point, NULL, 0, NULL, NULL);
                clFinish(command_queues[0]);
            }
            ret |= clEnqueueNDRangeKernel(command_queues[0], mem_kernel_Points[0], 1, NULL, &globalWorkSize_Point, &localWorkSize, 0, NULL, NULL);
            clFinish(command_queues[0]);
            ret |= clEnqueueNDRangeKernel(command_queues[0], cr_kernel_Points[0], 1, NULL, &cr_globalWorkSize_Point, NULL, 0, NULL, NULL);
            clFinish(command_queues[0]);
            clEnqueueReadBuffer(command_queues[0], _var4MemObjConflictFlag_Points[0], CL_TRUE, 0, sizeof(bool), &conflictFlag_Point, 0, NULL, NULL);
            clFinish(command_queues[0]);
        conflictFlag = conflictFlag_Point;
        } while (conflictFlag);
        clEnqueueReadBuffer(command_queues[0], _var4MemObj_Points[0], CL_TRUE, 0, _var4_Point, _var7buff_Point0, 0, NULL, NULL);
        clEnqueueReadBuffer(command_queues[0], _var4MemObjEnv_Points[0], CL_TRUE, 0, sizeof(Lane)*4096, _var7Env_Point, 0, NULL, NULL);
        clEnqueueWriteBuffer(command_queues[1], _var4MemObj_Points[1], CL_TRUE, 0, _var4_Point, _var7buff_Point0, 0, NULL, NULL);
        clEnqueueWriteBuffer(command_queues[1], _var4MemObjEnv_Points[1], CL_TRUE, 0, sizeof(Lane)*4096, _var7Env_Point, 0, NULL, NULL);
        
    }
    ret = clEnqueueReadBuffer(command_queues[0], _var4MemObj_Points[0], CL_TRUE, 0, _var4_Point, agents.agents_Point.values, 0, NULL, NULL);
    
    
    wtime = omp_get_wtime() - wtime;
    printf("Time elapsed is %f seconds\n", wtime);
    return 0;
}
