#include "libabl.h"
#include "time.h"
#include "stdio.h"
#include "CL/cl.h"

#include "omp.h"

typedef struct {
    float2 pos;
    double desiredSpeed;
    float2 velocity;
    bool isFinished;
    int leader;
    int id;
    float2 target;

    int envId;
}Point;
static const type_info Point_info[] = {
    { TYPE_FLOAT2, offsetof(Point, pos), "pos", true },
    { TYPE_FLOAT, offsetof(Point, desiredSpeed), "desiredSpeed", false },
    { TYPE_FLOAT2, offsetof(Point, velocity), "velocity", false },
    { TYPE_BOOL, offsetof(Point, isFinished), "isFinished", false },
    { TYPE_INT, offsetof(Point, leader), "leader", false },
    { TYPE_INT, offsetof(Point, id), "id", false },
    { TYPE_FLOAT2, offsetof(Point, target), "target", false },
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
double T = 0.54;
double rho = 0.05;
double r = 2.0;
double lambda = 2.0;
double gamma = 0.35;
double n_prime = 3.0;
double n = 2.0;
double A = 4.5;
int aa = 3;
double bb = 0.1;
double stepTime = 0.25;
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
int dijkstra(int n, int startnode, int endnode) {
    int cost[10000];
    int distance[100];
    int pred[100];
    int visited[100];
    int count = 0;
    int mindistance = 0;
    int nextnode = 0;
    for (int i = 0, _var0 = (int) n; i < _var0; ++i) {
        for (int j = 0, _var1 = (int) n; j < _var1; ++j) {
            if ((((i - j) == 1) || ((j - i) == 1))) cost[((i * (int) n) + j)] = 1; else cost[((i * (int) n) + j)] = 32767;
        }
    }
    for (int i = 0, _var2 = (int) n; i < _var2; ++i) {
        distance[i] = cost[(((int) startnode * (int) n) + i)];
        pred[i] = startnode;
        visited[i] = 0;
    }
    distance[(int) startnode] = 0;
    visited[(int) startnode] = 1;
    count = 1;
    while ((count < ((int) n - 1))) {
        mindistance = 32767;
        for (int i = 0, _var3 = (int) n; i < _var3; ++i) {
            if (((distance[i] < mindistance) && (!(bool) visited[i]))) {
                mindistance = distance[i];
                nextnode = i;
            }
        }
        visited[nextnode] = 1;
        for (int i = 0, _var4 = (int) n; i < _var4; ++i) {
            if ((!(bool) visited[i])) {
                if (((mindistance + cost[((nextnode * (int) n) + i)]) < distance[i])) {
                    distance[i] = (mindistance + cost[((nextnode * (int) n) + i)]);
                    pred[i] = nextnode;
                }
            }
        }
        count = (count + 1);
    }
    int j = (int) endnode;
    while ((pred[j] != (int) startnode)) {
        j = pred[j];
    }
    return j;
}


Point* coexecution_merge_Point(Point* A0, Point* A1) {
    if ((A0->id == A0->leader)) A0->target = A1->target;
    return A0;
}
int main() {
    double wtime = omp_get_wtime();
    for (int i = 0, _var5 = num_agents; i < _var5; ++i) {
        *DYN_ARRAY_PLACE(&agents.agents_Point, Point) = (Point) {
            .pos = random_4(float2_fill(W)),
            .desiredSpeed = random_float(0.1, 0.5),
            .velocity = float2_create(0, 0),
            .id = i,
            .leader = (128 * (i / 128)),
            .isFinished = false,
            .target = float2_create(1, 1),
        };
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
    size_t _var6_Point = sizeof(Point)*agents.agents_Point.len;
    size_t _var6_Point_dbuf = sizeof(Point)*agents.agents_Point.len;
    cl_kernel kernel_Point_0s[2];
    cl_kernel kernel_Point_1s[2];
    cl_kernel st_kernel_Points[2];
    cl_kernel mem_kernel_Points[2];
    cl_kernel upenv_kernel_Points[2];
    cl_mem _var6MemObj_Points[2];
    cl_mem _var6MemObjDbuf_Points[2];
    cl_mem _var6MemObjLen_Points[2];
    cl_mem _var6MemObjEnv_Points[2];
    int activeDevice[2]={0,1};
    cl_mem _var6MemObjRng_Points[2];

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
        cl_mem _var6MemObj_Point = clCreateBuffer(context, CL_MEM_READ_WRITE, _var6_Point, NULL , &ret);
        _var6MemObj_Points[deviceIter] = _var6MemObj_Point;
        cl_mem _var6MemObjDbuf_Point = clCreateBuffer(context, CL_MEM_READ_WRITE, _var6_Point_dbuf, NULL , &ret);
        _var6MemObjDbuf_Points[deviceIter] = _var6MemObjDbuf_Point;
        cl_mem _var6MemObjLen_Point = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(int), NULL , &ret);
        _var6MemObjLen_Points[deviceIter] = _var6MemObjLen_Point;
        cl_mem _var6MemObjEnv_Point = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(env)*13225, NULL , &ret);
        _var6MemObjEnv_Points[deviceIter] = _var6MemObjEnv_Point;
        cl_uint2 *rngState_Point;
        rngState_Point = (cl_uint2 *)calloc(agents.agents_Point.len, sizeof(cl_uint2));
        for ( int _var7 = 0 ; _var7 < agents.agents_Point.len ; _var7++ ) {
            cl_uint2 _var8 = { (uint32_t)(_var7 * 2), (uint32_t)(_var7 * 2 + 1) };
            rngState_Point[_var7] = _var8;
        }
        cl_mem _var6MemObjRng_Point = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(cl_uint2)*agents.agents_Point.len, NULL , &ret);
        _var6MemObjRng_Points[deviceIter] = _var6MemObjRng_Point;
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
        cl_kernel upenv_kernel_Point = clCreateKernel(program, "update_envId_Point", &ret);
        upenv_kernel_Points[deviceIter] = upenv_kernel_Point;
        ret = clSetKernelArg(kernel_Point_0, 0, sizeof(cl_mem), &_var6MemObj_Point);
        ret |= clSetKernelArg(kernel_Point_0, 1, sizeof(cl_mem), &_var6MemObjDbuf_Point);
        ret |= clSetKernelArg(kernel_Point_0, 2, sizeof(cl_mem), &_var6MemObjLen_Point);
        ret |= clSetKernelArg(kernel_Point_0, 3, sizeof(cl_mem), &_var6MemObjEnv_Point);
        ret |= clSetKernelArg(kernel_Point_0, 4, sizeof(cl_mem), &_var6MemObjRng_Point);
        ret = clSetKernelArg(kernel_Point_1, 0, sizeof(cl_mem), &_var6MemObj_Point);
        ret |= clSetKernelArg(kernel_Point_1, 1, sizeof(cl_mem), &_var6MemObjDbuf_Point);
        ret |= clSetKernelArg(kernel_Point_1, 2, sizeof(cl_mem), &_var6MemObjLen_Point);
        ret |= clSetKernelArg(kernel_Point_1, 3, sizeof(cl_mem), &_var6MemObjEnv_Point);
        ret |= clSetKernelArg(kernel_Point_1, 4, sizeof(cl_mem), &_var6MemObjRng_Point);
        ret = clSetKernelArg(upenv_kernel_Point, 0, sizeof(cl_mem), &_var6MemObj_Point);
        ret |= clSetKernelArg(upenv_kernel_Point, 1, sizeof(cl_mem), &_var6MemObjLen_Point);
        ret = clSetKernelArg(st_kernel_Point, 0, sizeof(cl_mem), &_var6MemObj_Point);
        ret |= clSetKernelArg(st_kernel_Point, 1, sizeof(cl_mem), &_var6MemObjDbuf_Point);
        ret |= clSetKernelArg(st_kernel_Point, 2, sizeof(cl_mem), &_var6MemObjLen_Point);
        ret = clSetKernelArg(mem_kernel_Point, 0, sizeof(cl_mem), &_var6MemObj_Point);
        ret |= clSetKernelArg(mem_kernel_Point, 1, sizeof(cl_mem), &_var6MemObjDbuf_Point);
        ret |= clSetKernelArg(mem_kernel_Point, 2, sizeof(cl_mem), &_var6MemObjLen_Point);
        ret |= clSetKernelArg(mem_kernel_Point, 3, sizeof(cl_mem), &_var6MemObjEnv_Point);

        ret = clEnqueueWriteBuffer(command_queue, _var6MemObj_Point, CL_TRUE, 0, _var6_Point, agents.agents_Point.values, 0, NULL, NULL);
        ret = clEnqueueWriteBuffer(command_queue, _var6MemObjDbuf_Point, CL_TRUE, 0, _var6_Point_dbuf, agents.agents_Point_dbuf.values, 0, NULL, NULL);
        ret = clEnqueueWriteBuffer(command_queue, _var6MemObjLen_Point, CL_TRUE, 0, sizeof(int), &agents.agents_Point.len, 0, NULL, NULL);
        ret = clEnqueueWriteBuffer(command_queue, _var6MemObjRng_Point, CL_TRUE, 0, sizeof(cl_uint2)*agents.agents_Point.len, rngState_Point, 0, NULL, NULL);

    }
    size_t localWorkSize = 128;
    Point* _var9buff_Point0 = calloc(sizeof(Point), agents.agents_Point.len);
    Point* _var9buff_Point1 = calloc(sizeof(Point), agents.agents_Point.len);
    Point* _var9buffMerge_Point = calloc(sizeof(Point), agents.agents_Point.len);
    env *_var9Env_Point = calloc(sizeof(env),13225);
    size_t globalWorkSize_Point = roundWorkSizeUp(128, agents.agents_Point.len);
    ret = clEnqueueNDRangeKernel(command_queues[0], upenv_kernel_Points[0], 1, NULL, &globalWorkSize_Point, &localWorkSize, 0, NULL, NULL);
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
    clEnqueueReadBuffer(command_queues[0], _var6MemObj_Points[0], CL_TRUE, 0, _var6_Point, _var9buff_Point0, 0, NULL, NULL);
    clEnqueueReadBuffer(command_queues[0], _var6MemObjEnv_Points[0], CL_TRUE, 0, sizeof(env)*13225, _var9Env_Point, 0, NULL, NULL);
    clEnqueueWriteBuffer(command_queues[1], _var6MemObj_Points[1], CL_TRUE, 0, _var6_Point, _var9buff_Point0, 0, NULL, NULL);
    clEnqueueWriteBuffer(command_queues[1], _var6MemObjEnv_Points[1], CL_TRUE, 0, sizeof(env)*13225, _var9Env_Point, 0, NULL, NULL);
    for (int _var10 = 0; _var10 < num_timesteps; _var10++) {
        ret = clEnqueueNDRangeKernel(command_queues[0], kernel_Point_0s[0], 1, NULL, &globalWorkSize_Point, &localWorkSize, 0, NULL, NULL);
        ret |= clEnqueueNDRangeKernel(command_queues[1], kernel_Point_0s[1], 1, NULL, &globalWorkSize_Point, &localWorkSize, 0, NULL, NULL);
        printf("%d %d\n",_var10,ret);
        clFinish(command_queues[1]);
        clFinish(command_queues[0]);
        clEnqueueReadBuffer(command_queues[1], _var6MemObj_Points[1], CL_TRUE, 0, _var6_Point, _var9buff_Point1, 0, NULL, NULL);
        clEnqueueReadBuffer(command_queues[0], _var6MemObj_Points[0], CL_TRUE, 0, _var6_Point, _var9buff_Point0, 0, NULL, NULL);
        #pragma omp for schedule(static, 128)
        for (int _var11 = 0; _var11 < agents.agents_Point.len; _var11++) {
            Point *_agent = coexecution_merge_Point(&_var9buff_Point0[_var11], &_var9buff_Point1[_var11]);
            _var9buffMerge_Point[_var11] = *_agent;
        }
        clEnqueueWriteBuffer(command_queues[0], _var6MemObj_Points[0], CL_TRUE, 0, _var6_Point, _var9buffMerge_Point, 0, NULL, NULL);
        ret = clEnqueueNDRangeKernel(command_queues[0], upenv_kernel_Points[0], 1, NULL, &globalWorkSize_Point, &localWorkSize, 0, NULL, NULL);
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
        clEnqueueReadBuffer(command_queues[0], _var6MemObj_Points[0], CL_TRUE, 0, _var6_Point, _var9buff_Point0, 0, NULL, NULL);
        clEnqueueReadBuffer(command_queues[0], _var6MemObjEnv_Points[0], CL_TRUE, 0, sizeof(env)*13225, _var9Env_Point, 0, NULL, NULL);
        clEnqueueWriteBuffer(command_queues[1], _var6MemObj_Points[1], CL_TRUE, 0, _var6_Point, _var9buff_Point0, 0, NULL, NULL);
        clEnqueueWriteBuffer(command_queues[1], _var6MemObjEnv_Points[1], CL_TRUE, 0, sizeof(env)*13225, _var9Env_Point, 0, NULL, NULL);
        
    }
    ret = clEnqueueReadBuffer(command_queues[0], _var6MemObj_Points[0], CL_TRUE, 0, _var6_Point, agents.agents_Point.values, 0, NULL, NULL);
    
    wtime = omp_get_wtime() - wtime;
    printf("Time elapsed is %f seconds\n", wtime);
    return 0;
}
