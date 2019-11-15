#include "libopenl.h"

typedef struct { 
    int mem_start;
    int mem_end;
} env;
typedef struct {
    flo2 pos;
    double desiredSpeed;
    flo2 velocity;
    bool isFinished;
    int leader;
    int id;
    flo2 target;

    int envId;
}Point;

__constant double PI = 3.14159;
__constant int num_timesteps = 100;
__constant int num_agents = 65536;
__constant double T = 0.54;
__constant double rho = 0.05;
__constant double r = 2.0;
__constant double lambda = 2.0;
__constant double gamma = 0.35;
__constant double n_prime = 3.0;
__constant double n = 2.0;
__constant double A = 4.5;
__constant int aa = 3;
__constant double bb = 0.1;
__constant double stepTime = 0.25;
__constant double W = 1144.87;

flo2 random_1(flo2 min, flo2 max) {
    return flo2_create(random_float(min.x, max.x), random_float(min.y, max.y));
}
flo3 random_2(flo3 min, flo3 max) {
    return flo3_create(random_float(min.x, max.x), random_float(min.y, max.y), random_float(min.z, max.z));
}
double random_3(double max) {
    return random_float(0, max);
}
flo2 random_4(flo2 max) {
    return random_1(flo2_fill(0), max);
}
flo3 random_5(flo3 max) {
    return random_2(flo3_fill(0), max);
}
int randomInt_1(int max) {
    return random_int(0, max);
}
double clam(double pos, double min, double max) {
    return ((pos < min) ? min : ((pos > max) ? max : pos));
}
flo2 clam_1(flo2 pos, flo2 min, flo2 max) {
    return flo2_create(((pos.x < min.x) ? min.x : ((pos.x > max.x) ? max.x : pos.x)), ((pos.y < min.y) ? min.y : ((pos.y > max.y) ? max.y : pos.y)));
}
flo3 clam_2(flo3 pos, flo3 min, flo3 max) {
    return flo3_create(((pos.x < min.x) ? min.x : ((pos.x > max.x) ? max.x : pos.x)), ((pos.y < min.y) ? min.y : ((pos.y > max.y) ? max.y : pos.y)), ((pos.z < min.z) ? min.z : ((pos.z > max.z) ? max.z : pos.z)));
}
double clam_3(double pos, double max) {
    return clam(pos, 0, max);
}
flo2 clam_4(flo2 pos, flo2 max) {
    return clam_1(pos, flo2_fill(0), max);
}
flo3 clam_5(flo3 pos, flo3 max) {
    return clam_2(pos, flo3_fill(0), max);
}
double wraparound(double pos, double max) {
    return ((pos < 0) ? (max + pos) : ((pos >= max) ? (pos - max) : pos));
}
flo2 wraparound_1(flo2 pos, flo2 max) {
    return flo2_create(((pos.x < 0) ? (max.x + pos.x) : ((pos.x >= max.x) ? (pos.x - max.x) : pos.x)), ((pos.y < 0) ? (max.y + pos.y) : ((pos.y >= max.y) ? (pos.y - max.y) : pos.y)));
}
flo3 wraparound_2(flo3 pos, flo3 max) {
    return flo3_create(((pos.x < 0) ? (max.x + pos.x) : ((pos.x >= max.x) ? (pos.x - max.x) : pos.x)), ((pos.y < 0) ? (max.y + pos.y) : ((pos.y >= max.y) ? (pos.y - max.y) : pos.y)), ((pos.z < 0) ? (max.z + pos.z) : ((pos.z >= max.z) ? (pos.z - max.z) : pos.z)));
}
bool is_inside(flo2 pos, flo2 max) {
    return ((((pos.x >= 0) && (pos.y >= 0)) && (pos.x <= max.x)) && (pos.y <= max.y));
}
bool isSortNeeded(int p1, int p2) {
    if ((p1 > p2)) return true;
    return false;
}
bool isSortNeeded_1(flo2 p1, flo2 p2) {
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
void compute_leader_route(__global Point *buff_Point, __global int *len_Point, __global env *envo_Point,  __global uint2 *rngState, int agentIId, __global int *interaction_current, __global bool *isProfilingIter, Point* in, Point*out) {
    if ((in->isFinished || (in->leader != in->id))) return;
    if (*isProfilingIter)
        atomic_inc(interaction_current);
    double width = (W / 10.0);
    int current = ((((int) (( double ) in->pos.x / width) * 10) + (int) (in->pos.y / width)) + 1);
    int dd = dijkstra(100, current, 1);
    double yy = (( double ) ((dd / 10) + 1) * (width / 2.0));
    double xx = (( double ) (dd % 10) * (width / 2.0));
    flo2 e_i = flo2_create(xx, yy);
    out->target = e_i;
}
void move_all_point(__global Point *buff_Point, __global int *len_Point, __global env *envo_Point,  __global uint2 *rngState, int agentIId, __global int *interaction_current, __global bool *isProfilingIter, Point* in, Point*out) {
    if (in->isFinished) return;
    flo2 new_pos = in->pos;
    if ((in->leader != in->id)) {
        double x = (MWC64X(&rngState[agentIId])*W);
        double y = (MWC64X(&rngState[agentIId])*W);
        in->target = flo2_create(x, y);
        for (int _var8 = -1; _var8 < 2; _var8++)
        for (int _var9 = - ((int)(1144.87/10)+1); _var9 < ((int)(1144.87/10)+1)+1 ; _var9 =  _var9 + ((int)(1144.87/10+1))) {
            int _var10 = 0;
            int envId = in->envId + _var8 + _var9 + _var10;
            if ( envId >= 0 && envId < 13225 )
            {
                int _var11 = envo_Point[envId].mem_start;
                int _var12 = envo_Point[envId].mem_end;
                if (_var11 != _var12)
                for (size_t _var6 = _var11;_var6 < _var12;_var6++) {
                    Point _var7 = buff_Point[_var6];
                    Point* nx= &_var7;
                    if (dist_flo2(nx->pos, in->pos) > (5.0 * r)) continue;
                    {
                        if (*isProfilingIter)
                            atomic_inc(interaction_current);
                        if (nx->isFinished) continue;
                        if ((nx->id == nx->leader)) {
                            in->target = nx->pos;
                            out->target = nx->pos;
                            break;
                        }
                    }
                }
            }
        }
    }
    flo2 e_i = flo2_sub(in->target, in->pos);
    e_i = normalize_flo2(e_i);
    flo2 f_i = flo2_mul_scalar(flo2_sub(flo2_mul_scalar(e_i, in->desiredSpeed), in->velocity), (1.0 / T));
    flo2 f_ij = flo2_create(0, 0);
    for (int _var16 = -1; _var16 < 2; _var16++)
    for (int _var17 = - ((int)(1144.87/10)+1); _var17 < ((int)(1144.87/10)+1)+1 ; _var17 =  _var17 + ((int)(1144.87/10+1))) {
        int _var18 = 0;
        int envId = in->envId + _var16 + _var17 + _var18;
        if ( envId >= 0 && envId < 13225 )
        {
            int _var19 = envo_Point[envId].mem_start;
            int _var20 = envo_Point[envId].mem_end;
            if (_var19 != _var20)
            for (size_t _var14 = _var19;_var14 < _var20;_var14++) {
                Point _var15 = buff_Point[_var14];
                Point* nx= &_var15;
                if (dist_flo2(nx->pos, in->pos) > (1.0 * r)) continue;
                {
                    if (*isProfilingIter)
                       atomic_inc(interaction_current);
                    if (nx->isFinished) continue;
                    flo2 distance_ij = flo2_sub(nx->pos, in->pos);
                    if ((length_flo2(distance_ij) == 0)) continue;
                    flo2 e_ij = distance_ij;
                    e_ij = normalize_flo2(e_ij);
                    flo2 D_ij = flo2_add(flo2_mul_scalar(flo2_sub(in->velocity, nx->velocity), lambda), e_ij);
                    double B = (gamma * length_flo2(D_ij));
                    flo2 t_ij = D_ij;
                    t_ij = normalize_flo2(t_ij);
                    double dot = dot_flo2(t_ij, e_ij);
                    double det = ((( double ) t_ij.x * e_ij.y) - (t_ij.y * ( double ) e_ij.x));
                    double theta = atan2(det, dot);
                    int K = 0;
                    if ((theta < 0)) K = (-1); else if ((theta > 0)) K = 1; else K = 0;
                    double f_v = ((-A) * exp((((-length_flo2(distance_ij)) / B) - (((n_prime * B) * theta) * ((n_prime * B) * theta)))));
                    double f_theta = (((-A) * ( double ) K) * exp((((-length_flo2(distance_ij)) / B) - (((n * B) * theta) * ((n * B) * theta)))));
                    flo2 n_ij = flo2_create((-t_ij.y), t_ij.x);
                    f_ij = flo2_add(f_ij, flo2_add(flo2_mul_scalar(t_ij, f_v), flo2_mul_scalar(n_ij, f_theta)));
                }
            }
        }
    }
    flo2 f_iw = flo2_create(min(in->pos.x, (W - ( double ) in->pos.x)), min(in->pos.y, (W - in->pos.y)));
    double d_w = min(min(in->pos.y, (W - in->pos.y)), min(in->pos.x, (W - ( double ) in->pos.x)));
    double f_wv = (( double ) aa * exp(((-d_w) / bb)));
    f_iw = flo2_mul_scalar(normalize_flo2(f_iw), f_wv);
    flo2 force = flo2_add(flo2_add(f_i, f_ij), f_iw);
    out->velocity = flo2_add(in->velocity, flo2_mul_scalar(force, stepTime));
    if ((length_flo2(out->velocity) > in->desiredSpeed)) out->velocity = flo2_mul_scalar(normalize_flo2(out->velocity), in->desiredSpeed);
    out->pos = flo2_add(new_pos, flo2_mul_scalar(out->velocity, stepTime));
    if (((new_pos.x <= 1) && (new_pos.y <= 10))) out->isFinished = true;
}
Point* coexecution_merge_Point(Point* A0, Point* A1) {
    if ((A0->id == A0->leader)) A0->target = A1->target;
    return A0;
}

__kernel void compute_kernel_0_Point_0(__global Point *buff_Point, __global Point *dbuff_Point, __global int *len_Point, __global env *envo_Point,  __global uint2 *rngState_Point, __global int *interaction_current, __global bool *isProfilingIter) {
    int _var24 = get_global_id(0);
    if (_var24 >= *len_Point) return;
    Point _var25 = buff_Point[_var24];
    Point _var26 = _var25;
    compute_leader_route(buff_Point, len_Point, envo_Point, rngState_Point, _var24, interaction_current, isProfilingIter, &_var25, &_var26);
    if (!*isProfilingIter) {
        buff_Point[_var24] = _var26;
        dbuff_Point[_var24] = _var25;
    }
}
__kernel void compute_kernel_1_Point_0(__global Point *buff_Point, __global Point *dbuff_Point, __global int *len_Point, __global env *envo_Point,  __global uint2 *rngState_Point, __global int *interaction_current, __global bool *isProfilingIter) {
    int _var24 = get_global_id(0);
    if (_var24 >= *len_Point) return;
    Point _var25 = buff_Point[_var24];
    Point _var26 = _var25;
    compute_leader_route(buff_Point, len_Point, envo_Point, rngState_Point, _var24, interaction_current, isProfilingIter, &_var25, &_var26);
    if (!*isProfilingIter) {
        buff_Point[_var24] = _var26;
        dbuff_Point[_var24] = _var25;
    }
}
__kernel void compute_kernel_0_Point_1(__global Point *buff_Point, __global Point *dbuff_Point, __global int *len_Point, __global env *envo_Point,  __global uint2 *rngState_Point, __global int *interaction_current, __global bool *isProfilingIter) {
    int _var21 = get_global_id(0);
    if (_var21 >= *len_Point) return;
    Point _var22 = buff_Point[_var21];
    Point _var23 = _var22;
    move_all_point(buff_Point, len_Point, envo_Point, rngState_Point, _var21, interaction_current, isProfilingIter, &_var22, &_var23);
    if (!*isProfilingIter) {
        buff_Point[_var21] = _var23;
        dbuff_Point[_var21] = _var22;
    }
}
__kernel void compute_kernel_1_Point_1(__global Point *buff_Point, __global Point *dbuff_Point, __global int *len_Point, __global env *envo_Point,  __global uint2 *rngState_Point, __global int *interaction_current, __global bool *isProfilingIter) {
    int _var21 = get_global_id(0);
    if (_var21 >= *len_Point) return;
    Point _var22 = buff_Point[_var21];
    Point _var23 = _var22;
    move_all_point(buff_Point, len_Point, envo_Point, rngState_Point, _var21, interaction_current, isProfilingIter, &_var22, &_var23);
    if (!*isProfilingIter) {
        buff_Point[_var21] = _var23;
        dbuff_Point[_var21] = _var22;
    }
}

__kernel void update_envId_Point(__global Point *buff_Point, __global int *len_Point) {
    int _var31 = get_global_id(0);
    if (_var31 >= *len_Point) return;
    Point _var32 = buff_Point[_var31];
    _var32.envId = (int)((_var32.pos.y - 0)/10) * ((int)(1144.87/10)+1) + (int)((_var32.pos.x - 0)/10);
    buff_Point[_var31] = _var32;
}
__kernel void sorting_Point(__global Point *buff_Point, __global Point *dbuff_Point, __global int *len_Point, int inc, int dir) {
    int i = get_global_id(0);
    int j = i ^ inc;
    if (i>j) return;
    Point ag1 = buff_Point[i];
    Point ag2 = buff_Point[j];
    bool smaller = isSortNeeded(ag1.envId, ag2.envId);
    bool swap = smaller ^ (j < i) ^ ((dir & i) != 0);
    if (swap) {
        Point tmp = buff_Point[i];
        buff_Point[i] = buff_Point[j];
        buff_Point[j] = tmp;
        tmp = dbuff_Point[i];
        dbuff_Point[i] = dbuff_Point[j];
        dbuff_Point[j] = tmp;
    }
}
__kernel void mem_update_Point(__global Point *buff_Point, __global Point *dbuff_Point, __global int *len_Point, __global env *envo_Point) {
    int i = get_global_id(0);
    if (i > *len_Point-1) return;
    int x = buff_Point[i].envId;
    int y = (i==0)?0:buff_Point[i-1].envId;
    
    if (i == 0) {
        envo_Point[x].mem_start = 0;
    }
    else if (i < *len_Point) {
        if (x != y) {
            envo_Point[y].mem_end = i;
            envo_Point[x].mem_start = i;
        }
        if (i == *len_Point-1) {
            envo_Point[x].mem_end = *len_Point;
        }
    }
}

__kernel void check_interaction_rate_0(__global int *interaction_current, __global int *interaction_last, __global bool *redoProfiling) {
    *redoProfiling = false;
    printf("func0 %d %d\n",*interaction_current,*interaction_last);

    if (*interaction_last == 0) {
        *interaction_last = *interaction_current;
    } else {
        float rate = (float)abs_diff(*interaction_current, *interaction_last)/(float)*interaction_last;
        if (rate > 0.4) {
            *redoProfiling = true;
            *interaction_last = *interaction_current;
        }
    }
}
__kernel void check_interaction_rate_1(__global int *interaction_current, __global int *interaction_last, __global bool *redoProfiling) {
    *redoProfiling = false;
    printf("func1 %d %d\n",*interaction_current,*interaction_last);

    if (*interaction_last == 0) {
        *interaction_last = *interaction_current;
    } else {
        float rate = (float)abs_diff(*interaction_current, *interaction_last)/(float)*interaction_last;
        if (rate > 0.4) {
            *redoProfiling = true;
            *interaction_last = *interaction_current;
        }
    }
}

