#include "libopenl.h"

typedef struct { 
    int mem_start;
    int mem_end;
} env;
typedef struct {
    flo2 pos;

    int envId;
}Point;

__constant double PI = 3.14159;
__constant int num_timesteps = 1000;
__constant int num_agents = 1024;
__constant double rho = 0.05;
__constant double k_rep = 0.05;
__constant double k_att = 0.01;
__constant double r = 5.0;
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
void move_point(__global int *len, __global Point *buff, __global env *envo, __global int *interaction_current, Point* in, Point*out) {
    flo2 new_pos = in->pos;
    for (int _var3 = -1; _var3 < 2; _var3++)
    for (int _var4 = - ((int)(1144.87/10)+1); _var4 < ((int)(1144.87/10)+1)+1 ; _var4 =  _var4 + ((int)(1144.87/10+1))) {
        int _var5 = 0;
        int envId = in->envId + _var3 + _var4 + _var5;
        if ( envId >= 0 && envId < 13225 )
        {
            int _var6 = envo[envId].mem_start;
            int _var7 = envo[envId].mem_end;
            if (_var6 != _var7)
            for (size_t _var1 = _var6;_var1 < _var7;_var1++) {
                Point _var2 = buff[_var1];
                Point* nx= &_var2;
                if (dist_flo2(nx->pos, in->pos) > (2.0 * r)) continue;
                {
                    double pos_dist = dist_flo2(in->pos, nx->pos);
                    if ((pos_dist == 0)) continue;
                    atomic_inc(interaction_current);
                    double sep_dist = (pos_dist - r);
                    flo2 force = flo2_fill(0);
                    if ((sep_dist > 0.0)) {
                        force = flo2_div_scalar(flo2_mul_scalar(flo2_sub(nx->pos, in->pos), (k_att * ((2.0 * r) - pos_dist))), pos_dist);
                    } else {
                        force = flo2_mul_scalar(flo2_sub(in->pos, nx->pos), k_rep);
                    }
                    new_pos = flo2_add(new_pos, force);
                }
            }
        }
    }
    out->pos = clam_1(new_pos, flo2_fill(1), flo2_fill((W - 1.0)));
}
__kernel void compute_kernel_0(__global Point *buff, __global Point *dbuff, __global int *len, __global env *envo, __global int *interaction_current) {
    int _var8 = get_global_id(0);
    if (_var8 >= *len) return;
    Point _var9 = buff[_var8];
    Point _var10 = _var9;
    move_point(len, buff, envo, interaction_current, &_var9, &_var10);
    buff[_var8] = _var10;
    dbuff[_var8] = _var9;
}
__kernel void compute_kernel_1(__global Point *buff, __global Point *dbuff, __global int *len, __global env *envo, __global int *interaction_current) {
    int _var8 = get_global_id(0);
    if (_var8 >= *len) return;
    Point _var9 = buff[_var8];
    Point _var10 = _var9;
    move_point(len, buff, envo, interaction_current, &_var9, &_var10);
    buff[_var8] = _var10;
    dbuff[_var8] = _var9;
}
__kernel void update_envId(__global Point *buff, __global int *len, __global int *interaction_current) {
    int _var15 = get_global_id(0);
    if (_var15 >= *len) return;
    Point _var16 = buff[_var15];
    _var16.envId = (int)((_var16.pos.y - 0)/10) * ((int)(1144.87/10)+1) + (int)((_var16.pos.x - 0)/10);
    buff[_var15] = _var16;
    *interaction_current = 0;
}
__kernel void sorting(__global Point *buff, __global Point *dbuff, __global int *len, int inc, int dir) {
    int i = get_global_id(0);
    int j = i ^ inc;
    if (i>j) return;
    Point ag1 = buff[i];
    Point ag2 = buff[j];
    bool smaller = isSortNeeded(ag1.envId, ag2.envId);
    bool swap = smaller ^ (j < i) ^ ((dir & i) != 0);
    if (swap) {
        Point tmp = buff[i];
        buff[i] = buff[j];
        buff[j] = tmp;
        tmp = dbuff[i];
        dbuff[i] = dbuff[j];
        dbuff[j] = tmp;
    }
}
__kernel void mem_update(__global Point *buff, __global Point *dbuff, __global int *len, __global env *envo) {
    int i = get_global_id(0);
    if (i>*len-1) return;
    int x = buff[i].envId;
    int y = (i==0)?0:buff[i-1].envId;
   // printf("%d %f %f\n",i,buff[i].pos.x,buff[i].pos.y);
    if (i == 0) {
        envo[x].mem_start = 0;
    }
    else if (i < *len) {
        if (x != y) {
            envo[y].mem_end = i;
            envo[x].mem_start = i;
        }
        if (i==*len-1) {
            envo[x].mem_end = *len;
        }
    }
}
__kernel void check_interaction_rate(__global int *interaction_current, __global int *interaction_last, __global bool *redoProfiling) {
    *redoProfiling = false;

    if (*interaction_last == 0) {
        *interaction_last = *interaction_current;
    } else {
        float rate = (float)abs_diff(*interaction_current, *interaction_last)/(float)*interaction_last;
        //printf("%d %d %f\n",*interaction_current,*interaction_last,rate);
        if (rate > 0.5) {
            *redoProfiling = true;
            *interaction_last = *interaction_current;
        }
    }
}

