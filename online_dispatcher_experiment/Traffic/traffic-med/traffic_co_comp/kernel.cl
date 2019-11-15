#include "libopenl_int.h"

typedef struct {
    double velocity;
    flo2 pos;
    int id;
    bool isFinished;
    bool forwardOnLane;
    double milage;
}Point;

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

__constant double PI = 3.14159;
__constant int num_timesteps = 1000;
__constant int num_agents = 8192;
__constant double dt = 250.0;
__constant double size = 3.5;

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
void car_follow(__global Point *buff_Point, __global int *len_Point, __global Lane *roads,  __global uint2 *rngState, int agentIId, Point* in, Point*out) {
    if (in->isFinished) return;
    double desired_velocity = ( double ) roads[in->pos.x].speedLimit;
    double ratio = 0.0;
    double free_road_term = 0.0;
    out->forwardOnLane = false;
    if ((in->velocity < desired_velocity)) {
        ratio = (in->velocity / desired_velocity);
        free_road_term = (1.8 * (1.0 - pow(ratio, 4)));
    } else {
        ratio = (desired_velocity / in->velocity);
        free_road_term = ((-2.0) * (1.0 - pow(ratio, 4)));
    }
    double interaction_term = 0.0;
    Lane _var0 = roads[in->pos.x];
    Lane* current = &_var0;
    if (current->mem_start != current->mem_end)
    for (size_t _var2 = current->mem_start;_var2 < current->mem_end;_var2++) {
        Point _var3 = buff_Point[_var2];
        Point* nx= &_var3;
        {
            if ((nx->pos.y > in->pos.y)) {
                double dV = (in->velocity - nx->velocity);
                double temp = ((in->velocity * 1.5) + ((in->velocity * dV) / 3.79));
                double ss = (2.0 + temp);
                double pos_dist = (nx->pos.y - in->pos.y);
                interaction_term = ((-1.8) * pow((ss / pos_dist), 2.0));
                break;
            }
        }
    }
    double acceleration = (free_road_term + interaction_term);
    out->velocity = (in->velocity + (acceleration * (dt / 1000.0)));
    if ((out->velocity < 0)) out->velocity = 0;
    out->pos.y = (in->pos.y + (out->velocity * (dt / 1000.0)));
    out->milage = (in->milage + (out->velocity * (dt / 1000.0)));
    if ((out->milage > (500))) {
        out->isFinished = true;
    }
    if ((out->pos.y > roads[out->pos.x].length)) {
        if ((roads[out->pos.x].nextLaneId != (-1))) {
            out->pos.x = roads[in->pos.x].nextLaneId;
            out->pos.y = (out->pos.y - roads[out->pos.x].length);
            out->forwardOnLane = true;
        } else {
            out->isFinished = true;
        }
    }
}
void lane_change(__global Point *buff_Point, __global int *len_Point, __global Lane *roads,  __global uint2 *rngState, int agentIId, Point* in, Point*out) {
    if (in->isFinished) return;
    double dis_rear = (-50.0);
    double dis_front = 50.0;
    double dis_rear_left = (-50.0);
    double dis_front_left = 50.0;
    double dis_rear_right = (-50.0);
    double dis_front_right = 50.0;
    double vel_rear = 0.0;
    double vel_front = 30.0;
    double vel_rear_left = 0.0;
    double vel_front_left = 30.0;
    double vel_rear_right = 0.0;
    double vel_front_right = 30.0;
    double dis_tmp_right = 0.0;
    double dis_tmp_left = 0.0;
    double dis_tmp = 0.0;
    if ((roads[in->pos.x].leftLaneId != (-1))) {
        Lane _var4 = roads[roads[in->pos.x].leftLaneId];
        Lane* left = &_var4;
        if (left->mem_start != left->mem_end)
        for (size_t _var6 = left->mem_start;_var6 < left->mem_end;_var6++) {
            Point _var7 = buff_Point[_var6];
            Point* nx= &_var7;
            {
                dis_tmp_left = (nx->pos.y - in->pos.y);
                if (((dis_tmp_left > 0) && (dis_tmp_left < dis_front_left))) {
                    dis_front_left = (dis_tmp_left - size);
                    vel_front_left = nx->velocity;
                }
                if (((dis_tmp_left < 0) && (dis_tmp_left > dis_rear_left))) {
                    dis_rear_left = (dis_tmp_left + size);
                    vel_rear_left = nx->velocity;
                }
            }
        }
    }
    if ((roads[in->pos.x].rightLaneId != (-1))) {
        Lane _var8 = roads[roads[in->pos.x].rightLaneId];
        Lane* right = &_var8;
        if (right->mem_start != right->mem_end)
        for (size_t _var10 = right->mem_start;_var10 < right->mem_end;_var10++) {
            Point _var11 = buff_Point[_var10];
            Point* nx= &_var11;
            {
                dis_tmp_right = (nx->pos.y - in->pos.y);
                if (((dis_tmp_right > 0) && (dis_tmp_right < dis_front_right))) {
                    dis_front_right = (dis_tmp_right - size);
                    vel_front_right = nx->velocity;
                }
                if (((dis_tmp_right < 0) && (dis_tmp_right > dis_rear_right))) {
                    dis_rear_right = (dis_tmp_right + size);
                    vel_rear_right = nx->velocity;
                }
            }
        }
    }
    Lane _var12 = roads[in->pos.x];
    Lane* current = &_var12;
    if (current->mem_start != current->mem_end)
    for (size_t _var14 = current->mem_start;_var14 < current->mem_end;_var14++) {
        Point _var15 = buff_Point[_var14];
        Point* nx= &_var15;
        {
            dis_tmp = (nx->pos.y - in->pos.y);
            if (((dis_tmp > 0) && (dis_tmp < dis_front))) {
                dis_front = (dis_tmp - size);
                vel_front = nx->velocity;
            }
            if (((dis_tmp < 0) && (dis_tmp > dis_rear))) {
                dis_rear = (dis_tmp + size);
                vel_rear = nx->velocity;
            }
        }
    }
    double con_e = 2.71828;
    double a_l = (((25.0 * in->velocity) * (vel_front_left - in->velocity)) / (dis_front_left * dis_front_left));
    double a_c = (((25.0 * in->velocity) * (vel_front - in->velocity)) / (dis_front * dis_front));
    double a_r = (((25.0 * in->velocity) * (vel_front_right - in->velocity)) / (dis_front_right * dis_front_right));
    double p_a_l = pow(con_e, a_l);
    double p_a_c = pow(con_e, a_c);
    double p_a_r = pow(con_e, a_r);
    double p_base = ((p_a_l + p_a_c) + p_a_r);
    double p_l = (p_a_l / p_base);
    double p_c = (p_a_c / p_base);
    double p_r = (p_a_r / p_base);
    double a_rear_l = (((25.0 * vel_rear_left) * (in->velocity - vel_rear_left)) / (dis_rear_left * dis_rear_left));
    double a_rear_r = (((25.0 * vel_rear_right) * (in->velocity - vel_rear_right)) / (dis_rear_right * dis_rear_right));
    double aa = ((-0.78) * ((dis_front_right / in->velocity) - 0.7));
    double bb = ((-0.78) * (((-dis_rear_right) / in->velocity) - 0.7));
    double p_right_lead = (1.0 - pow(con_e, aa));
    double p_right_lag = (1.0 - pow(con_e, bb));
    double p_right = (p_right_lead * p_right_lag);
    if (((dis_rear_right > (-size)) || (dis_front_right < size))) p_right = 0;
    double cc = ((-0.78) * ((dis_front_left / in->velocity) - 0.7));
    double dd = ((-0.78) * (((-dis_rear_left) / in->velocity) - 0.7));
    double p_left_lead = (1.0 - pow(con_e, cc));
    double p_left_lag = (1.0 - pow(con_e, dd));
    double p_left = (p_left_lead * p_left_lag);
    if (((dis_rear_left > (-size)) || (dis_front_left < size))) p_left = 0;
    double rnd1 = (MWC64X(&rngState[agentIId])*1.0);
    double rnd2 = (MWC64X(&rngState[agentIId])*1.0);
    if ((((rnd1 <= p_l) && (a_rear_l > (-2.0))) && (rnd2 <= p_left))) {
        if ((roads[in->pos.x].leftLaneId != (-1))) {
            out->pos.x = roads[in->pos.x].leftLaneId;
        }
    } else if ((((rnd1 <= (p_r + p_l)) && (a_rear_r > (-2.0))) && (rnd2 <= p_right))) {
        if ((roads[in->pos.x].rightLaneId != (-1))) {
            out->pos.x = roads[in->pos.x].rightLaneId;
        }
    } else {
        out->pos.x = in->pos.x;
    }
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
__kernel void compute_kernel_1_Point_0(__global Point *buff_Point, __global Point *dbuff_Point, __global int *len_Point, __global Lane *roads_Point,  __global uint2 *rngState_Point) {
    int _var16 = get_global_id(0);
    if (_var16 >= *len_Point) return;
    Point _var17 = buff_Point[_var16];
    Point _var18 = _var17;
    car_follow(buff_Point, len_Point, roads_Point, rngState_Point, _var16, &_var17, &_var18);
    buff_Point[_var16] = _var18;
    dbuff_Point[_var16] = _var17;
}
__kernel void compute_kernel_0_Point_0(__global Point *buff_Point, __global Point *dbuff_Point, __global int *len_Point, __global Lane *roads_Point,  __global uint2 *rngState_Point) {
    int _var19 = get_global_id(0);
    if (_var19 >= *len_Point) return;
    Point _var20 = buff_Point[_var19];
    Point _var21 = _var20;
    lane_change(buff_Point, len_Point, roads_Point, rngState_Point, _var19, &_var20, &_var21);
    buff_Point[_var19] = _var21;
    dbuff_Point[_var19] = _var20;
}
__kernel void sorting_Point(__global Point *buff_Point, __global Point *dbuff_Point, __global int *len_Point, int inc, int dir) {
    int i = get_global_id(0);
    int j = i ^ inc;
    if (i>j) return;
    Point ag1 = buff_Point[i];
    Point ag2 = buff_Point[j];
    bool smaller = isSortNeeded_1(ag1.pos, ag2.pos);
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
__kernel void mem_update_Point(__global Point *buff_Point, __global Point *dbuff_Point, __global int *len_Point, __global Lane *roads_Point, __global bool *isConflicted_Point) {
    int i = get_global_id(0);
    if (i > *len_Point-1) return;
    int x = buff_Point[i].pos.x;
    int y = buff_Point[i-1].pos.x;
    
    if (i == 0) {
        roads_Point[x].mem_start = 0;
    }
    else if (i < *len_Point) {
        if (x != y) {
            roads_Point[y].mem_end = i;
            roads_Point[x].mem_start = i;
        }
        if (i == *len_Point-1) {
            roads_Point[x].mem_end = *len_Point;
        }
    }
    *isConflicted_Point = false;
}

__kernel void conflict_resolver_Point(__global Point *buff_Point, __global Point *dbuff_Point, __global int *len_Point, __global Lane *roads_Point, __global bool *isConflicted_Point) {
    int i = get_global_id(0);
    if (i > 4096) return;
    Lane _var26 = roads_Point[i];
    if (_var26.mem_start !=_var26.mem_end)
    for (size_t _var27 = _var26.mem_start;_var27 < _var26.mem_end; _var27++)
    for (size_t _var28 = _var26.mem_start;_var28 < _var26.mem_end; _var28++) {
        if (_var27!=_var28) {
            Point _var29 = buff_Point[_var27];
            Point _var30 = buff_Point[_var28];
            if (tie_breaking(&_var29, &_var30)) {
                if ((buff_Point[_var27].pos.x != dbuff_Point[_var27].pos.x) || (buff_Point[_var27].pos.y != dbuff_Point[_var27].pos.y))
                    buff_Point[_var27] = dbuff_Point[_var27];
                else
                    buff_Point[_var28] = dbuff_Point[_var28];
                *isConflicted_Point = true;
            }
        }
    }
}

