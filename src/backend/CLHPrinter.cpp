/* Copyright 2017 OpenABL Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License. */

#include <string>
#include "CLHPrinter.hpp"

namespace OpenABL {

void CLHPrinter::printType(Type type) {
  if (type.isArray()) {
    // Print only the base type
    *this << type.getBaseType();
  } else if (type.isAgent()) {
    *this << type.getAgentDecl()->name << '*';
  } else if (type.isFloat()) {
    *this << (useFloat ? "float" : "double");
  } else {
    *this << type;
  }
}

static CLHPrinter &operator<<(CLHPrinter &s, Type type) {
  s.printType(type);
  return s;
}

static void printStorageType(CLHPrinter &s, Type type) {
  if (type.isAgent()) {
    // Don't use a pointer
    s << type.getAgentDecl()->name;
  } else {
    s.printType(type);
  }
}

static bool typeRequiresStorage(Type type) {
  return type.isArray() || type.isAgent();
}

static void printTypeCtor(CLHPrinter &p, const AST::CallExpression &expr) {
  Type t = expr.type;
  if (t.isVec()) {
    size_t numArgs = expr.args->size();
    p << "float" << t.getVecLen() << "_"
      << (numArgs == 1 ? "fill" : "create");
    p << "(";
    p.printArgs(expr);
    p << ")";
  } else {
    p << "(" << t << ") " << *(*expr.args)[0];
  }
}
void CLHPrinter::print(const AST::CallExpression &expr) {
  if (expr.isCtor()) {
    printTypeCtor(*this, expr);
  } else {
    const FunctionSignature &sig = expr.calledSig;
    if (sig.name == "add") {
      AST::AgentDeclaration *agent = sig.paramTypes[0].getAgentDecl();
      *this << "*DYN_ARRAY_PLACE(&agents.agents_" << agent->name
            << ", " << agent->name << ") = " << *(*expr.args)[0];
      return;
    } else if (sig.name == "save") {
      *this << "save(&agents, agents_info, " << *(*expr.args)[0] << ", SAVE_JSON)";
      return;
    }

    *this << sig.name << "(";
    printArgs(expr);
    *this << ")";
  }
}
void CLHPrinter::print(const AST::MemberInitEntry &entry) {
  *this << "." << entry.name << " = " << *entry.expr << ",";
}
void CLHPrinter::print(const AST::AgentCreationExpression &expr) {
  *this << "(" << expr.name << ") {" << indent
        << *expr.members;

  *this << outdent << nl << "}";
}
void CLHPrinter::print(const AST::NewArrayExpression &expr) {
  *this << "DYN_ARRAY_CREATE_FIXED(";
  printStorageType(*this, expr.elemType->resolved);
  *this << ", " << *expr.sizeExpr << ")";
}

void CLHPrinter::print(const AST::MemberAccessExpression &expr) {
  if (expr.expr->type.isAgent()) {
    *this << *expr.expr << "->" << expr.member;
  } else {
    GenericCPrinter::print(expr);
  }
}

void CLHPrinter::print(const AST::AssignStatement &expr) {
  if (expr.right->type.isAgent()) {
    // Agent assignments are interpreted as copies, not reference assignments
    *this << "" << *expr.left << " = *" << *expr.right << ";";
  } else {
      GenericCPrinter::print(expr);
  }
}
void CLHPrinter::print(const AST::VarDeclarationStatement &stmt) {
  Type type = stmt.type->resolved;
  if (typeRequiresStorage(type)) {
    // Type requires a separate variable for storage.
    // This makes access to it consistent lateron
    std::string sLabel = makeAnonLabel();
    printStorageType(*this, type);
    *this << " " << sLabel;
    if (stmt.initializer) {
      *this << " = ";
      if (stmt.initializer->type.isAgent()) {
        // Agent assignments are interpreted as copies, not reference assignments
        *this << "";
      }
      *this << *stmt.initializer;
    }
    *this << ";" << nl;
    *this << type << " " << *stmt.var << " = &" << sLabel << ";";
  } else {
      GenericCPrinter::print(stmt);
  }
}

//print initialisation
static void printRangeFor(CLHPrinter &p, const AST::ForStatement &stmt) {
  std::string eLabel = p.makeAnonLabel();
  auto range = stmt.getRange();
  p << "for (int " << *stmt.var << " = " << range.first
    << ", " << eLabel << " = " << range.second << "; "
    << *stmt.var << " < " << eLabel << "; ++" << *stmt.var << ") " << *stmt.stmt;
}

void CLHPrinter::print(const AST::ForStatement &stmt) {
  if (stmt.isRange()) {
    printRangeFor(*this, stmt);
    return;
  } else if (stmt.isNear()) {
    const AST::Expression &agentExpr = stmt.getNearAgent();
    const AST::Expression &radiusExpr = stmt.getNearRadius();

    AST::AgentDeclaration *agentDecl = stmt.type->resolved.getAgentDecl();
    AST::AgentMember *posMember = agentDecl->getPositionMember();
    const char *dist_fn = posMember->type->resolved == Type::VEC2
      ? "dist_float2" : "dist_float3";

    std::string eLabel = makeAnonLabel();
    std::string iLabel = makeAnonLabel();

    // For now: Print normal loop with radius check
    *this << "for (size_t " << iLabel << " = 0; "
          << iLabel << " < agents.agents_" << agentDecl->name << ".len; "
          << iLabel << "++) {"
          << indent << nl << *stmt.type << " " << *stmt.var
          << " = DYN_ARRAY_GET(&agents.agents_" << agentDecl->name << ", ";
    printStorageType(*this, stmt.type->resolved);
    *this << ", " << iLabel << ");" << nl
          << "if (" << dist_fn << "(" << *stmt.var << "->" << posMember->name << ", "
          << agentExpr << "->" << posMember->name << ") > " << radiusExpr
          << ") continue;" << nl
          << *stmt.stmt << outdent << nl << "}";
    return;
  }

  // Normal range-based for loop
  std::string eLabel = makeAnonLabel();
  std::string iLabel = makeAnonLabel();

  *this << stmt.expr->type << " " << eLabel << " = " << *stmt.expr << ";" << nl
        << "for (size_t " << iLabel << " = 0; "
        << iLabel << " < " << eLabel << "->len; "
        << iLabel << "++) {"
        << indent << nl << *stmt.type << " " << *stmt.var
        << " = DYN_ARRAY_GET(" << eLabel << ", ";
  printStorageType(*this, stmt.type->resolved);
  *this << ", " << iLabel << ");" << nl
        << *stmt.stmt << outdent << nl << "}";
}
void CLHPrinter::print(const AST::SimulateStatement &stmt) {
    *this << "cl_int ret;" << nl
    << "cl_device_id device_id = NULL;" << nl
    << "cl_uint num_of_platforms;" << nl
    << "cl_uint num_of_devices=0;" << nl
    << "clGetPlatformIDs(0, NULL, &num_of_platforms);" << nl
    << "cl_platform_id platform_ids[num_of_platforms];" << nl
    << "ret = clGetPlatformIDs( num_of_platforms, platform_ids, NULL );" << nl;
    int num_devices = stmt.num_devices;

    std::set <Type> agentTypes;
    for (AST::FunctionDeclaration *stepFunc : stmt.stepFuncDecls) {
        const AST::Param &param = *(*stepFunc->params)[0];
        Type type = param.type->resolved;
        agentTypes.insert(type);
    }

    std::string agentLabel = makeAnonLabel();

    *this << "cl_command_queue command_queues[" << num_devices << "];" << nl;

    for (auto type:agentTypes) {
        std::ostringstream s;
        s << "agents.agents_" << type;
        std::string bufName = s.str();
        s << "_dbuf";
        std::string dbufName = s.str();

        *this << "if (!" << dbufName << ".values) {" << indent
              << nl << dbufName << " = DYN_ARRAY_COPY_FIXED(" << type
              << ", &" << bufName << ");"
              << outdent << nl << "}" << nl;

        *this << "size_t " << agentLabel << "_"<< type <<" = sizeof(" << type << ")*" << bufName << ".len;" << nl
              << "size_t " << agentLabel << "_" << type << "_dbuf"  << " = sizeof(" << type << ")*" << bufName << ".len;" << nl;

        int stepFuncCount = 0;
        for (AST::FunctionDeclaration *stepFunc : stmt.stepFuncDecls) {
            const AST::Param &paramet = *(*stepFunc->params)[0];
            Type typeCurrent = paramet.type->resolved;
            if (typeCurrent != type) continue;

            *this << "cl_kernel kernel_" << type << "_" << stepFuncCount << "s[" << num_devices << "];" << nl;
            stepFuncCount++;
        }

        *this << "cl_kernel st_kernel_" << type << "s[" << num_devices << "];" << nl
              << "cl_kernel mem_kernel_" << type << "s[" << num_devices << "];" << nl;

        if (!generateForGraph) {
            *this << "cl_kernel upenv_kernel_" << type <<  "s[" << num_devices << "];" << nl;
        }

        *this << "cl_mem " << agentLabel << "MemObj_"  << type << "s[" << num_devices << "];" << nl
              << "cl_mem " << agentLabel << "MemObjDbuf_" << type << "s[" << num_devices << "];" << nl
              << "cl_mem " << agentLabel << "MemObjLen_" << type << "s[" << num_devices << "];" << nl
              << "cl_mem " << agentLabel << "MemObjEnv_" << type << "s[" << num_devices << "];" << nl;
    }

    std::vector<int> devices = stmt.activeDevices;
    std::sort(devices.begin(), devices.end());
    *this << "int activeDevice[" << num_devices << "]={";
    for ( auto i = devices.begin(); i != devices.end(); i++ ) {
       if (i!=devices.end()-1)
           *this << *i << ",";
       else
           *this << *i;
    }
    *this << "};" << nl;

    for (auto type:agentTypes) {
        if (useRNG) {
            *this << "cl_mem " << agentLabel << "MemObjRng_" << type << "s[" << num_devices << "];" << nl;
        }
        if (isConflictResolutionEnabled) {
            *this << "cl_mem " << agentLabel << "MemObjConflictFlag_" << type << "s[" << num_devices << "];" << nl
                  << "cl_kernel cr_kernel_" << type << "s[" << num_devices << "];" << nl;
            if (!generateForGraph) {
                *this << "cl_kernel cra_kernel_" << type << "s[" << num_devices << "];" << nl;
            }
        }
    }

    *this << nl << "char fileName[] = \"kernel.cl\";" << nl
          << "char *source_str;" << nl
          << "size_t source_size;" << nl
          << "FILE *fp;" << nl
          << "fp = fopen(fileName, \"r\");" << nl
          << "source_str = (char*)malloc(0x100000);" << nl
          << "source_size = fread(source_str, 1, 0x100000, fp);" << nl
          << "fclose(fp);" << nl;

    std::string deviceIter = "deviceIter";

    *this << "for (int " << deviceIter << " = 0 ; " << deviceIter << " < " << num_devices << " ; " << deviceIter << "++){" << indent << nl;
    *this << "int devId = " << "activeDevice[" <<  deviceIter << "];" << nl;
    *this << "ret = clGetDeviceIDs( platform_ids[devId], CL_DEVICE_TYPE_ALL, 1, &device_id, &ret );"
          << nl
          << "cl_context_properties contextProperties[] =" << nl
          << "{" << indent << nl
          << "CL_CONTEXT_PLATFORM," << nl
          << "(cl_context_properties) platform_ids[devId]," << nl
          << "0" << outdent << nl
          << "};" << nl
          << "cl_context context = clCreateContextFromType(contextProperties, CL_DEVICE_TYPE_ALL, NULL, NULL, &ret);"
          << nl
          << "cl_command_queue command_queue = clCreateCommandQueueWithProperties(context, device_id, 0, &ret);"
          << nl
          << "command_queues[" << deviceIter << "] = command_queue;"
          << nl;

    *this << nl
          << "cl_program program = clCreateProgramWithSource(context, 1, (const char **)&source_str, NULL, &ret);"
          << nl
          << "ret = clBuildProgram(program, 1, &device_id, NULL, NULL, NULL);" << nl
          << "char* kernel_name_prefix = \"compute_kernel_\";" << nl
          << "char number[2];" << nl
          << "char kernel_name[50];" << nl;

    for (auto type:agentTypes) {
        std::ostringstream s;
        s << "agents.agents_" << type;
        std::string bufName = s.str();
        s << "_dbuf";
        std::string dbufName = s.str();

        *this << "cl_mem " << agentLabel << "MemObj_" << type << " = clCreateBuffer(context, CL_MEM_READ_WRITE, "
              << agentLabel
              << "_" << type << ", NULL , &ret);" << nl
              << agentLabel << "MemObj_" << type << "s[" << deviceIter << "] = " << agentLabel << "MemObj_" << type
              << ";" << nl
              << "cl_mem " << agentLabel << "MemObjDbuf_" << type << " = clCreateBuffer(context, CL_MEM_READ_WRITE, "
              << agentLabel
              << "_" << type << "_dbuf, NULL , &ret);" << nl
              << agentLabel << "MemObjDbuf_" << type << "s[" << deviceIter << "] = " << agentLabel << "MemObjDbuf_"
              << type << ";" << nl
              << "cl_mem " << agentLabel
              << "MemObjLen_" << type << " = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(int), NULL , &ret);"
              << nl
              << agentLabel << "MemObjLen_" << type << "s[" << deviceIter << "] = " << agentLabel << "MemObjLen_"
              << type << ";" << nl;

        if (generateForGraph) {
            *this << "cl_mem " << agentLabel << "MemObjEnv_" << type
                  << " = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof("
                  << envType << ")*" << envGraphSize << ", NULL , &ret);" << nl
                  << agentLabel << "MemObjEnv_" << type << "s[" << deviceIter << "] = " << agentLabel << "MemObjEnv_"
                  << type << ";" << nl;
        } else {
            *this << "cl_mem " << agentLabel << "MemObjEnv_" << type
                  << " = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof("
                  << envType << ")*" << envSizeValue << ", NULL , &ret);" << nl
                  << agentLabel << "MemObjEnv_" << type << "s[" << deviceIter << "] = " << agentLabel << "MemObjEnv_"
                  << type << ";" << nl;
        }

        if (isConflictResolutionEnabled) {
            *this << "cl_mem " << agentLabel
                  << "MemObjConflictFlag_" << type
                  << " = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(bool), NULL , &ret);"
                  << nl
                  << agentLabel << "MemObjConflictFlag_" << type << "s[" << deviceIter << "] = " << agentLabel
                  << "MemObjConflictFlag_" << type << ";"
                  << nl;
            if (!generateForGraph) {
                *this << "cl_mem " << agentLabel
                      << "MemObjConflictSet_" << type << " = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(" << type
                      << "_conflict)*" << bufName << ".len, NULL , &ret);"
                      << nl;
            }
        }

        if (useRNG) {
            std::string iterLabel = makeAnonLabel();
            std::string varLabel = makeAnonLabel();

            *this << "cl_uint2 *rngState_" << type << ";" << nl
                  << "rngState_" << type << " = (cl_uint2 *)calloc(" << bufName << ".len, sizeof(cl_uint2));" << nl
                  << "for ( int " << iterLabel << " = 0 ; " << iterLabel << " < " << bufName << ".len ; " << iterLabel
                  << "++ ) {" << indent << nl
                  << "cl_uint2 " << varLabel << " = { (uint32_t)(" << iterLabel << " * 2), (uint32_t)(" << iterLabel
                  << " * 2 + 1) };" << nl
                  << "rngState_" << type << "[" << iterLabel << "] = " << varLabel << ";"
                  << outdent << nl << "}" << nl
                  << "cl_mem " << agentLabel
                  << "MemObjRng_" << type << " = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(cl_uint2)*"
                  << bufName
                  << ".len, NULL , &ret);" << nl
                  << agentLabel << "MemObjRng_" << type << "s[" << deviceIter << "] = " << agentLabel << "MemObjRng_"
                  << type << ";" << nl;
        }

    }

    for (auto type:agentTypes) {
        std::ostringstream s;
        s << "agents.agents_" << type;
        std::string bufName = s.str();
        s << "_dbuf";
        std::string dbufName = s.str();


        int stepFuncCount = 0;
        for (AST::FunctionDeclaration *stepFunc : stmt.stepFuncDecls) {
            const AST::Param &paramet = *(*stepFunc->params)[0];
            Type typeCurrent = paramet.type->resolved;
            if (typeCurrent != type) continue;

            *this << "sprintf(number,\"%d\", " << deviceIter << ");" << nl
                  << "sprintf(kernel_name,\"%s%s_%s_%s\", kernel_name_prefix, number, \"" << type << "\",\"" << stepFuncCount << "\");" << nl
                  << "cl_kernel kernel_" << type << "_" << stepFuncCount << " = clCreateKernel(program, kernel_name, &ret);" << nl
                  << "kernel_" << type << "_" << stepFuncCount <<  "s[" << deviceIter << "] = " << "kernel_" << type << "_"
                  << stepFuncCount << ";" << nl;
            stepFuncCount++;
        }

        *this << "cl_kernel st_kernel_" << type << " = clCreateKernel(program, \"sorting_" << type << "\", &ret);" << nl
              << "st_kernel_" << type << "s[" << deviceIter << "] = " << "st_kernel_" << type << ";" << nl
              << "cl_kernel mem_kernel_" << type << " = clCreateKernel(program, \"mem_update_" << type << "\", &ret);" << nl
              << "mem_kernel_" << type << "s[" << deviceIter << "] = " << "mem_kernel_" << type << ";" << nl;

        if (!generateForGraph) {
            *this << "cl_kernel upenv_kernel_" << type << " = clCreateKernel(program, \"update_envId_" << type << "\", &ret);" << nl
                  << "upenv_kernel_" << type << "s[" << deviceIter << "] = " << "upenv_kernel_" << type << ";" << nl;
        }
        if (isConflictResolutionEnabled) {
            *this << "cl_kernel cr_kernel_" << type << " = clCreateKernel(program, \"conflict_resolver_" << type << "\", &ret);" << nl
                  << "cr_kernel_" << type << "s[" << deviceIter << "] = " << "cr_kernel_" << type << ";" << nl;
            if (!generateForGraph) {
                *this << "cl_kernel cra_kernel_" << type << " = clCreateKernel(program, \"conflict_resolver_act_" << type << "\", &ret);" << nl
                      << "cra_kernel_" << type << "s[" << deviceIter << "] = " << "cra_kernel_" << type << ";" << nl;
            }
        }

        int tmpSizeStringCount = 0;

        stepFuncCount = 0;
        for (AST::FunctionDeclaration *stepFunc : stmt.stepFuncDecls) {
            const AST::Param &paramet = *(*stepFunc->params)[0];
            Type typeCurrent = paramet.type->resolved;
            if (typeCurrent != type) continue;
            tmpSizeStringCount = 0;
            for (auto typee:agentTypes) {
                *this << "ret = clSetKernelArg(kernel_" << type << "_" << stepFuncCount << ", " << tmpSizeStringCount << ", sizeof(cl_mem), &"
                      << agentLabel
                      << "MemObj_" << typee << ");" << nl
                      << "ret |= clSetKernelArg(kernel_" << type << "_" << stepFuncCount << ", " << tmpSizeStringCount + 1 << ", sizeof(cl_mem), &"
                      << agentLabel
                      << "MemObjDbuf_" << typee << ");" << nl
                      << "ret |= clSetKernelArg(kernel_" << type << "_" << stepFuncCount << ", " << tmpSizeStringCount + 2 << ", sizeof(cl_mem), &"
                      << agentLabel
                      << "MemObjLen_" << typee << ");" << nl
                      << "ret |= clSetKernelArg(kernel_" << type << "_" << stepFuncCount << ", " << tmpSizeStringCount + 3 << ", sizeof(cl_mem), &"
                      << agentLabel
                      << "MemObjEnv_" << typee << ");" << nl;

                if (useRNG) {
                    *this << "ret |= clSetKernelArg(kernel_" << type << "_" << stepFuncCount << ", " << tmpSizeStringCount + 4
                          << ", sizeof(cl_mem), &" << agentLabel
                          << "MemObjRng_" << type << ");" << nl;
                }
                tmpSizeStringCount = tmpSizeStringCount + 5;
            }
            stepFuncCount++;
        }


        tmpSizeStringCount =0;

        if (!generateForGraph) {
            *this << "ret = clSetKernelArg(upenv_kernel_" << type << ", " << tmpSizeStringCount << ", sizeof(cl_mem), &" << agentLabel
                  << "MemObj_" << type << ");" << nl
                  << "ret |= clSetKernelArg(upenv_kernel_" << type << ", " << tmpSizeStringCount + 1 << ", sizeof(cl_mem), &"
                  << agentLabel
                  << "MemObjLen_" << type << ");" << nl;
        }

        *this << "ret = clSetKernelArg(st_kernel_" << type << ", " << tmpSizeStringCount << ", sizeof(cl_mem), &" << agentLabel
              << "MemObj_" << type << ");" << nl
              << "ret |= clSetKernelArg(st_kernel_" << type << ", " << tmpSizeStringCount + 1 << ", sizeof(cl_mem), &" << agentLabel
              << "MemObjDbuf_" << type << ");" << nl
              << "ret |= clSetKernelArg(st_kernel_" << type << ", " << tmpSizeStringCount + 2 << ", sizeof(cl_mem), &" << agentLabel
              << "MemObjLen_" << type << ");" << nl
              << "ret = clSetKernelArg(mem_kernel_" << type << ", " << tmpSizeStringCount << ", sizeof(cl_mem), &" << agentLabel
              << "MemObj_" << type << ");" << nl
              << "ret |= clSetKernelArg(mem_kernel_" << type << ", " << tmpSizeStringCount + 1 << ", sizeof(cl_mem), &" << agentLabel
              << "MemObjDbuf_" << type << ");" << nl
              << "ret |= clSetKernelArg(mem_kernel_" << type << ", " << tmpSizeStringCount + 2 << ", sizeof(cl_mem), &" << agentLabel
              << "MemObjLen_" << type << ");" << nl
              << "ret |= clSetKernelArg(mem_kernel_" << type << ", " << tmpSizeStringCount + 3 << ", sizeof(cl_mem), &" << agentLabel
              << "MemObjEnv_" << type << ");" << nl;


        if (isConflictResolutionEnabled) {
            *this << "ret |= clSetKernelArg(mem_kernel_" << type << ", " << tmpSizeStringCount + 4 << ", sizeof(cl_mem), &"
                  << agentLabel
                  << "MemObjConflictFlag_" << type << ");" << nl
                  << "ret = clSetKernelArg(cr_kernel_" << type << ", " << tmpSizeStringCount << ", sizeof(cl_mem), &"
                  << agentLabel
                  << "MemObj_" << type << ");" << nl
                  << "ret |= clSetKernelArg(cr_kernel_" << type << ", " << tmpSizeStringCount + 1 << ", sizeof(cl_mem), &"
                  << agentLabel
                  << "MemObjDbuf_" << type << ");" << nl
                  << "ret |= clSetKernelArg(cr_kernel_" << type << ", " << tmpSizeStringCount + 2 << ", sizeof(cl_mem), &"
                  << agentLabel
                  << "MemObjLen_" << type << ");" << nl
                  << "ret |= clSetKernelArg(cr_kernel_" << type << ", " << tmpSizeStringCount + 3 << ", sizeof(cl_mem), &"
                  << agentLabel
                  << "MemObjEnv_" << type << ");" << nl
                  << "ret |= clSetKernelArg(cr_kernel_" << type << ", " << tmpSizeStringCount + 4 << ", sizeof(cl_mem), &"
                  << agentLabel
                  << "MemObjConflictFlag_" << type << ");" << nl;

            if (!generateForGraph) {
                *this << "ret |= clSetKernelArg(mem_kernel_" << type << ", " << tmpSizeStringCount + 5 << ", sizeof(cl_mem), &"
                      << agentLabel
                      << "MemObjConflictSet_" << type << ");" << nl
                      << "ret |= clSetKernelArg(cr_kernel_" << type << ", " << tmpSizeStringCount + 5 << ", sizeof(cl_mem), &"
                      << agentLabel
                      << "MemObjConflictSet_" << type << ");" << nl
                      << "ret = clSetKernelArg(cra_kernel_" << type << ", " << tmpSizeStringCount << ", sizeof(cl_mem), &"
                      << agentLabel
                      << "MemObj_" << type << ");" << nl
                      << "ret |= clSetKernelArg(cra_kernel_" << type << ", " << tmpSizeStringCount + 1 << ", sizeof(cl_mem), &"
                      << agentLabel
                      << "MemObjDbuf_" << type << ");" << nl
                      << "ret |= clSetKernelArg(cra_kernel_" << type << ", " << tmpSizeStringCount + 2 << ", sizeof(cl_mem), &"
                      << agentLabel
                      << "MemObjLen_" << type << ");" << nl
                      << "ret |= clSetKernelArg(cra_kernel_" << type << ", " << tmpSizeStringCount + 3 << ", sizeof(cl_mem), &"
                      << agentLabel
                      << "MemObjEnv_" << type << ");" << nl
                      << "ret |= clSetKernelArg(cra_kernel_" << type << ", " << tmpSizeStringCount + 4 << ", sizeof(cl_mem), &"
                      << agentLabel
                      << "MemObjConflictFlag_" << type << ");" << nl
                      << "ret |= clSetKernelArg(cra_kernel_" << type << ", " << tmpSizeStringCount + 5 << ", sizeof(cl_mem), &"
                      << agentLabel
                      << "MemObjRng_" << type << ");" << nl
                      << "ret |= clSetKernelArg(cra_kernel_" << type << ", " << tmpSizeStringCount + 6 << ", sizeof(cl_mem), &"
                      << agentLabel
                      << "MemObjConflictSet_" << type << ");" << nl;
            }
        }

        *this << nl
              << "ret = clEnqueueWriteBuffer(command_queue, " << agentLabel << "MemObj_" << type << ", CL_TRUE, 0, " << agentLabel
              << "_" << type << ", "
              << bufName << ".values, 0, NULL, NULL);" << nl
              << "ret = clEnqueueWriteBuffer(command_queue, " << agentLabel << "MemObjDbuf_" << type << ", CL_TRUE, 0, " << agentLabel
              << "_" << type << "_dbuf, "
              << dbufName << ".values, 0, NULL, NULL);" << nl
              << "ret = clEnqueueWriteBuffer(command_queue, " << agentLabel << "MemObjLen_" << type << ", CL_TRUE, 0, sizeof(int), &"
              << bufName << ".len, 0, NULL, NULL);" << nl;
        if (generateForGraph) {
            *this << "ret = clEnqueueWriteBuffer(command_queue, " << agentLabel << "MemObjEnv_" << type << ", CL_TRUE, 0, sizeof("
                  << envType << ")*"
                  << envGraphSize << ", &" << envName << ", 0, NULL, NULL);" << nl;
        }

        if (useRNG) {
            *this << "ret = clEnqueueWriteBuffer(command_queue, " << agentLabel
                  << "MemObjRng_" << type << ", CL_TRUE, 0, sizeof(cl_uint2)*"
                  << bufName << ".len, rngState_" << type << ", 0, NULL, NULL);" << nl;
        }
    }
    *this << outdent << nl << "}" <<nl;

    *this << "size_t localWorkSize = 128;" << nl;
    std::string outTmpLabel = makeAnonLabel();

    for (auto type:agentTypes) {
        std::ostringstream s;
        s << "agents.agents_" << type;
        std::string bufName = s.str();

        if (num_devices > 1) {
            for (int i = 0; i < num_devices; i++) {
                *this << type << " " << outTmpLabel << "buff_"<< type << i << " = calloc(sizeof(" << type << "), " << bufName
                      << ".len);" << nl;
            }
            *this << type << " " << outTmpLabel << "buffMerge_" << type << " = calloc(sizeof(" << type << "), " << bufName
                  << ".len);" << nl;
            if (generateForGraph)
                *this << envType << " *" << outTmpLabel << "Env_"<< type << " = calloc(sizeof(" << envType << ")," << envGraphSize << ");"
                      << nl;
            else
                *this << envType << " *" << outTmpLabel << "Env_" << type << " = calloc(sizeof(" << envType << ")," << envSizeValue << ");"
                      << nl;
        }

        *this << "size_t globalWorkSize_" << type << " = roundWorkSizeUp(128, " << bufName << ".len);" << nl;

        if (isConflictResolutionEnabled) {
            if (generateForGraph)
                *this << "size_t cr_globalWorkSize_" << type << " = " << envGraphSize << ";" << nl;
            else
                *this << "size_t cr_globalWorkSize_" << type << " = globalWorkSize_" << type << ";" << nl;
        }

        if (!generateForGraph) {
            *this << "ret = clEnqueueNDRangeKernel(command_queues[0], upenv_kernel_" << type
                  << "s[0], 1, NULL, &globalWorkSize_" << type << ", &localWorkSize, 0, NULL, NULL);"
                  << nl;
        }

        *this << "for (int length = 1; length < globalWorkSize_" << type << "; length <<= 1)" << nl
              << "for (int inc = length; inc > 0; inc >>= 1) {" << indent << nl
              << "int dir = length << 1;" << nl
              << "clSetKernelArg(st_kernel_" << type << "s[0], 3, sizeof(int), &inc);" << nl
              << "clSetKernelArg(st_kernel_" << type << "s[0], 4, sizeof(int), &dir);" << nl
              << "clEnqueueNDRangeKernel(command_queues[0], st_kernel_" << type << "s[0], 1, NULL, &globalWorkSize_"
              << type << ", NULL, 0, NULL, NULL);" << nl
              << "clFinish(command_queues[0]);"
              << outdent << nl
              << "}" << nl
              << "ret |= clEnqueueNDRangeKernel(command_queues[0], mem_kernel_" << type
              << "s[0], 1, NULL, &globalWorkSize_" << type << ", &localWorkSize, 0, NULL, NULL);"
              << nl
              << "clFinish(command_queues[0]);" << nl;

        if (num_devices > 1) {
            *this << "clEnqueueReadBuffer(command_queues[0], " << agentLabel << "MemObj_" << type << "s[0], CL_TRUE, 0, "
                  << agentLabel << "_" << type << ", " << outTmpLabel << "buff_"
                  << type << "0" << ", 0, NULL, NULL);" << nl;
            if (generateForGraph)
                *this << "clEnqueueReadBuffer(command_queues[0], " << agentLabel << "MemObjEnv_" << type << "s[0], CL_TRUE, 0, "
                      << "sizeof(" << envType << ")*" << envGraphSize << ", " << outTmpLabel << "Env_"  << type
                      << ", 0, NULL, NULL);" << nl;
            else
                *this << "clEnqueueReadBuffer(command_queues[0], " << agentLabel << "MemObjEnv_" << type << "s[0], CL_TRUE, 0, "
                      << "sizeof(" << envType << ")*" << envSizeValue << ", " << outTmpLabel << "Env_"  << type
                      << ", 0, NULL, NULL);" << nl;

            for (int i = 1; i < num_devices; i++) {
                *this << "clEnqueueWriteBuffer(command_queues[" << i << "], " << agentLabel << "MemObj_" << type << "s[" << i
                      << "], CL_TRUE, 0, " << agentLabel << "_" << type << ", " << outTmpLabel << "buff_" << type << "0"
                      << ", 0, NULL, NULL);" << nl;
                if (generateForGraph)
                    *this << "clEnqueueWriteBuffer(command_queues[" << i << "], " << agentLabel << "MemObjEnv_" << type << "s[" << i
                          << "], CL_TRUE, 0, " << "sizeof(" << envType << ")*" << envGraphSize << ", " << outTmpLabel
                          << "Env_" << type << ", 0, NULL, NULL);" << nl;
                else
                    *this << "clEnqueueWriteBuffer(command_queues[" << i << "], " << agentLabel << "MemObjEnv_" << type << "s[" << i
                          << "], CL_TRUE, 0, " << "sizeof(" << envType << ")*" << envSizeValue << ", " << outTmpLabel
                          << "Env_" << type << ", 0, NULL, NULL);" << nl;
            }
        }
    }

    std::string tLabel = makeAnonLabel();
    *this << "for (int " << tLabel << " = 0; "
          << tLabel << " < " << *stmt.timestepsExpr << "; "
          << tLabel << "++) {" << indent << nl;

    if (isConflictResolutionEnabled)
        *this << "bool conflictFlag = false;" << nl;

    if (num_devices == 1) {
        int stepFuncCount[agentTypes.size()]= { 0 };
        for (AST::FunctionDeclaration *stepFunc : stmt.stepFuncDecls) {
            int cc = 0;
            for (auto type:agentTypes) {
                const AST::Param &paramet = *(*stepFunc->params)[0];
                Type typeCurrent = paramet.type->resolved;
                if (typeCurrent != type) {cc++; continue;}

                *this << "ret = clEnqueueNDRangeKernel(command_queues[0], kernel_" << type << "_" << stepFuncCount[cc]
                      << "s[0], 1, NULL, &globalWorkSize_" << type << ", &localWorkSize, 0, NULL, NULL);"
                      << nl;
                stepFuncCount[cc]++;
            }
        }
    } else {
        for (int i = 0 ; i < num_devices ; i++) {
            int stepFuncCount[agentTypes.size()] = {0};
            for (AST::FunctionDeclaration *stepFun : stmt.stepFuncDeclOnDevice[i]) {
                int cc = 0;
                for (auto type:agentTypes) {
                    const AST::Param &paramet = *(*stepFun->params)[0];
                    Type typeCurrent = paramet.type->resolved;
                    if (typeCurrent != type) {
                        cc++;
                        continue;
                    }

                    *this << "ret = clEnqueueNDRangeKernel(command_queues[" << i << "], kernel_" << type << "_"
                          << stepFuncCount[cc]
                          << "s[" << i << "], 1, NULL, &globalWorkSize_" << type << ", &localWorkSize, 0, NULL, NULL);"
                          << nl;
                    stepFuncCount[cc]++;
                }
            }
        }
    }

    for (int i = num_devices - 1; i >= 0; i--) {
        *this << "clFinish(command_queues[" << i << "]);" << nl;
    }

    for (auto type:agentTypes) {
        std::ostringstream s;
        s << "agents.agents_" << type;
        std::string bufName = s.str();

        if (num_devices > 1) {
            for (int i = num_devices - 1; i >= 0; i--) {
                *this << "clEnqueueReadBuffer(command_queues[" << i << "], " << agentLabel << "MemObj_" << type << "s[" << i
                      << "], CL_TRUE, 0, " << agentLabel << "_" << type << ", " << outTmpLabel << "buff_" << type << i << ", 0, NULL, NULL);"
                      << nl;
            }
            *this << "#pragma omp for schedule(static, 128)" << nl;

            std::string iLabel = makeAnonLabel();
            *this << "for (int " << iLabel << " = 0; "
                  << iLabel << " < " << bufName << ".len; "
                  << iLabel << "++) {" << indent << nl
                  << type << " *_agent = " << "coexecution_merge_" << type << "(";

            for (int i = 0; i < num_devices; i++) {
                if (i != num_devices - 1)
                    *this << "&" << outTmpLabel << "buff_" << type << i << "[" << iLabel << "], ";
                else
                    *this << "&" << outTmpLabel << "buff_" << type << i << "[" << iLabel << "]";
            }

            *this << ");" << nl
                  << outTmpLabel << "buffMerge_" << type << "[" << iLabel << "] = *_agent;"
                  << outdent << nl << "}" << nl;

            *this << "clEnqueueWriteBuffer(command_queues[0], " << agentLabel << "MemObj_" << type << "s[0], CL_TRUE, 0, "
                  << agentLabel << "_" << type << ", " << outTmpLabel << "buffMerge_" << type << ", 0, NULL, NULL);"
                  << nl;
        }
    }
    if (isConflictResolutionEnabled) {
        *this << "do {" << indent << nl;
        for (auto type:agentTypes) {
            *this << "bool conflictFlag_" << type << " = false;" << nl;
        }
    }
    for (auto type:agentTypes) {
        if (!generateForGraph) {
            *this  << "ret = clEnqueueNDRangeKernel(command_queues[0], upenv_kernel_" << type << "s[0], 1, NULL, &globalWorkSize_" << type << ", &localWorkSize, 0, NULL, NULL);"
                   << nl;
        }
        *this << "for (int length = 1; length < globalWorkSize_" << type << "; length <<= 1)" << nl
              << "for (int inc = length; inc > 0; inc >>= 1) {" << indent << nl
              << "int dir = length << 1;" << nl
              << "clSetKernelArg(st_kernel_" << type << "s[0], 3, sizeof(int), &inc);" << nl
              << "clSetKernelArg(st_kernel_" << type << "s[0], 4, sizeof(int), &dir);" << nl
              << "clEnqueueNDRangeKernel(command_queues[0], st_kernel_" << type << "s[0], 1, NULL, &globalWorkSize_" << type << ", NULL, 0, NULL, NULL);"
              << nl
              << "clFinish(command_queues[0]);"
              << outdent << nl
              << "}" << nl
              << "ret |= clEnqueueNDRangeKernel(command_queues[0], mem_kernel_" << type << "s[0], 1, NULL, &globalWorkSize_" << type << ", &localWorkSize, 0, NULL, NULL);"
              << nl
              << "clFinish(command_queues[0]);" << nl;

        if (isConflictResolutionEnabled) {
            *this << "ret |= clEnqueueNDRangeKernel(command_queues[0], cr_kernel_" << type << "s[0], 1, NULL, &cr_globalWorkSize_" << type << ", NULL, 0, NULL, NULL);" << nl
                  << "clFinish(command_queues[0]);" << nl;
            if (!generateForGraph) {
                *this << "ret |= clEnqueueNDRangeKernel(command_queues[0], cra_kernel_" << type << "s[0], 1, NULL, &cr_globalWorkSize_" << type << ", NULL, 0, NULL, NULL);" << nl
                      << "clFinish(command_queues[0]);" << nl;
            }
            *this << "clEnqueueReadBuffer(command_queues[0], " << agentLabel << "MemObjConflictFlag_" << type << "s[0], CL_TRUE, 0, sizeof(bool), &conflictFlag_" << type << ", 0, NULL, NULL);" << nl
                  << "clFinish(command_queues[0]);"
                  << outdent << nl;
        }
    }

    if (isConflictResolutionEnabled) {
        *this << "conflictFlag = ";
        int cc = 0;
        for (auto type:agentTypes) {
            if (cc == 0)
                *this << "conflictFlag_" << type;
            else
                *this << " && conflictFlag_" << type;
            cc++;
        }
        *this << ";" << nl;
        *this << "} while (conflictFlag);" << nl;
    }

    for (auto type:agentTypes) {
        if (num_devices > 1) {
            *this << "clEnqueueReadBuffer(command_queues[0], " << agentLabel << "MemObj_" << type << "s[0], CL_TRUE, 0, "
                  << agentLabel << "_" << type << ", " << outTmpLabel << "buff_" << type << "0" << ", 0, NULL, NULL);" << nl;
            if (generateForGraph)
                *this << "clEnqueueReadBuffer(command_queues[0], " << agentLabel << "MemObjEnv_" << type << "s[0], CL_TRUE, 0, "
                      << "sizeof(" << envType << ")*" << envGraphSize << ", " << outTmpLabel << "Env_" << type
                      << ", 0, NULL, NULL);" << nl;
            else
                *this << "clEnqueueReadBuffer(command_queues[0], " << agentLabel << "MemObjEnv_" << type << "s[0], CL_TRUE, 0, "
                      << "sizeof(" << envType << ")*" << envSizeValue << ", " << outTmpLabel << "Env_" << type
                      << ", 0, NULL, NULL);" << nl;

            for (int i = 1; i < num_devices; i++) {
                *this << "clEnqueueWriteBuffer(command_queues[" << i << "], " << agentLabel << "MemObj_" << type << "s[" << i
                      << "], CL_TRUE, 0, " << agentLabel << "_" << type << ", " << outTmpLabel << "buff_" << type
                      << "0" << ", 0, NULL, NULL);" << nl;
                if (generateForGraph)
                    *this << "clEnqueueWriteBuffer(command_queues[" << i << "], " << agentLabel << "MemObjEnv_" << type << "s[" << i
                          << "], CL_TRUE, 0, " << "sizeof(" << envType << ")*" << envGraphSize << ", " << outTmpLabel
                          << "Env_" << type << ", 0, NULL, NULL);" << nl;
                else
                    *this << "clEnqueueWriteBuffer(command_queues[" << i << "], " << agentLabel << "MemObjEnv_" << type << "s[" << i
                          << "], CL_TRUE, 0, " << "sizeof(env)*" << envSizeValue << ", " << outTmpLabel
                          << "Env_" << type << ", 0, NULL, NULL);" << nl;
            }
        }
    }
    *this << outdent << nl << "}" << nl;

    for (auto type:agentTypes) {
        std::ostringstream s;
        s << "agents.agents_" << type;
        std::string bufName = s.str();

        *this << "ret = clEnqueueReadBuffer(command_queues[0], " << agentLabel << "MemObj_" << type << "s[0], CL_TRUE, 0, "
              << agentLabel << "_" << type << ", " << bufName << ".values" << ", 0, NULL, NULL);" << nl;
    }
  // TODO Cleanup memory
}

void CLHPrinter::print(const AST::AgentMember &member) {
  if (!member.isArray)
      *this << *member.type << " " << member.name << ";";
  else
      //TODO: improve this
      *this << *member.type << " " << member.name << ";";
}

static void printTypeIdentifier(CLHPrinter &p, Type type) {
  switch (type.getTypeId()) {
    case Type::BOOL: p << "TYPE_BOOL"; break;
    case Type::INT32: p << "TYPE_INT"; break;
    case Type::FLOAT: p << "TYPE_FLOAT"; break;
    case Type::STRING: p << "TYPE_STRING"; break;
    case Type::VEC2: p << "TYPE_FLOAT2"; break;
    case Type::VEC3: p << "TYPE_FLOAT3"; break;
    case Type::ARRAY: p << "TYPE_ARRAY"; break;
    default: assert(0);
  }
}
void CLHPrinter::print(const AST::AgentDeclaration &decl) {
  *this << "typedef struct {" << indent
        << *decl.members << outdent << nl;

  if (decl.isRealAgent) {
      if (!generateForGraph) {
          *this << indent << nl << "int envId;" << outdent << nl;
      }
      *this << "}" << decl.name << ";" << nl;
      if (isConflictResolutionEnabled && !generateForGraph){
        *this << "typedef struct {" << indent << nl
        << "int conflictSet[1024];" << nl
        << "int conflictSetPointer;" << nl
        << "int conflictSetSmallest;" << outdent << nl
        << "}" << decl.name << "_conflict;" << nl;
      }
  }
  else {
        *this << indent << nl
        << "int mem_start;" << nl
        << "int mem_end;"
        << outdent << nl
        << "} " << decl.name  << " ;" << nl;
  }
  // Runtime type information
  *this << "static const type_info " << decl.name << "_info[] = {" << indent << nl;
  for (AST::AgentMemberPtr &member : *decl.members) {
    *this << "{ ";
    printTypeIdentifier(*this, member->type->resolved);
    *this << ", offsetof(" << decl.name << ", " << member->name
          << "), \"" << member->name << "\", "
          << (member->isPosition ? "true" : "false") << " }," << nl;
  }
  *this << "{ TYPE_END, sizeof(" << decl.name << "), NULL }" << outdent << nl << "};" << nl;
}

void CLHPrinter::print(const AST::EnvironmentDeclaration &) {
    // Often not used explicitly
//    assert(0);
    if (!generateForGraph) {
        *this << "typedef struct { " << indent << nl
              << "int mem_start;" << nl
              << "int mem_end;" << outdent << nl
              << "} env;" << nl;

        envType = "env";
        envName = "envAarry";
    }  else {
        *this << envType << " " << envName << "[" << envGraphSize << "]" << ";";
    }
}

void CLHPrinter::print(const AST::FunctionDeclaration &decl) {
    if (decl.isMain()) {
        // Return result code from main()
        *this << "int main() {" << indent <<nl
        << "double wtime = omp_get_wtime();"
        << *decl.stmts << nl
        << "wtime = omp_get_wtime() - wtime;" << nl
        << "printf(\"Time elapsed is %f seconds\\n\", wtime);" << nl
        << "return 0;" << outdent << nl << "}";
        return;
    }
    if (decl.isAnyStep())
        return;;
    GenericCPrinter::print(decl);
}

void CLHPrinter::print(const AST::Script &script) {
    envName = script.envDecl->envName;
    envType = script.envDecl->envType;
    envGraphSize = script.envDecl->envGraphSize;

    if (!envGraphSize.isInt()) {
        double radius = script.envDecl->envGranularity.getFloat();
        if (script.envDecl->envSize.isVec2()) {
            envSizeValue = ((int) (script.envDecl->envSize.getVec2().x / radius) + 1) *
                           ((int) (script.envDecl->envSize.getVec2().y / radius) + 1);
        } else if (script.envDecl->envSize.isVec3()) {
            envSizeValue = ((int) (script.envDecl->envSize.getVec3().x / radius) + 1) *
                           ((int) (script.envDecl->envSize.getVec3().y / radius) + 1) *
                           ((int) (script.envDecl->envSize.getVec3().z / radius) + 1);
        }
        generateForGraph = false;
    } else {
        generateForGraph = true;
    };

    useRNG = ((!generateForGraph) && isConflictResolutionEnabled) || script.usesRng;

    if (!envGraphSize.isInt())
        *this << "#include \"libabl.h\"" << nl ;
    else
        *this << "#include \"libabl_int.h\"" << nl ;

    *this << "#include \"time.h\"" << nl ;
    *this << "#include \"stdio.h\"" << nl ;
    *this << "#include \"CL/cl.h\"" << nl << nl;
    *this << "#include \"omp.h\"" << nl << nl;

    // First declare all agents
    for (AST::AgentDeclaration *decl : script.agents) {
        *this << *decl << nl;
    }

    *this << *script.envDecl << nl;


    // Create structure to store agents
    *this << "struct agent_struct {" << indent;
    for (AST::AgentDeclaration *decl : script.agents) {
        if (!decl->isRealAgent) continue;
        *this << nl << "dyn_array agents_" << decl->name << ";";
        *this << nl << "dyn_array agents_" << decl->name << "_dbuf;";
    }
    *this << outdent << nl << "};" << nl
          << "struct agent_struct agents;" << nl;

    // Create runtime type information for this structure
    *this << "static const agent_info agents_info[] = {" << indent << nl;
    for (AST::AgentDeclaration *decl : script.agents) {
    if (!decl->isRealAgent) continue;
        *this << "{ " << decl->name << "_info, "
              << "offsetof(struct agent_struct, agents_" << decl->name
              << "), \"" << decl->name << "\" }," << nl;
    }
    *this << "{ NULL, 0, NULL }" << outdent << nl << "};" << nl << nl;

    // Then declare everything else
    for (AST::ConstDeclaration *decl : script.consts) {
        *this << *decl << nl;
    }
    for (AST::FunctionDeclaration *decl : script.funcs) {
        *this << *decl << nl;
    }
}

}
