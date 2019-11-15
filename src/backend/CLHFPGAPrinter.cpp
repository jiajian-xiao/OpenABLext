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
#include "CLHFPGAPrinter.hpp"

namespace OpenABL {

void CLHFPGAPrinter::printType(Type type) {
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

static CLHFPGAPrinter &operator<<(CLHFPGAPrinter &s, Type type) {
  s.printType(type);
  return s;
}

static void printStorageType(CLHFPGAPrinter &s, Type type) {
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

static void printTypeCtor(CLHFPGAPrinter &p, const AST::CallExpression &expr) {
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
void CLHFPGAPrinter::print(const AST::CallExpression &expr) {
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
void CLHFPGAPrinter::print(const AST::MemberInitEntry &entry) {
  *this << "." << entry.name << " = " << *entry.expr << ",";
}
void CLHFPGAPrinter::print(const AST::AgentCreationExpression &expr) {
  *this << "(" << expr.name << ") {" << indent
        << *expr.members;

  *this << outdent << nl << "}";
}
void CLHFPGAPrinter::print(const AST::NewArrayExpression &expr) {
  *this << "DYN_ARRAY_CREATE_FIXED(";
  printStorageType(*this, expr.elemType->resolved);
  *this << ", " << *expr.sizeExpr << ")";
}

void CLHFPGAPrinter::print(const AST::MemberAccessExpression &expr) {
  if (expr.expr->type.isAgent()) {
    *this << *expr.expr << "->" << expr.member;
  } else {
    GenericCPrinter::print(expr);
  }
}

void CLHFPGAPrinter::print(const AST::AssignStatement &expr) {
  if (expr.right->type.isAgent()) {
    // Agent assignments are interpreted as copies, not reference assignments
    *this << "" << *expr.left << " = *" << *expr.right << ";";
  } else {
      GenericCPrinter::print(expr);
  }
}
void CLHFPGAPrinter::print(const AST::VarDeclarationStatement &stmt) {
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
static void printRangeFor(CLHFPGAPrinter &p, const AST::ForStatement &stmt) {
  std::string eLabel = p.makeAnonLabel();
  auto range = stmt.getRange();
  p << "for (int " << *stmt.var << " = " << range.first
    << ", " << eLabel << " = " << range.second << "; "
    << *stmt.var << " < " << eLabel << "; ++" << *stmt.var << ") " << *stmt.stmt;
}

void CLHFPGAPrinter::print(const AST::ForStatement &stmt) {
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
void CLHFPGAPrinter::print(const AST::SimulateStatement &stmt) {
    std::set <Type> agentTypes;
    for (AST::FunctionDeclaration *stepFunc : stmt.stepFuncDecls) {
        const AST::Param &param = *(*stepFunc->params)[0];
        Type type = param.type->resolved;
        agentTypes.insert(type);
    }

    std::string agentLabel = makeAnonLabel();

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

        *this << "size_t " << agentLabel << "_" << type << " = sizeof(" << type << ")*" << bufName << ".len;" << nl
              << "size_t " << agentLabel << "_" << type << "_dbuf = sizeof(" << type << ")*" << bufName << ".len;" << nl;
        if (!generateForGraph) {
            *this << "env *env_" << type << "s = (env *)calloc(" << envSizeValue << ", sizeof(env));" << nl
                  << "for (int i=0 ; i < " << envSizeValue << "; i++) {" << indent << nl
                  << "env_" << type << "s[i].mem_start = 0;" << nl
                  << "env_" << type << "s[i].mem_end = 0;"
                  << outdent << nl << "}" << nl;
        } else {
            *this << "for (int i=0 ; i <" << envGraphSize << "; i++) {" << indent << nl
                  << envName << "[i].mem_start = 0;" << nl
                  << envName << "[i].mem_end = 0;"
                  << outdent << nl << "}" << nl;
        }
    }


    *this << "auto devices = xcl::get_xil_devices();" << nl
          << "auto device = devices[0];" << nl
          << "cl_int err;" << nl
          << "OCL_CHECK(err, cl::Context context(device, NULL, NULL, NULL, &err));" << nl
          << "OCL_CHECK(err,cl::CommandQueue q(context, device, 0, &err));" << nl
          << "OCL_CHECK(err,std::string device_name = device.getInfo<CL_DEVICE_NAME>(&err));" << nl
          << "std::string binaryFile = xcl::find_binary_file(device_name, \"kernel\");" << nl
          << "cl::Program::Binaries bins = xcl::import_binary_file(binaryFile);" << nl
          << "devices.resize(1);" << nl
          << "OCL_CHECK(err, cl::Program program(context, devices, bins, NULL, &err));" << nl;

    for (auto type:agentTypes) {
        std::ostringstream s;
        s << "agents.agents_" << type;
        std::string bufName = s.str();
        s << "_dbuf";
        std::string dbufName = s.str();

        *this << "OCL_CHECK(err, cl::Buffer buffer_buff_" << type << "(context, CL_MEM_USE_HOST_PTR | CL_MEM_READ_WRITE, "
              << agentLabel << "_" << type << ", " << bufName << ".values, &err));" << nl
              << "OCL_CHECK(err, cl::Buffer buffer_dbuff_" << type << "(context, CL_MEM_USE_HOST_PTR | CL_MEM_READ_WRITE, "
              << agentLabel << "_" << type << "_dbuf, " << dbufName << ".values, &err));" << nl;
        if (!generateForGraph) {
            *this << "OCL_CHECK(err, cl::Buffer envo_" << type << "(context, CL_MEM_USE_HOST_PTR | CL_MEM_READ_WRITE, "
                  << "sizeof(env)*" << envSizeValue << ", env_" << type << "s, &err));" << nl;
        }
    }

    if (generateForGraph)
        *this << "OCL_CHECK(err, cl::Buffer buffer_env(context, CL_MEM_USE_HOST_PTR | CL_MEM_READ_WRITE, sizeof("
              << envType << ")*" << envGraphSize << ", &" << envName << ", &err));" << nl;


    *this << "OCL_CHECK(err, cl::Kernel krnl(program, \"compute_kernel_0\", &err));" << nl
          << "int narg = 0;" << nl;

    for (auto type:agentTypes) {
        *this << "OCL_CHECK(err, err = krnl.setArg(narg++, buffer_buff_" << type << "));" << nl
              << "OCL_CHECK(err, err = krnl.setArg(narg++, buffer_dbuff_" << type << "));"
              << nl;
        if (!generateForGraph){
            *this << "OCL_CHECK(err, err = krnl.setArg(narg++, envo_" << type << "));" << nl;
        }
    }

    if (generateForGraph)
        *this << "OCL_CHECK(err, err = krnl.setArg(narg++, buffer_env));" << nl;

    for (auto type:agentTypes) {
        std::ostringstream s;
        s << "agents.agents_" << type;
        std::string bufName = s.str();
        *this << "int num_agents_" << type << " = "<< bufName << ".len;" << nl
              << "OCL_CHECK(err, err = krnl.setArg(narg++, num_agents_" << type << "));" << nl;
    }
    *this << "OCL_CHECK(err, err = krnl.setArg(narg++, " << *stmt.timestepsExpr << "));" << nl;

    if (generateForGraph) {
        *this << "OCL_CHECK(err, err = q.enqueueMigrateMemObjects({";
        for (auto type:agentTypes) {
            *this << "buffer_buff_" << type << ", buffer_dbuff_" << type << ",";
        }
        *this << "buffer_env}, 0));" << nl;
    } else {
        *this << "OCL_CHECK(err, err = q.enqueueMigrateMemObjects({";
        int count = 0;
        for (auto type:agentTypes) {
            if (count < agentTypes.size()-1)
                *this << "buffer_buff_" << type << ", buffer_dbuff_" << type << ", envo_" << type << ",";
            else
                *this << "buffer_buff_" << type << ", buffer_dbuff_" << type<< ", envo_" << type ;
            count++;
        }
        *this << "}, 0));" << nl;
    }

    *this << "OCL_CHECK(err, err = q.enqueueTask(krnl));" << nl;

    *this << "OCL_CHECK(err, err = q.enqueueMigrateMemObjects({";
    int count = 0;
    for (auto type:agentTypes) {
        if (count < agentTypes.size()-1)
            *this << "buffer_buff_" << type << ",";
        else
            *this << "buffer_buff_" << type;
        count++;
    }
    *this <<   "}, CL_MIGRATE_MEM_OBJECT_HOST));" << nl;


    *this << "q.finish();" << nl;
  // TODO Cleanup memory
}

void CLHFPGAPrinter::print(const AST::AgentMember &member) {
  if (!member.isArray)
      *this << *member.type << " " << member.name << ";";
  else
      //TODO: improve this
      *this << *member.type << " " << member.name << ";";
}

static void printTypeIdentifier(CLHFPGAPrinter &p, Type type) {
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
void CLHFPGAPrinter::print(const AST::AgentDeclaration &decl) {
    *this << "typedef struct {" << indent
          << *decl.members << outdent << nl;

    if (decl.isRealAgent) {
        if (!generateForGraph) {
            *this << indent << nl << "int envId;" << outdent << nl;
        }
        *this << "}" << decl.name << ";" << nl;
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

void CLHFPGAPrinter::print(const AST::EnvironmentDeclaration &) {
    // Often not used explicitly
//    assert(0);
    if (!generateForGraph) {
        *this << "typedef struct { " << indent << nl
              << "int mem_start;" << nl
              << "int mem_end;" << outdent << nl
              << "} env;" << nl;
        envType = "env";
        envName = "envo";
    }  else {
        *this << envType << " " << envName << "[" << envGraphSize << "]" << ";";
    }
}

void CLHFPGAPrinter::print(const AST::FunctionDeclaration &decl) {
  if (decl.isMain()) {
    // Return result code from main()
    *this << "int main() {" << indent
    << *decl.stmts << nl
    << "printf(\"Time elapsed is seconds\\n\");" << nl
    << "return 0;" << outdent << nl << "}";
    return;
  }
  if (decl.isAnyStep())
    return;
  GenericCPrinter::print(decl);
}

void CLHFPGAPrinter::print(const AST::Script &script) {
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

  useRNG = script.usesRng;

  if (!envGraphSize.isInt())
      *this << "#include \"libabl.h\"" << nl ;
  else
      *this << "#include \"libabl_int.h\"" << nl ;

  *this << "#include \"time.h\"" << nl ;
  *this << "#include \"stdio.h\"" << nl ;
  *this << "#include \"CL/cl.h\"" << nl << nl;
  *this << "#include \"xcl2.hpp\"" << nl << nl;


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
