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
#include "CLPrinter.hpp"

namespace OpenABL {

void CLPrinter::printType(Type type) {
  if (type.isArray()) {
    // Print only the base type
    *this << type.getBaseType();
  } else if (type.isAgent()) {
    *this << type.getAgentDecl()->name << '*';
  } else if (type.isFloat()) {
    *this << (useFloat ? "float" : "double");
  } else if (type.isVec2()) {
      *this << "flo2";
  } else if (type.isVec3()) {
      *this << "flo3";
  }
  else {
    *this << type;
  }
}

static CLPrinter &operator<<(CLPrinter &s, Type type) {
  s.printType(type);
  return s;
}

static void printStorageType(CLPrinter &s, Type type) {
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

static void printTypeCtor(CLPrinter &p, const AST::CallExpression &expr) {
  Type t = expr.type;
  if (t.isVec()) {
    size_t numArgs = expr.args->size();
    p << "flo" << t.getVecLen() << "_"
      << (numArgs == 1 ? "fill" : "create");
    p << "(";
    p.printArgs(expr);
    p << ")";
//    if (numArgs == 1){
//      p << "{";
//      for  (int j=0;j<t.getVecLen()-1;j++) {
//          p.printArgs(expr);
//          p << ",";
//      }
//      p.printArgs(expr);
//      p << "}";
//    } else {
//        p << "{";
//        p.printArgs(expr);
//        p << "}";
//    }
  } else {
      if (t.isFloat() && !p.useFloat)
        p << "( double ) " << *(*expr.args)[0];
      else
        p << "(" << t << ") " << *(*expr.args)[0];
  }
}
void CLPrinter::print(const AST::CallExpression &expr) {
  if (expr.isCtor()) {
    printTypeCtor(*this, expr);
  } else {
    const FunctionSignature &sig = expr.calledSig;
    if (sig.name == "add") {
        //TODO: dynamic increase array size
//      AST::AgentDeclaration *agent = sig.paramTypes[0].getAgentDecl();
//      *this << "*DYN_ARRAY_PLACE(&agents.agents_" << agent->name
//            << ", " << agent->name << ") = " << *(*expr.args)[0];
      return;
    } else if (sig.name == "save") {
//      *this << "save(&agents, agents_info, " << *(*expr.args)[0] << ", SAVE_JSON)";
      return;
    }
    if (sig.name == "dist_float2")
        *this << "dist_flo2" << "(";
    else if (sig.name == "dist_float3")
        *this << "dist_flo3" << "(";
    else if (sig.name == "length_float2")
        *this << "length_flo2" << "(";
    else if (sig.name == "length_float3")
        *this << "length_flo3" << "(";
    else if (sig.name == "normalize_float2")
        *this << "normalize_flo2" << "(";
    else if (sig.name == "normalize_float3")
        *this << "normalize_flo3" << "(";
    else if (sig.name == "dot_float2")
        *this << "dot_flo2" << "(";
    else if (sig.name == "dot_float3")
        *this << "dot_flo3" << "(";
    else if (sig.name == "MWC64X") {
//        const AST::Param &param = *(*sig.decl->params)[0];
//        Type type = param.type->resolved;
        *this << "(MWC64X" << "(&rngState" << "[agentIId])*";
    }
    else
        *this << sig.name << "(";
    printArgs(expr);
    *this << ")";
  }
}
void CLPrinter::print(const AST::MemberInitEntry &entry) {
  *this << "." << entry.name << " = " << *entry.expr << ",";
}
void CLPrinter::print(const AST::AgentCreationExpression &expr) {
  *this << "(" << expr.name << ") {" << indent
        << *expr.members << outdent << nl << "}";
}
void CLPrinter::print(const AST::NewArrayExpression &expr) {
    //TODO: dynamic alloc not supported
//  *this << "DYN_ARRAY_CREATE_FIXED(";
//  printStorageType(*this, expr.elemType->resolved);
//  *this << ", " << *expr.sizeExpr << ")";
}

void CLPrinter::print(const AST::MemberAccessExpression &expr) {
  if (expr.expr->type.isAgent()) {
    if (dynamic_cast<const AST::ArrayAccessExpression *>(&*expr.expr))
        *this << *expr.expr << "." << expr.member;
    else
        *this << *expr.expr << "->" << expr.member;
  } else {
      GenericCPrinter::print(expr);
  }
}

void CLPrinter::print(const AST::Param &param) {
    *this << *param.type << " " << *param.var;
    if (param.outVar) {
        *this << ", " << *param.type  << *param.outVar;
    }
}

void CLPrinter::printParams(const AST::FunctionDeclaration &decl) {
    printCommaSeparated(*decl.params, [&](const AST::ParamPtr &param) {
        *this << *param;
    });
}

bool CLPrinter::isSpecialBinaryOp(
        AST::BinaryOp op, const AST::Expression &left, const AST::Expression &right) {
    Type l = left.type, r = right.type;
    if (op == AST::BinaryOp::MOD && !(l.isInt() && r.isInt())) {
        return true;
    }
    return l.isVec() || r.isVec();
}

void CLPrinter::printSpecialBinaryOp(
        AST::BinaryOp op, const AST::Expression &left, const AST::Expression &right) {
    Type l = left.type;
    Type r = right.type;
    if (l.isVec() || r.isVec()) {
        Type v = l.isVec() ? l : r;
        *this << "flo" << v.getVecLen() << "_";
        switch (op) {
            case AST::BinaryOp::ADD: *this << "add"; break;
            case AST::BinaryOp::SUB: *this << "sub"; break;
            case AST::BinaryOp::DIV: *this << "div_scalar"; break;
            case AST::BinaryOp::MUL: *this << "mul_scalar"; break;
            case AST::BinaryOp::EQUALS: *this << "equals"; break;
            case AST::BinaryOp::NOT_EQUALS: *this << "not_equals"; break;
            default:
                assert(0);
        }
        *this << "(" << left << ", " << right << ")";
        return;
    }

    if (op == AST::BinaryOp::MOD && !(l.isInt() && r.isInt())) {
        *this << "fmod(" << left << ", " << right << ")";
        return;
    }

    assert(0);
}

void CLPrinter::print(const AST::AssignStatement &expr) {
  if (expr.right->type.isAgent()) {
    // Agent assignments are interpreted as copies, not reference assignments
    *this << "*" << *expr.left << " = *" << *expr.right << ";";
  } else {
//      GenericCPrinter::print(expr);
      *this << *expr.left << " = " << *expr.right << ";";
  }
}

void CLPrinter::print(const AST::BinaryOpExpression &expr) {
    if (isSpecialBinaryOp(expr.op, *expr.left, *expr.right)) {
        printSpecialBinaryOp(expr.op, *expr.left, *expr.right);
        return;
    }

    *this << "(" << *expr.left << " "
          << AST::getBinaryOpSigil(expr.op) << " " << *expr.right << ")";
}

void CLPrinter::print(const AST::VarDeclarationStatement &stmt) {
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
static void printRangeFor(CLPrinter &p, const AST::ForStatement &stmt) {
  std::string eLabel = p.makeAnonLabel();
  auto range = stmt.getRange();
  p << "for (int " << *stmt.var << " = " << range.first
    << ", " << eLabel << " = " << range.second << "; "
    << *stmt.var << " < " << eLabel << "; ++" << *stmt.var << ") " << *stmt.stmt;
}

void CLPrinter::print(const AST::ForStatement &stmt) {
  if (stmt.isRange()) {
    printRangeFor(*this, stmt);
    return;
  } else if (stmt.isNear()) {
      //interpret Near
    const AST::Expression &agentExpr = stmt.getNearAgent();
    const AST::Expression &radiusExpr = stmt.getNearRadius();

    AST::AgentDeclaration *agentDecl = stmt.type->resolved.getAgentDecl();
    AST::AgentMember *posMember = agentDecl->getPositionMember();
    const char *dist_fn = posMember->type->resolved == Type::VEC2
      ? "dist_flo2" : "dist_flo3";

    std::string eLabel = makeAnonLabel();
    std::string iLabel = makeAnonLabel();
    std::string ttmpLabel = makeAnonLabel();
    int max_par_size;
    if (envSize.isVec2()) {
      max_par_size = ((int)(envSize.getVec2().x/maxRadius)+1)*((int)(envSize.getVec2().y/maxRadius)+1);
    } else if (envSize.isVec3()) {
      max_par_size = ((int)(envSize.getVec3().x/maxRadius)+1)*((int)(envSize.getVec3().y/maxRadius)+1)
                *((int)(envSize.getVec3().z/maxRadius)+1);
    }

    std::string pLabel = makeAnonLabel();
    std::string qLabel = makeAnonLabel();
    std::string rLabel = makeAnonLabel();
    std::string memStartLabel = makeAnonLabel();
    std::string memEndLabel = makeAnonLabel();

    *this << "for (int " << pLabel << " = -1; " << pLabel << " < 2; " << pLabel << "++)" << nl;
      if (envSize.isVec2())
          *this << "for (int " << qLabel << " = - ((int)(" << envSize.getVec2().x << "/" << maxRadius
                << ")+1); " << qLabel << " < ((int)(" << envSize.getVec2().x << "/" << maxRadius
                << ")+1)+1 ; " << qLabel << " =  " << qLabel << " + ((int)(" << envSize.getVec2().x << "/" << maxRadius << "+1))) {" << indent << nl
                << "int " << rLabel <<" = 0;" << nl;
      else if (envSize.isVec3())
          *this << "for (int " << qLabel << " = - ((int)(" << envSize.getVec3().x << "/" << maxRadius
                << ")+1); " << qLabel << " < ((int)(" << envSize.getVec3().x << "/" << maxRadius
                << ")+1)+1 ; "<< qLabel << " =  " << qLabel <<" + ((int)(" << envSize.getVec3().x << "/" << maxRadius << "+1)))" << nl
                << "for (int "<< rLabel << " = - ((int)(" << envSize.getVec3().y << "/" << maxRadius
                << ")+1) * ((int)(" << envSize.getVec3().x << "/" << maxRadius
                << ")+1); "  << rLabel << " < ((int)(" << envSize.getVec3().y << "/" << maxRadius
                << ")+1) * ((int)(" << envSize.getVec3().x << "/" << maxRadius
                << ")+1) +1; " << rLabel << " =  " << rLabel <<" + ((int)(" << envSize.getVec3().y << "/" << maxRadius
                << "+1)) * ((int)(" << envSize.getVec3().x << "/" << maxRadius << "+1))) {"
                << indent << nl;

    *this << "int envId = " << agentExpr << "->envId + "<<pLabel<<" + "<<qLabel<<" + "<<rLabel<<";" << nl;
    *this << "if ( envId >= 0 && envId < " << max_par_size << " )" << nl << "{" << indent <<nl;
    *this << "int "<< memStartLabel << " = " << envName << "_";
    printStorageType(*this, stmt.type->resolved);
    *this << "[envId].mem_start;" << nl;
    *this << "int "<< memEndLabel <<  " = " << envName << "_";
    printStorageType(*this, stmt.type->resolved);
    *this << "[envId].mem_end;" << nl;
    *this << "if ("<< memStartLabel << " != " << memEndLabel << ")" << nl;
    *this << "for (size_t " << iLabel << " = "
          << memStartLabel << ";"
          << iLabel << " < " << memEndLabel << ";"
          << iLabel << "++) {"
          << indent << nl ;
    printStorageType(*this, stmt.type->resolved);
    *this << " " << ttmpLabel << " = buff_";
    printStorageType(*this, stmt.type->resolved);
    *this << "[" << iLabel << "];" << nl
          << *stmt.type << " " << *stmt.var << "= &" << ttmpLabel << ";" << nl
          << "if (" << dist_fn << "(" << *stmt.var << "->" << posMember->name << ", "
          << agentExpr << "->" << posMember->name << ") > " << radiusExpr
          << ") continue;" << nl
          << *stmt.stmt << outdent << nl << "}";
    *this << outdent << nl <<"}";
    *this << outdent << nl <<"}";

    return;
  } else if (stmt.isOn()) {
      //interpret Near
      const AST::Expression &agentExpr = stmt.getOnEnv();


//      AST::AgentDeclaration *agentDecl = stmt.type->resolved.getAgentDecl();
//      AST::AgentMember *posMember = agentDecl->getPositionMember();
//      const char *dist_fn = posMember->type->resolved == Type::VEC2
//                            ? "dist_flo2" : "dist_flo3";

      std::string eLabel = makeAnonLabel();
      std::string iLabel = makeAnonLabel();
      std::string ttmpLabel = makeAnonLabel();

      // For now: Print normal loop with radius check
      *this << "if ("<<agentExpr << "->mem_start != "<< agentExpr << "->mem_end)" << nl;
      *this << "for (size_t " << iLabel << " = "
            << agentExpr << "->mem_start;"
            << iLabel << " < " << agentExpr << "->mem_end;"
            << iLabel << "++) {"
            << indent << nl ;
      printStorageType(*this, stmt.type->resolved);
      *this << " " <<ttmpLabel<<" = buff_";
      printStorageType(*this, stmt.type->resolved);
      *this << "[" << iLabel << "];" << nl
            << *stmt.type << " " << *stmt.var << "= &" << ttmpLabel << ";" << nl
            << *stmt.stmt << outdent << nl << "}";
      return;
  }

  // Normal range-based for loop
  std::string eLabel = makeAnonLabel();
  std::string iLabel = makeAnonLabel();
  std::string ttmpLabel = makeAnonLabel();

  *this << "for (size_t " << iLabel << " = 0; "
        << iLabel << " < *len; "
        << iLabel << "++) {"
        << indent << nl ;
  printStorageType(*this, stmt.type->resolved);
  *this << " " <<ttmpLabel<<" = buff_";
  printStorageType(*this, stmt.type->resolved);
  *this << "[" << iLabel << "];" << nl
        << *stmt.type << " " << *stmt.var << "= &" << ttmpLabel << ";" << nl
        << *stmt.stmt << outdent << nl << "}";
}
void CLPrinter::print(const AST::ConflictResolutionStatement &stmt){
    if (!isConflictResolutionEnabled)
        return;

//    AST::FunctionDeclaration *stepFunc = script.simStmt->stepFuncDecls[0];
//    const AST::Param &param = *(*stepFunc->params)[0];
//    Type type = param.type->resolved;
    std::set <Type> agentTypes;
    for (AST::FunctionDeclaration *stepFunc : script.simStmt->stepFuncDecls) {
        const AST::Param &param = *(*stepFunc->params)[0];
        Type type = param.type->resolved;
        agentTypes.insert(type);
    }

    std::string envLabel = makeAnonLabel();
    std::string iLabel = makeAnonLabel();
    std::string jLabel = makeAnonLabel();
    std::string ag1Label = makeAnonLabel();
    std::string ag2Label = makeAnonLabel();

    for (auto type:agentTypes) {
        if (generateForGraph) {
            AST::AgentDeclaration *agentDecl = type.getAgentDecl();
            AST::AgentMember *posMember = agentDecl->getPositionMember();

            if (stmt.envName.compare(envName) != 0) {
                std::cerr << "environment name appears in the conflict resolution has been never declared" << std::endl;
                exit(1);
            }

            *this << "__kernel void conflict_resolver_" << type << "(__global " << type << " *buff_" << type
                  << ", __global " << type << " *dbuff_" << type
                  << ", __global int *len_" << type
                  << ", __global " << envType << " *" << envName << "_" << type
                  << ", __global bool *isConflicted_" << type << ") {"
                  << indent << nl
                  << "int i = get_global_id(0);" << nl
                  << "if (i > " << envGraphSize.getInt() << ") return;" << nl
                  << envType << " " << envLabel << " = " << envName << "_" << type << "[" << "i" << "];" << nl
                  << "if (" << envLabel << ".mem_start !=" << envLabel << ".mem_end)" << nl
                  << "for (size_t " << iLabel << " = " << envLabel << ".mem_start;" << iLabel << " < " << envLabel
                  << ".mem_end; " << iLabel << "++)" << nl
                  << "for (size_t " << jLabel << " = " << envLabel << ".mem_start;" << jLabel << " < " << envLabel
                  << ".mem_end; " << jLabel << "++) {" << indent << nl
                  << "if (" << iLabel << "!=" << jLabel << ") {" << indent << nl
                  << type << " " << ag1Label << " = buff_" << type << "[" << iLabel << "];" << nl
                  << type << " " << ag2Label << " = buff_" << type << "[" << jLabel << "];" << nl
                  << "if (" << stmt.tiebreakingFuncDecl->name << "(&" << ag1Label << ", &" << ag2Label << ")) {"
                  << indent << nl
                  << "if ((buff_" << type << "[" << iLabel << "]." << posMember->name << ".x != dbuff_" << type
                  << "[" << iLabel << "]." << posMember->name << ".x) || (buff_"<< type
                  << "[" << iLabel << "]." << posMember->name << ".y != dbuff_Point["
                  << iLabel << "]." << posMember->name << ".y))" << indent << nl
                  << "buff_" << type << "[" << iLabel << "] = " << "dbuff_" << type << "[" << iLabel << "];" << outdent << nl
                  << "else" << indent << nl
                  << "buff_" << type << "[" << jLabel << "] = " << "dbuff_" << type << "[" << jLabel << "];" << outdent << nl
                  << "*isConflicted_" << type << " = true;"
                  << outdent << nl << "}"
                  << outdent << nl << "}"
                  << outdent << nl << "}"
                  << outdent << nl << "}";
        } else {
            int max_par_size;
            if (envSize.isVec2()) {
                max_par_size =
                        ((int) (envSize.getVec2().x / maxRadius) + 1) * ((int) (envSize.getVec2().y / maxRadius) + 1);
            } else if (envSize.isVec3()) {
                max_par_size =
                        ((int) (envSize.getVec3().x / maxRadius) + 1) * ((int) (envSize.getVec3().y / maxRadius) + 1)
                        * ((int) (envSize.getVec3().z / maxRadius) + 1);
            }

            std::string pLabel = makeAnonLabel();
            std::string qLabel = makeAnonLabel();
            std::string rLabel = makeAnonLabel();

            *this << "__kernel void conflict_resolver(__global " << type << " *buff_" << type
                  << ", __global " << type << " *dbuff_" << type
                  << ", __global int *len_" << type
                  << ", __global " << envType << " *" << envName << "_" << type
                  << ", __global bool *isConflicted_" << type
                  << ", __global " << type << "_conflict *conflictBuff_" << type
                  << ") {" << indent << nl;
            *this << "int i = get_global_id(0);" << nl
                  << "if (i > *len_" << type << "-1) return;" << nl
                  << type << " " << ag1Label << "  = buff_" << type << "[i];" << nl;

            *this << "for (int " << pLabel << " = -1; " << pLabel << " < 2; " << pLabel << "++)" << nl;
            if (envSize.isVec2())
                *this << "for (int " << qLabel << " = - ((int)(" << envSize.getVec2().x << "/" << maxRadius
                      << ")+1); " << qLabel << " < ((int)(" << envSize.getVec2().x << "/" << maxRadius
                      << ")+1)+1 ; " << qLabel << " =  " << qLabel << " + ((int)(" << envSize.getVec2().x << "/"
                      << maxRadius << "+1))) {" << indent << nl
                      << "int " << rLabel << " = 0;" << nl;
            else if (envSize.isVec3())
                *this << "for (int " << qLabel << " = - ((int)(" << envSize.getVec3().x << "/" << maxRadius
                      << ")+1); " << qLabel << " < ((int)(" << envSize.getVec3().x << "/" << maxRadius
                      << ")+1)+1 ; " << qLabel << " =  " << qLabel << " + ((int)(" << envSize.getVec3().x << "/"
                      << maxRadius << "+1)))" << nl
                      << "for (int " << rLabel << " = - ((int)(" << envSize.getVec3().y << "/" << maxRadius
                      << ")+1) * ((int)(" << envSize.getVec3().x << "/" << maxRadius
                      << ")+1); " << rLabel << " < ((int)(" << envSize.getVec3().y << "/" << maxRadius
                      << ")+1) * ((int)(" << envSize.getVec3().x << "/" << maxRadius
                      << ")+1) +1; " << rLabel << " =  " << rLabel << " + ((int)(" << envSize.getVec3().y << "/"
                      << maxRadius
                      << "+1)) * ((int)(" << envSize.getVec3().x << "/" << maxRadius << "+1))) {"
                      << indent << nl;

            *this << "int envId = " << ag1Label << ".envId + " << pLabel << " + " << qLabel << " + " << rLabel << ";"
                  << nl
                  << "if ( envId > 0 && envId < " << max_par_size << " )" << nl
                  << "if (" << envName << "_" << type << "[envId].mem_start != " << envName << "_" << type << "[envId].mem_end)" << nl
                  << "for (size_t " << jLabel << " = "
                  << envName << "_" << type << "[envId].mem_start;"
                  << jLabel << " < " << envName << "_" << type << "[envId].mem_end;"
                  << jLabel << "++) {"
                  << indent << nl

                  << type << " " << ag2Label << " = buff_" << type << "[" << jLabel << "];" << nl
                  << "if (i !=" << jLabel << ") {" << indent << nl
                  << "if (" << stmt.tiebreakingFuncDecl->name << "(&" << ag1Label << ", &" << ag2Label << ")) {"
                  << indent << nl
                  << "if (conflictBuff_" << type << "[i].conflictSetPointer == 0) {" << indent << nl
                  << "conflictBuff_" << type << "[i].conflictSet[conflictBuff_" << type << "[i].conflictSetPointer] = i;" << nl
                  << "atomic_inc(&conflictBuff_" << type << "[i].conflictSetPointer);" << nl
                  << "conflictBuff_" << type << "[i].conflictSetSmallest = i;"
                  << outdent << nl << "}" << nl
                  << "conflictBuff_" << type << "[i].conflictSet[conflictBuff_" << type << "[i].conflictSetPointer] = " << jLabel << ";" << nl
                  << "if ( " << jLabel << " < conflictBuff_" << type << "[i].conflictSetSmallest )" << indent << nl
                  << "conflictBuff_" << type << "[i].conflictSetSmallest = " << jLabel << ";" << outdent << nl
                  << "atomic_inc(&conflictBuff_" << type << "[i].conflictSetPointer);" << nl
                  << "*isConflicted_" << type << " = true;"
                  << outdent << nl << "}"
                  << outdent << nl << "}"
                  << outdent << nl << "}"
                  << outdent << nl << "}"
                  << outdent << nl << "}" << nl;

            *this << "__kernel void conflict_resolver_act_" << type
                  << "(__global " << type << " *buff_" << type
                  << ", __global " << type << " *dbuff_" << type
                  << ", __global int *len_" << type
                  << ", __global " << envType << " *" << envName << "_" << type
                  << ", __global bool *isConflicted_" << type
                  << ", __global uint2 *rngState_" << type
                  << ", __global " << type << "_conflict *conflictBuff_" << type << ") {"
                  << indent << nl;
            std::string iterLabel = makeAnonLabel();
            *this << "int i = get_global_id(0);" << nl
                  << "if (i > *len_" << type << "-1 || i!=conflictBuff_" << type << "[i].conflictSetSmallest) return;" << nl
                  << "int roll_back_agent = MWC64X(&rngState_" << type << "[0])*conflictBuff_" << type << "[i].conflictSetPointer;" << nl
                  << "int size = conflictBuff_" << type << "[i].conflictSetPointer;" << nl
                  << "for (size_t " << iterLabel << " = 0 ; " << iterLabel << " < size; " << iterLabel << " ++) {"
                  << indent << nl
                  << "int index = conflictBuff_" << type << "[i].conflictSet[" << iterLabel << "];" << nl
                  << "if (roll_back_agent == " << iterLabel << ") {" << indent << nl
                  << "buff_" << type << "[index] = dbuff_" << type << "[index];"
                  << outdent << nl << "}"
                  << outdent << nl << "}"
                  << outdent << nl << "}";
        }
    }
}
void CLPrinter::print(const AST::SimulateStatement &stmt) {
    std::set <Type> agentTypes;
    for (AST::FunctionDeclaration *stepFunc : stmt.stepFuncDecls) {
        const AST::Param &param = *(*stepFunc->params)[0];
        Type type = param.type->resolved;
        agentTypes.insert(type);
    }

    int num_devices = stmt.num_devices;

    for (int i = 0 ; i < num_devices ; i++) {
        if (num_devices > 1) {
            for (auto type:agentTypes) {
                int stepFuncCount = 0;
                for (AST::FunctionDeclaration *stepFun : stmt.stepFuncDeclOnDevice[i]) {
                    const AST::Param &paramet = *(*stepFun->params)[0];
                    Type typeCurrent = paramet.type->resolved;
                    if (typeCurrent != type) continue;

                    *this << "__kernel void compute_kernel_" << i << "_" << type << "_" << stepFuncCount << "(";
                    int cc = 0;
                    for (auto type:agentTypes) {
                        *this << "__global " << type << " *buff_" << type << ", "
                              << "__global " << type << " *dbuff_" << type << ", "
                              << "__global int *len_" << type << ", "
                              << "__global " << envType << " *" << envName << "_" << type;
                        if (script.usesRng) {
                            if (cc == agentTypes.size()-1)
                                *this << ",  __global uint2 *rngState_" << type;
                            else
                                *this << ",  __global uint2 *rngState_" << type <<", ";
                        }
                        if (cc == agentTypes.size()-1)
                            *this << ") {" << indent << nl;
                        else
                            cc++;
                    }

                    std::string idLabel = makeAnonLabel();
                    std::string bufLabel = makeAnonLabel();
                    std::string dbufLabel = makeAnonLabel();

                    *this << "int " << idLabel << " = get_global_id(0);" << nl
                          << "if (" << idLabel << " >= " << "*len_" << type << ") return;" << nl
                          << type << " " << bufLabel << " = buff_" << type << "[" << idLabel << "];" << nl
                          << type << " " << dbufLabel << " = " << bufLabel << ";" << nl;

                    if (script.usesRng) {
                        *this << stepFun->name << "(";
                        for (auto type:agentTypes) {
                            *this << "buff_" << type << ", "
                                  << "len_" << type << ", "
                                  << envName << "_" << type
                                  << ", rngState_" << type << ", ";
                        }
                        *this << idLabel << ", &" << bufLabel << ", &" << dbufLabel
                              << ");" << nl;
                    } else {
                        *this << stepFun->name << "(";
                        for (auto type:agentTypes) {
                            *this << "buff_" << type << ", "
                                  << "len_" << type << ", "
                                  << envName << "_" << type;
                        }
                        *this << ", &" << bufLabel << ", &" << dbufLabel
                              << ");" << nl;
                    }

                    *this << "buff_" << type << "[" << idLabel << "] = " << dbufLabel << ";" << nl;
                    if (!isConflictResolutionEnabled || stepFuncCount == 0)
                        *this << "dbuff_" << type << "[" << idLabel << "] = " << bufLabel << ";";

                    *this << outdent << nl << "}" << nl;
                    stepFuncCount++;
                }
            }
        } else {
            for (auto type:agentTypes) {
                int stepFuncCount = 0;
                 for (AST::FunctionDeclaration *stepFun : stmt.stepFuncDecls) {
                    const AST::Param &paramet = *(*stepFun->params)[0];
                    Type typeCurrent = paramet.type->resolved;
                    if (typeCurrent != type) continue;

                    *this << "__kernel void compute_kernel_" << i << "_" << type << "_" << stepFuncCount << "(";
                    int cc = 0;
                    for (auto type:agentTypes) {
                        *this << "__global " << type << " *buff_" << type << ", "
                              << "__global " << type << " *dbuff_" << type << ", "
                              << "__global int *len_" << type << ", "
                              << "__global " << envType << " *" << envName << "_" << type;
                        if (script.usesRng) {
                            if (cc == agentTypes.size()-1)
                                *this << ",  __global uint2 *rngState_" << type;
                            else
                                *this << ",  __global uint2 *rngState_" << type <<", ";
                        }
                        if (cc == agentTypes.size()-1)
                            *this << ") {" << indent << nl;
                        else
                            cc++;
                    }

                    std::string idLabel = makeAnonLabel();
                    std::string bufLabel = makeAnonLabel();
                    std::string dbufLabel = makeAnonLabel();

                    *this << "int " << idLabel << " = get_global_id(0);" << nl
                          << "if (" << idLabel << " >= " << "*len_" << type << ") return;" << nl
                          << type << " " << bufLabel << " = buff_" << type << "[" << idLabel << "];" << nl
                          << type << " " << dbufLabel << " = " << bufLabel << ";" << nl;

                    if (script.usesRng) {
                        *this << stepFun->name << "(";
                        for (auto type:agentTypes) {
                            *this << "buff_" << type << ", "
                                  << "len_" << type << ", "
                                  << envName << "_" << type
                                  << ", rngState_" << type << ", ";
                        }
                        *this << idLabel << ", &" << bufLabel << ", &" << dbufLabel
                              << ");" << nl;
                    } else {
                        *this << stepFun->name << "(";
                        for (auto type:agentTypes) {
                            *this << "buff_" << type << ", "
                                  << "len_" << type << ", "
                                  << envName << "_" << type;
                        }
                        *this << ", &" << bufLabel << ", &" << dbufLabel
                              << ");" << nl;
                    }

                    *this << "buff_" << type << "[" << idLabel << "] = " << dbufLabel << ";" << nl;
                    if (!isConflictResolutionEnabled || stepFuncCount == 0)
                        *this << "dbuff_" << type << "[" << idLabel << "] = " << bufLabel << ";";

                    *this << outdent << nl << "}" << nl;
                    stepFuncCount++;
                }
            }
        }

    }

    // print conflict resolution
    std::string id1 = makeAnonLabel();
    std::string id2 = makeAnonLabel();
    std::string agent1 = makeAnonLabel();
    std::string agent2 = makeAnonLabel();

    for (auto type:agentTypes) {
        AST::AgentDeclaration *agentDecl = type.getAgentDecl();
        AST::AgentMember *posMember = agentDecl->getPositionMember();
        if (!generateForGraph) {
            std::string idLabel = makeAnonLabel();
            std::string bufLabel = makeAnonLabel();

            *this << "__kernel void update_envId_" << type << "(__global " << type << " *buff_" << type
                  << ", __global int *len_" << type << ") {"
                  << indent << nl
                  << "int " << idLabel << " = get_global_id(0);" << nl
                  << "if (" << idLabel << " >= " << "*len_" << type << ") return;" << nl
                  << type << " " << bufLabel << " = buff_" << type << "[" << idLabel << "];" << nl;

            if (envMin.isVec2())
                *this << bufLabel << ".envId = (int)((" << bufLabel << "." << posMember->name << ".y - "
                      << envMin.getVec2().y << ")/" << maxRadius
                      << ") * " << "((int)(" << envSize.getVec2().x << "/" << maxRadius << ")+1) + (int)((" << bufLabel
                      << "." << posMember->name << ".x - " <<
                      envMin.getVec2().x << ")/" << maxRadius << ");";
            else if (envMin.isVec3())
                *this << bufLabel << ".envId = (int)((" << bufLabel << "." << posMember->name << ".z - "
                      << envMin.getVec3().z << ")/" << maxRadius
                      << ") * " << "((int)(" << envSize.getVec3().y << "/" << maxRadius << ")+1) * ((int)("
                      << envSize.getVec3().x << "/" << maxRadius << ")+1) + (int)((" << bufLabel
                      << "." << posMember->name << ".y - " <<
                      envMin.getVec3().y << ")/" << maxRadius
                      << ") * " << "((int)(" << envSize.getVec3().x << "/" << maxRadius << ")+1) + (int)((" << bufLabel
                      << "." << posMember->name << ".x - " <<
                      envMin.getVec3().x << ")/" << maxRadius << ");";

            *this << nl << "buff_" << type << "[" << idLabel << "] = " << bufLabel << ";"
                  << outdent << nl
                  << "}" << nl;

            *this << "__kernel void sorting_" << type << "(__global " << type << " *buff_" << type
                  << ", __global " << type
                  << " *dbuff_" << type
                  << ", __global int *len_" << type
                  << ", int inc, int dir) {"
                  << indent << nl
                  << "int i = get_global_id(0);" << nl
                  << "int j = i ^ inc;" << nl
                  << "if (i>j) return;" << nl
                  << type << " ag1 = buff_" << type << "[i];" << nl
                  << type << " ag2 = buff_" << type << "[j];" << nl
                  << "bool smaller = isSortNeeded(ag1.envId, ag2.envId);"
                  << nl
                  << "bool swap = smaller ^ (j < i) ^ ((dir & i) != 0);" << nl
                  << "if (swap) {"
                  << indent << nl
                  << type << " tmp = buff_" << type << "[i];" << nl
                  << "buff_" << type << "[i] = buff_" << type << "[j];" << nl
                  << "buff_" << type << "[j] = tmp;" << nl
                  << "tmp = dbuff_" << type << "[i];" << nl
                  << "dbuff_" << type << "[i] = dbuff_" << type << "[j];" << nl
                  << "dbuff_" << type << "[j] = tmp;"
                  << outdent << nl
                  << "}"
                  << outdent << nl
                  << "}" << nl;

            *this << "__kernel void mem_update_" << type << "(__global " << type << " *buff_" << type
                  << ", __global " << type
                  << " *dbuff_" << type
                  << ", __global int *len_" << type
                  << ", __global " << envType << " *" << envName << "_" << type;

            if (isConflictResolutionEnabled) {
                *this << ", __global bool *isConflicted_" << type << ", __global " << type << "_conflict *conflictBuff_" << type << ") {";
            } else {
                *this << ") {";
            }

            *this << indent << nl
                  << "int i = get_global_id(0);" << nl
                  << "if (i > *len_" << type << "-1) return;" << nl;

            if (isConflictResolutionEnabled) {
                *this << "conflictBuff_" << type << "[i].conflictSetPointer = 0;" << nl
                      << "conflictBuff_" << type << "[i].conflictSetSmallest = -1;" << nl;
            }

            *this << "int x = buff_" << type << "[i].envId;" << nl
                  << "int y = (i==0)?0:buff_" << type << "[i-1].envId;" << nl
                  << nl;

            *this << "if (i == 0) {" << indent << nl
                  << envName << "_" << type << "[x].mem_start = 0;"
                  << outdent << nl
                  << "}" << nl
                  << "else if (i < *len_" << type << ") {" << indent << nl
                  << "if (x != y) {" << indent << nl
                  << envName << "_" << type << "[y].mem_end = i;" << nl
                  << envName << "_" << type << "[x].mem_start = i;" << outdent << nl
                  << "}" << nl
                  << "if (i == *len_" << type << "-1) {" << indent << nl
                  << envName << "_" << type << "[x].mem_end = *len_" << type << ";"
                  << outdent << nl
                  << "}"
                  << outdent << nl
                  << "}";

            if (isConflictResolutionEnabled)
                *this << nl << "*isConflicted_" << type << " = false;";

            *this << outdent << nl
                  << "}" << nl;
        } else {
            *this << "__kernel void sorting_" << type << "(__global " << type << " *buff_" << type
                  << ", __global " << type
                  << " *dbuff_" << type
                  << ", __global int *len_" << type
                  << ", int inc, int dir) {"
                  << indent << nl
                  << "int i = get_global_id(0);" << nl
                  << "int j = i ^ inc;" << nl
                  << "if (i>j) return;" << nl
                  << type << " ag1 = buff_" << type << "[i];" << nl
                  << type << " ag2 = buff_" << type << "[j];" << nl
                  << "bool smaller = isSortNeeded_1(ag1." << posMember->name << ", ag2." << posMember->name << ");"
                  << nl
                  << "bool swap = smaller ^ (j < i) ^ ((dir & i) != 0);" << nl
                  << "if (swap) {"
                  << indent << nl
                  << type << " tmp = buff_" << type << "[i];" << nl
                  << "buff_" << type << "[i] = buff_" << type << "[j];" << nl
                  << "buff_" << type << "[j] = tmp;" << nl
                  << "tmp = dbuff_" << type << "[i];" << nl
                  << "dbuff_" << type << "[i] = dbuff_" << type << "[j];" << nl
                  << "dbuff_" << type << "[j] = tmp;"
                  << outdent << nl
                  << "}"
                  << outdent << nl
                  << "}" << nl;
            *this << "__kernel void mem_update_" << type << "(__global " << type << " *buff_" << type
                  << ", __global " << type << " *dbuff_" << type
                  << ", __global int *len_" << type
                  << ", __global " << envType << " *" << envName << "_"  << type;
            if (isConflictResolutionEnabled) {
                *this << ", __global bool *isConflicted_" << type << ") {";
            } else {
                *this << ") {";
            }

            *this << indent << nl
                  << "int i = get_global_id(0);" << nl
                  << "if (i > *len_" << type << "-1) return;" << nl
                  << "int x = buff_" << type << "[i]." << posMember->name << ".x;" << nl
                  << "int y = (i==0)?0:buff_" << type << "[i-1]." << posMember->name << ".x;" << nl
                  << nl
                  << "if (i == 0) {" << indent << nl
                  << envName << "_"  << type << "[x].mem_start = 0;"
                  << outdent << nl
                  << "}" << nl
                  << "else if (i < *len_" << type << ") {" << indent << nl
                  << "if (x != y) {" << indent << nl
                  << envName << "_"  << type << "[y].mem_end = i;" << nl
                  << envName << "_"  << type << "[x].mem_start = i;" << outdent << nl
                  << "}" << nl
                  << "if (i == *len_" << type << "-1) {" << indent << nl
                  << envName << "_"  << type << "[x].mem_end = *len_" << type << ";"
                  << outdent << nl
                  << "}"
                  << outdent << nl
                  << "}";

            if (isConflictResolutionEnabled)
                *this << nl << "*isConflicted_" << type << " = false;";

            *this << outdent << nl
                  << "}" << nl;
        }
    }
}

void CLPrinter::print(const AST::AgentMember &member) {
    if (!member.isArray)
        *this << *member.type << " " << member.name << ";";
    else
        //TODO: improve this
        *this << *member.type << " " << member.name << ";";
}

//static void printTypeIdentifier(CLPrinter &p, Type type) {
//  switch (type.getTypeId()) {
//    case Type::BOOL: p << "TYPE_BOOL"; break;
//    case Type::INT32: p << "TYPE_INT"; break;
//    case Type::FLOAT: p << "TYPE_FLOAT"; break;
//    case Type::STRING: p << "TYPE_STRING"; break;
//    case Type::VEC2: p << "TYPE_FLOAT2"; break;
//    case Type::VEC3: p << "TYPE_FLOAT3"; break;
//    default: assert(0);
//  }
//}
void CLPrinter::print(const AST::AgentDeclaration &decl) {
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
}

void CLPrinter::print(const AST::FunctionDeclaration &decl) {
    if (decl.isMain()) {
        // Return result code from main()
        for (const AST::StatementPtr &stmt : *decl.stmts) {
            if (dynamic_cast<const AST::SimulateStatement *>(&*stmt) || dynamic_cast<const AST::ConflictResolutionStatement *>(&*stmt)) {
                *this << *stmt << nl;
            }
        }

        return;
    }

    const std::string &name = supportsOverloads ? decl.name : decl.sig.name;
    if (decl.isAnyStep()) {
        std::set <Type> agentTypes;
        for (AST::FunctionDeclaration *stepFunc : script.simStmt->stepFuncDecls) {
            const AST::Param &param = *(*stepFunc->params)[0];
            Type type = param.type->resolved;
            agentTypes.insert(type);
        }

        if (script.usesRng) {
            *this << *decl.returnType << " " << name << "(";
            for (auto type:agentTypes) {
                *this << "__global " << type << " *buff_" << type << ", "
                      << "__global int *len_" << type << ", ";
                if (!generateForGraph)
                    *this << "__global " << envType << " *" << envName << "_" << type
                          << ",  __global uint2 *rngState" << ", ";
                else {
                    *this << "__global " << envType << " *" << envName
                          << ",  __global uint2 *rngState" << ", ";
                }
            }
            *this << "int agentIId, ";
        } else {
            *this << *decl.returnType << " " << name << "(";
            for (auto type:agentTypes) {
                *this << "__global " << type << " *buff_" << type << ", "
                      << "__global int *len_" << type << ", ";
                if (!generateForGraph)
                    *this << "__global " << envType << " *" << envName << "_" << type;
                else
                    *this << "__global " << envType << " *" << envName;
                *this << ", ";
            }
        }
    } else
        *this << *decl.returnType << " " << name << "(" ;
    printParams(decl);

    *this << ") {" << indent << *decl.stmts
          << outdent << nl << "}";

}

void CLPrinter::print(const AST::ConstDeclaration &decl) {
    *this << "__constant " << *decl.type << " " << *decl.var
          << (decl.isArray ? "[]" : "")
          << " = " << *decl.expr << ";";
}

void CLPrinter::print(const AST::Script &script) {
    envName = script.envDecl->envName;
    envType = script.envDecl->envType;
    envMin = script.envDecl->envMin;
    envMax = script.envDecl->envMax;
    envSize = script.envDecl->envSize;
    envGraphSize = script.envDecl->envGraphSize;

    if (envName.compare("") == 0) {
        generateForGraph = false;
        *this << "#include \"libopenl.h\"" << nl << nl ;
        envName = "envo";
        envType = "env";
        maxRadius = script.envDecl->envGranularity.getFloat();
        *this << "typedef struct { " << indent << nl
              << "int mem_start;" << nl
              << "int mem_end;" << outdent << nl
              << "} env;" << nl;
    } else {
        generateForGraph = true;
        *this << "#include \"libopenl_int.h\"" << nl << nl ;
    }

    // First declare all agents
    for (AST::AgentDeclaration *decl : script.agents) {
        *this << *decl << nl;
    }

    // Create structure to store agents
//  *this << "struct agent_struct {" << indent;
//  for (AST::AgentDeclaration *decl : script.agents) {
//    *this << nl << "dyn_array agents_" << decl->name << ";";
//    *this << nl << "dyn_array agents_" << decl->name << "_dbuf;";
//  }
//  *this << outdent << nl << "};" << nl
//        << "struct agent_struct agents;" << nl;
//
//  // Create runtime type information for this structure
//  *this << "static const agent_info agents_info[] = {" << indent << nl;
//  for (AST::AgentDeclaration *decl : script.agents) {
//    *this << "{ " << decl->name << "_info, "
//          << "offsetof(struct agent_struct, agents_" << decl->name
//          << "), \"" << decl->name << "\" }," << nl;
//  }
//  *this << "{ NULL, 0, NULL }" << outdent << nl << "};" << nl << nl;

  // Then declare everything else
    for (AST::ConstDeclaration *decl : script.consts) {
        *this << *decl << nl;
    }

    *this << nl;

    for (AST::FunctionDeclaration *decl : script.funcs) {
        *this << *decl << nl;
    }
}

}
