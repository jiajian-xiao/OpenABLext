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
#include "CLFPGAPrinter.hpp"

namespace OpenABL {

void CLFPGAPrinter::printType(Type type) {
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

static CLFPGAPrinter &operator<<(CLFPGAPrinter &s, Type type) {
  s.printType(type);
  return s;
}

static void printStorageType(CLFPGAPrinter &s, Type type) {
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

static void printTypeCtor(CLFPGAPrinter &p, const AST::CallExpression &expr) {
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
void CLFPGAPrinter::print(const AST::CallExpression &expr) {
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
    else if (sig.name == "MWC64X")
        *this << "(MWC64X" << "(&rngState[agentIId])*";
    else
        *this << sig.name << "(";
    printArgs(expr);
    *this << ")";
  }
}
void CLFPGAPrinter::print(const AST::MemberInitEntry &entry) {
  *this << "." << entry.name << " = " << *entry.expr << ",";
}
void CLFPGAPrinter::print(const AST::AgentCreationExpression &expr) {
  *this << "(" << expr.name << ") {" << indent
        << *expr.members << outdent << nl << "}";
}
void CLFPGAPrinter::print(const AST::NewArrayExpression &expr) {
    //TODO: dynamic alloc not supported
//  *this << "DYN_ARRAY_CREATE_FIXED(";
//  printStorageType(*this, expr.elemType->resolved);
//  *this << ", " << *expr.sizeExpr << ")";
}

void CLFPGAPrinter::print(const AST::MemberAccessExpression &expr) {
  if (expr.expr->type.isAgent()) {
    if (dynamic_cast<const AST::ArrayAccessExpression *>(&*expr.expr))
        *this << *expr.expr << "." << expr.member;
    else
        *this << *expr.expr << "->" << expr.member;
  } else {
      GenericCPrinter::print(expr);
  }
}

void CLFPGAPrinter::print(const AST::Param &param) {
    *this << *param.type << " " << *param.var;
    if (param.outVar) {
        *this << ", " << *param.type  << *param.outVar;
    }
}

void CLFPGAPrinter::printParams(const AST::FunctionDeclaration &decl) {
    printCommaSeparated(*decl.params, [&](const AST::ParamPtr &param) {
        *this << *param;
    });
}

bool CLFPGAPrinter::isSpecialBinaryOp(
        AST::BinaryOp op, const AST::Expression &left, const AST::Expression &right) {
    Type l = left.type, r = right.type;
    if (op == AST::BinaryOp::MOD && !(l.isInt() && r.isInt())) {
        return true;
    }
    return l.isVec() || r.isVec();
}

void CLFPGAPrinter::printSpecialBinaryOp(
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

void CLFPGAPrinter::print(const AST::AssignStatement &expr) {
  if (expr.right->type.isAgent()) {
    // Agent assignments are interpreted as copies, not reference assignments
    *this << "*" << *expr.left << " = *" << *expr.right << ";";
  } else {
//      GenericCPrinter::print(expr);
    *this << *expr.left << " = " << *expr.right << ";";
  }
}

void CLFPGAPrinter::print(const AST::BinaryOpExpression &expr) {
    if (isSpecialBinaryOp(expr.op, *expr.left, *expr.right)) {
        printSpecialBinaryOp(expr.op, *expr.left, *expr.right);
        return;
    }

    *this << "(" << *expr.left << " "
          << AST::getBinaryOpSigil(expr.op) << " " << *expr.right << ")";
}

void CLFPGAPrinter::print(const AST::VarDeclarationStatement &stmt) {
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
static void printRangeFor(CLFPGAPrinter &p, const AST::ForStatement &stmt) {
  std::string eLabel = p.makeAnonLabel();
  auto range = stmt.getRange();
  p << "for (int " << *stmt.var << " = " << range.first
    << ", " << eLabel << " = " << range.second << "; "
    << *stmt.var << " < " << eLabel << "; ++" << *stmt.var << ") " << *stmt.stmt;
}

//void CLFPGAPrinter::print(const AST::ForStatement &stmt) {
//  if (stmt.isRange()) {
//    printRangeFor(*this, stmt);
//    return;
//  } else if (stmt.isNear()) {
//      //interpret Near
//    const AST::Expression &agentExpr = stmt.getNearAgent();
//    const AST::Expression &radiusExpr = stmt.getNearRadius();
//
//    AST::AgentDeclaration *agentDecl = stmt.type->resolved.getAgentDecl();
//    AST::AgentMember *posMember = agentDecl->getPositionMember();
//    const char *dist_fn = posMember->type->resolved == Type::VEC2
//      ? "dist_flo2" : "dist_flo3";
//
//    std::string eLabel = makeAnonLabel();
//    std::string iLabel = makeAnonLabel();
//    std::string ttmpLabel = makeAnonLabel();
//
//    *this << "__attribute__((xcl_pipeline_loop(1)))" << nl
//          << "for (int " << iLabel << " = 0; "
//          << iLabel << " < len_";
//    printStorageType(*this, stmt.type->resolved);
//    *this << " ; "
//          << iLabel << "++) {"
//          << indent << nl ;
//
//    printStorageType(*this, stmt.type->resolved);
//
//    *this << " " <<ttmpLabel<<" = buff_";
//    printStorageType(*this, stmt.type->resolved);
//    *this << "[" << iLabel << "];" << nl
//          << *stmt.type << " " << *stmt.var << "= &" << ttmpLabel << ";" << nl
//          << "if (" << dist_fn << "(" << *stmt.var << "->" << posMember->name << ", "
//          << agentExpr << "->" << posMember->name << ") > " << radiusExpr
//          << ") continue;" << nl
//          << *stmt.stmt << outdent << nl << "}";
//
//    return;
//  } else if (stmt.isOn()) {
//      //interpret Near
//      const AST::Expression &agentExpr = stmt.getOnEnv();
//      AST::AgentDeclaration *agentDecl = stmt.type->resolved.getAgentDecl();
//      AST::AgentMember *posMember = agentDecl->getPositionMember();
//
//      std::string eLabel = makeAnonLabel();
//      std::string iLabel = makeAnonLabel();
//      std::string ttmpLabel = makeAnonLabel();
//
//      *this << "__attribute__((xcl_pipeline_loop(1)))" << nl
//            << "for (int " << iLabel << " = 0; "
//            << iLabel << " < len_";
//      printStorageType(*this, stmt.type->resolved);
//      *this <<  " ; "
//            << iLabel << "++) {"
//            << indent << nl ;
//      printStorageType(*this, stmt.type->resolved);
//      *this << " " << ttmpLabel<<" = buff_";
//      printStorageType(*this, stmt.type->resolved);
//      *this << "[" << iLabel << "];" << nl
//            << *stmt.type << " " << *stmt.var << "= &" << ttmpLabel << ";" << nl
//            << "if (" << *stmt.var << "->" << posMember->name << ".x != " << agentExpr << "->id) continue;" << nl
//            << *stmt.stmt << outdent << nl << "}";
//      return;
//  }
//
//  // Normal range-based for loop
//  std::string eLabel = makeAnonLabel();
//  std::string iLabel = makeAnonLabel();
//  std::string ttmpLabel = makeAnonLabel();
//
//  *this << "for (size_t " << iLabel << " = 0; "
//        << iLabel << " < len; "
//        << iLabel << "++) {"
//        << indent << nl ;
//  printStorageType(*this, stmt.type->resolved);
//  *this << " " <<ttmpLabel<<" = buff[" << iLabel << "];" << nl
//        << *stmt.type << " " << *stmt.var << "= &" << ttmpLabel << ";" << nl
//        << *stmt.stmt << outdent << nl << "}";
//}
void CLFPGAPrinter::print(const AST::ForStatement &stmt) {
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
        *this << "#pragma HLS dependence variable=envo_";
        printStorageType(*this, stmt.type->resolved);
        *this << " inter false" << nl;
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
        *this << "#pragma HLS dependence variable=buff_";
        printStorageType(*this, stmt.type->resolved);
        *this << " inter false" << nl;
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
        *this << "#pragma HLS dependence variable=buff_";
        printStorageType(*this, stmt.type->resolved);
        *this << " inter false" << nl;
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
void CLFPGAPrinter::print(const AST::ConflictResolutionStatement &stmt){
    if (!isConflictResolutionEnabled)
        return;
}
void CLFPGAPrinter::print(const AST::SimulateStatement &stmt) {
    std::set <Type> agentTypes;
    for (AST::FunctionDeclaration *stepFunc : stmt.stepFuncDecls) {
        const AST::Param &param = *(*stepFunc->params)[0];
        Type type = param.type->resolved;
        agentTypes.insert(type);
    }
    for (auto type:agentTypes) {
        AST::AgentDeclaration *agentDecl = type.getAgentDecl();
        AST::AgentMember *posMember = agentDecl->getPositionMember();
        if (!generateForGraph) {
            std::string idLabel = makeAnonLabel();
            std::string bufLabel = makeAnonLabel();
            *this << "void envId_update_" << type << "(__global " << type << " *buff_" << type
                  << ", int len_" << type << ") {"
                  << indent << nl
                  << "for (int " << idLabel << " = 0 ; " << idLabel << " < len_" << type << " ; " << idLabel << "++) {" << indent << nl
                  << "#pragma HLS dependence variable=buff_" << type << " inter false" << nl
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
                  << "}"
                  << outdent << nl
                  << "}" << nl;

            *this << "void mem_update_" << type << "(__global " << type << " *buff_" << type
                  << ", __global " << type
                  << " *dbuff_" << type
                  << ", int len_" << type
                  << ", __global " << envType << " *" << envName << "_" << type
                  << ") {" << indent << nl;

            *this << "for (int i = 0 ; i < len_" << type << " ; i++) {" << indent << nl
                  << "#pragma HLS PIPELINE II=1" << nl
                  << "#pragma HLS dependence variable=buff_" << type << " inter false" << nl
                  << "#pragma HLS dependence variable=dbuff_" << type << " inter false" << nl
                  << "#pragma HLS dependence variable=envo_" << type << " inter false" << nl
                  << "int x = buff_" << type << "[i].envId;" << nl
                  << "int y = (i==0)?0:buff_" << type << "[i-1].envId;" << nl
                  << nl
                  << "if (i == 0) {" << indent << nl
                  << envName << "_" << type << "[x].mem_start = 0;"
                  << outdent << nl
                  << "}" << nl
                  << "else if (i < len_" << type << ") {" << indent << nl
                  << "if (x != y) {" << indent << nl
                  << envName << "_" << type << "[y].mem_end = i;" << nl
                  << envName << "_" << type << "[x].mem_start = i;" << outdent << nl
                  << "}" << nl
                  << "if (i == len_" << type << "-1) {" << indent << nl
                  << envName << "_" << type << "[x].mem_end = len_" << type << ";"
                  << outdent << nl
                  << "}"
                  << outdent << nl
                  << "}"
                  << outdent << nl
                  << "}";

            *this << outdent << nl
                  << "}" << nl;

            *this << "void count_sort_" << type << "(__global " << type << " *buff_" << type << ", __global " << type << " *dbuff_" << type << ", int len_" << type << ", int exp) {" << indent << nl
                  << "int i, count[10] = {0};" << nl
                  << "for (i = 0; i < len_" << type << "; i++){" << indent << nl
                  << "#pragma HLS PIPELINE II=1" << nl
                  << "#pragma HLS dependence variable=buff_" << type << " inter false" << nl
                  << "#pragma HLS dependence variable=count inter false" << nl
                  << "count[ (buff_" << type << "[i].envId/exp)%10 ]++;"
                  << outdent << nl
                  << "}" << nl
                  << "for (i = 1; i < 10; i++){" << indent << nl
                  << "#pragma HLS unroll" << nl
                  << "count[i] += count[i - 1];"
                  << outdent << nl
                  << "}" << nl
                  << "for (i = len_" << type << " - 1; i >= 0; i--){" << indent << nl
                  << "#pragma HLS PIPELINE II=1" << nl
                  << "#pragma HLS dependence variable=dbuff_" << type << " inter false" << nl
                  << "#pragma HLS dependence variable=buff_" << type << " inter false" << nl
                  << "#pragma HLS dependence variable=count inter false" << nl
                  << "dbuff_" << type << "[count[ (buff_" << type << "[i].envId/exp)%10 ] - 1] = buff_" << type << "[i];" << nl
                  << "count[ (buff_" << type << "[i].envId/exp)%10 ]--;"
                  << outdent << nl
                  << "}" << nl
                  << "for (i = 0; i < len_" << type << "; i++){" << indent << nl
                  << "#pragma HLS PIPELINE II=1"<< nl
                  << "buff_" << type << "[i] = dbuff_" << type << "[i];"
                  << outdent << nl
                  << "}"
                  << outdent << nl
                  << "}" << nl;

            *this << "void radix_sort_" << type << "(__global " << type << " *buff_" << type << ", __global " << type << " *dbuff_" << type << ", int len_" << type << "){" << indent << nl
                  << "int mx = buff_" << type << "[0].envId;" << nl
                  << "for (int i = 1; i < len_" << type << "; i++) {" << indent << nl
                  << "#pragma HLS PIPELINE II=1" << nl
                  << "if (buff_" << type << "[i].envId > mx)" << nl
                  << "mx = buff_" << type << "[i].envId;"
                  << outdent << nl
                  << "}" << nl
                  << "for (int exp = 1; mx/exp > 0; exp *= 10) {" << indent << nl
                  << "count_sort_" << type << "(buff_" << type << ", dbuff_" << type << ", len_" << type << ", exp);"
                  << outdent << nl
                  << "}"
                  << outdent << nl
                  << "}" << nl;
        } else {
            *this << "void mem_update_" << type << "(__global " << type << " *buff_" << type
                  << ", __global " << type
                  << " *dbuff_" << type
                  << ", int len_" << type
                  << ", __global " << envType << " *" << envName << "_" << type
                  << ") {" << indent << nl;

            *this << "for (int i = 0 ; i < len_" << type << " ; i++) {" << indent << nl
                  << "#pragma HLS PIPELINE II=1" << nl
                  << "#pragma HLS dependence variable=buff_" << type << " inter false" << nl
                  << "#pragma HLS dependence variable=dbuff_" << type << " inter false" << nl
                  << "#pragma HLS dependence variable=" << envName << "_" << type << " inter false" << nl
                  << "int x = buff_" << type << "[i]." << posMember->name << ".x;" << nl
                  << "int y = (i==0)?0:buff_" << type << "[i-1]." << posMember->name << ".x;" << nl
                  << nl
                  << "if (i == 0) {" << indent << nl
                  << envName << "_"  << type << "[x].mem_start = 0;"
                  << outdent << nl
                  << "}" << nl
                  << "else if (i < len_" << type << ") {" << indent << nl
                  << "if (x != y) {" << indent << nl
                  << envName << "_"  << type << "[y].mem_end = i;" << nl
                  << envName << "_"  << type << "[x].mem_start = i;" << outdent << nl
                  << "}" << nl
                  << "if (i == len_" << type << "-1) {" << indent << nl
                  << envName << "_"  << type << "[x].mem_end = len_" << type << ";"
                  << outdent << nl
                  << "}"
                  << outdent << nl
                  << "}"
                  << outdent << nl
                  << "}";

            *this << outdent << nl
                  << "}" << nl;

            *this << "void count_sort_" << type << "(__global " << type << " *buff_" << type << ", __global " << type << " *dbuff_" << type << ", int len_" << type << ", int exp) {" << indent << nl
                  << "int i, count[10] = {0};" << nl
                  << "for (i = 0; i < len_" << type << "; i++){" << indent << nl
                  << "#pragma HLS PIPELINE II=1" << nl
                  << "#pragma HLS dependence variable=buff_" << type << " inter false" << nl
                  << "#pragma HLS dependence variable=count inter false" << nl
                  << "count[ (buff_" << type << "[i]." << posMember->name << ".x/exp)%10 ]++;"
                  << outdent << nl
                  << "}" << nl
                  << "for (i = 1; i < 10; i++){" << indent << nl
                  << "#pragma HLS unroll" << nl
                  << "count[i] += count[i - 1];"
                  << outdent << nl
                  << "}" << nl
                  << "for (i = len_" << type << " - 1; i >= 0; i--){" << indent << nl
                  << "#pragma HLS PIPELINE II=1" << nl
                  << "#pragma HLS dependence variable=dbuff_" << type << " inter false" << nl
                  << "#pragma HLS dependence variable=buff_" << type << " inter false" << nl
                  << "#pragma HLS dependence variable=count inter false" << nl
                  << "dbuff_" << type << "[count[ (buff_" << type << "[i]."<< posMember->name << ".x/exp)%10 ] - 1] = buff_" << type << "[i];" << nl
                  << "count[ (buff_" << type << "[i]."<< posMember->name << ".x/exp)%10 ]--;"
                  << outdent << nl
                  << "}" << nl
                  << "for (i = 0; i < len_" << type << "; i++){" << indent << nl
                  << "#pragma HLS PIPELINE II=1"<< nl
                  << "buff_" << type << "[i] = dbuff_" << type << "[i];"
                  << outdent << nl
                  << "}"
                  << outdent << nl
                  << "}" << nl;

            *this << "void radix_sort_" << type << "(__global " << type << " *buff_" << type << ", __global " << type << " *dbuff_" << type << ", int len_" << type << "){" << indent << nl
                  << "int mx = buff_" << type << "[0]."<< posMember->name << ".x;" << nl
                  << "for (int i = 1; i < len_" << type << "; i++) {" << indent << nl
                  << "#pragma HLS PIPELINE II=1" << nl
                  << "if (buff_" << type << "[i]."<< posMember->name << ".x > mx)" << nl
                  << "mx = buff_" << type << "[i]."<< posMember->name << ".x;"
                  << outdent << nl
                  << "}" << nl
                  << "for (int exp = 1; mx/exp > 0; exp *= 10) {" << indent << nl
                  << "count_sort_" << type << "(buff_" << type << ", dbuff_" << type << ", len_" << type << ", exp);"
                  << outdent << nl
                  << "}"
                  << outdent << nl
                  << "}" << nl;
        }

    }
    *this << "kernel __attribute__((reqd_work_group_size(1, 1, 1)))" << nl
          << "void compute_kernel_0" << "(" ;

    for (auto type:agentTypes) {
        *this << "__global " << type << " * buff_" << type << ", __global " << type << " *dbuff_" << type << ", __global " << envType << " *" << envName << "_" << type << ", ";
    }

//    if (generateForGraph) {
//        *this << "__global " << envType << " *" << envName << ", ";
//    }

    for (auto type:agentTypes) {
        *this << "int len_"  << type << ", ";
    }
    *this << "int iteration) {" << indent << nl;

    std::string idLabel = makeAnonLabel();
    std::string agentLabel = makeAnonLabel();
    std::string bufLabel = makeAnonLabel();
    std::string dbufLabel = makeAnonLabel();

    *this << "for (int " << idLabel << " = 0;" << idLabel << " < iteration; " << idLabel << "++) {" << indent << nl;

    int count = 0;
    for (AST::FunctionDeclaration *stepFun : stmt.stepFuncDecls) {
        for (auto type:agentTypes) {
            const AST::Param &paramet = *(*stepFun->params)[0];
            Type typeCurrent = paramet.type->resolved;
            if (typeCurrent != type) continue;

            *this << "step" << count << ":for (int " << agentLabel << " = 0;" << agentLabel << " < len_" << type << "; " << agentLabel
                  << "++) {" << indent << nl
                  << type << " " << bufLabel << " = dbuff_" << type << "[" << agentLabel << "];" << nl
                  << type << " " << dbufLabel << " = " << bufLabel << ";" << nl;

            if (generateForGraph) {
                *this << stepFun->name << "(";
                for (auto type:agentTypes) {
                   *this << "buff_" << type << ", " << "len_" << type << ", " ;
                }
                *this << envName << ", &" << bufLabel << ", &" << dbufLabel << ");" << nl;
            } else {
                *this << stepFun->name << "(";
                for (auto type:agentTypes) {
                    *this << "buff_" << type << ", " << "len_" << type << ", " << "envo_" << type << ", " ;
                }
                *this << "&" << bufLabel << ", &" << dbufLabel << ");" << nl;
            }

            *this << "dbuff_" << type << "[" << agentLabel << "] = " << dbufLabel << ";"
                  << outdent << nl << "}";

            *this << nl
                  << "for (int " << agentLabel << " = 0;" << agentLabel << " < len_" << type << " ; " << agentLabel << "++) {" << indent
                  << nl << "#pragma HLS PIPELINE II=1" << nl
                  << type << " " << dbufLabel << " = dbuff_" << type << "[" << agentLabel << "];" << nl
                  << "buff_" << type << "[" << agentLabel << "] = " << dbufLabel << ";"
                  << outdent << nl << "}" << nl;
        }
        count++;
    }
    for (auto type:agentTypes) {
        if (!generateForGraph)
            *this << "envId_update_" << type << "(buff_" << type << ", len_" << type << ");" << nl;

        *this << "radix_sort_" << type << "(buff_" << type << ", dbuff_" << type << ", len_" << type << ");" << nl
              << "mem_update_" << type << "(buff_" << type << ", dbuff_" << type << ", len_" << type
              << ", " << envName << "_" << type << ");" << nl;
    }
    *this << outdent << nl << "}"
          << outdent << nl << "}" ;
}

void CLFPGAPrinter::print(const AST::AgentMember &member) {
    if (!member.isArray)
        *this << *member.type << " " << member.name << ";";
    else
        //TODO: improve this
        *this << *member.type << " " << member.name << ";";
}

//static void printTypeIdentifier(CLFPGAPrinter &p, Type type) {
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
void CLFPGAPrinter::print(const AST::AgentDeclaration &decl) {
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
}

void CLFPGAPrinter::print(const AST::FunctionDeclaration &decl) {
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

//        *this << "__attribute__((always_inline))" << nl
        *this << *decl.returnType << " " << name << "(";
        for (auto type:agentTypes) {
            *this << "__global " << type << " * buff_" << type << ", int len_" << type << ", "
                  << "__global " << envType << " *" << envName << "_" << type << ", ";
        }

//        if (generateForGraph) {
//            *this << "__global " << envType << " *" << envName << ", ";
//        }

    } else {
        *this << *decl.returnType << " " << name << "(";
    }

    printParams(decl);
    *this << ") {" << indent << *decl.stmts <<  outdent << nl << "}";
}

void CLFPGAPrinter::print(const AST::ConstDeclaration &decl) {
    *this << "__constant " << *decl.type << " " << *decl.var
          << (decl.isArray ? "[]" : "")
          << " = " << *decl.expr << ";";
}

void CLFPGAPrinter::print(const AST::Script &script) {
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
