//===- enzyme-tblgen.cpp - Top-Level TableGen implementation for Enzyme
//-------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the main function for Enzyme's TableGen.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/TableGen/Error.h"
#include "llvm/TableGen/Main.h"
#include "llvm/TableGen/Record.h"
#include "llvm/TableGen/TableGenBackend.h"

using namespace llvm;

enum ActionType { GenDerivatives };

static cl::opt<ActionType>
    action(cl::desc("Action to perform:"),
           cl::values(clEnumValN(GenDerivatives, "gen-derivatives",
                                 "Generate instruction derivative")));

bool hasDiffeRet(Init *resultTree) {
  if (DagInit *resultRoot = dyn_cast<DagInit>(resultTree)) {
    auto opName = resultRoot->getOperator()->getAsString();
    auto Def = cast<DefInit>(resultRoot->getOperator())->getDef();
    if (opName == "DiffeRet" || Def->isSubClassOf("DiffeRet")) {
      return true;
    }
    for (auto zp :
         llvm::zip(resultRoot->getArgs(), resultRoot->getArgNames())) {
      if (hasDiffeRet(std::get<0>(zp)))
        return true;
    }
  }
  return false;
}

void getFunction(raw_ostream &os, std::string callval, std::string FT,
                 std::string cconv, Init *func) {
  if (DagInit *resultRoot = dyn_cast<DagInit>(func)) {
    auto opName = resultRoot->getOperator()->getAsString();
    auto Def = cast<DefInit>(resultRoot->getOperator())->getDef();
    if (opName == "SameFunc" || Def->isSubClassOf("SameFunc")) {
      os << "#if LLVM_VERSION_MAJOR >= 11\n";
      os << "  auto " << callval << " = orig->getCalledOperand();\n";
      os << "#else\n";
      os << "  auto " << callval << " = orig->getCalledValue();\n";
      os << "#endif\n";
      os << "  auto " << FT << " = orig->getFunctionType();\n";
      os << "  auto " << cconv << " = orig->getCallingConv();\n";
      return;
    }
    if (opName == "SameTypesFunc" || Def->isSubClassOf("SameTypesFunc")) {
      os << "  auto " << FT << " = orig->getFunctionType();\n";
      os << "  auto " << callval
         << " = gutils->oldFunc->getParent()->getOrInsertFunction(";
      os << Def->getValueInit("name")->getAsString();
      os << ", " << FT << ", called->getAttributes())\n";
      os << "#if LLVM_VERSION_MAJOR >= 9\n";
      os << "  .getCallee()\n";
      os << "#endif\n";
      os << ";\n";
      os << "  auto " << cconv << " = orig->getCallingConv();\n";
      return;
    }
  }
  assert(0 && "Unhandled function");
}

// Returns whether value generated is a vector value or not.
bool handle(raw_ostream &os, Record *pattern, Init *resultTree,
            std::string builder, StringMap<std::string> &nameToOrdinal,
            bool lookup) {
  if (DagInit *resultRoot = dyn_cast<DagInit>(resultTree)) {
    auto opName = resultRoot->getOperator()->getAsString();
    auto Def = cast<DefInit>(resultRoot->getOperator())->getDef();
    if (opName == "DiffeRet" || Def->isSubClassOf("DiffeRet")) {
      os << "dif";
      return true;
    } else if (opName == "ConstantFP" || Def->isSubClassOf("ConstantFP")) {
      if (resultRoot->getNumArgs() != 1)
        PrintFatalError(pattern->getLoc(), "only single op constant supported");

      auto *argument = resultRoot->getArg(0);

      auto value = dyn_cast<StringInit>(Def->getValueInit("value"));
      if (!value)
        PrintFatalError(pattern->getLoc(), Twine("'value' not defined in ") +
                                               resultTree->getAsString());

      os << "ConstantFP::get(";
      if (resultRoot->getArgName(0)) {
        auto name = resultRoot->getArgName(0)->getAsUnquotedString();
        auto ord = nameToOrdinal.find(name);
        if (ord == nameToOrdinal.end())
          PrintFatalError(pattern->getLoc(), Twine("unknown named operand '") +
                                                 name + "'" +
                                                 resultTree->getAsString());
        os << ord->getValue();
      } else
        PrintFatalError(pattern->getLoc(),
                        Twine("unknown named operand in constantfp") +
                            resultTree->getAsString());
      os << "->getType(), \"" << value->getValue() << "\")";
      return false;
    } else if (opName == "Shadow" || Def->isSubClassOf("Shadow")) {
      if (resultRoot->getNumArgs() != 1)
        PrintFatalError(pattern->getLoc(), "only single op constant supported");

      auto *argument = resultRoot->getArg(0);
      if (lookup)
        os << "lookup(";
      os << "gutils->invertPointerM(" << builder << ", ";

      if (resultRoot->getArgName(0)) {
        auto name = resultRoot->getArgName(0)->getAsUnquotedString();
        auto ord = nameToOrdinal.find(name);
        if (ord == nameToOrdinal.end())
          PrintFatalError(pattern->getLoc(), Twine("unknown named operand '") +
                                                 name + "'" +
                                                 resultTree->getAsString());
        os << ord->getValue();

      } else
        PrintFatalError(pattern->getLoc(),
                        Twine("unknown named operand in shadow") +
                            resultTree->getAsString());
      os << ")";
      if (lookup)
        os << ", " << builder << ")";
      return true;
    }

    os << " ({\n";
    os << "    Value* args[" << resultRoot->getArgs().size() << "];\n";

    SmallVector<bool, 1> vectorValued;
    bool anyVector = false;

    size_t idx = 0;
    StringMap<std::string> oldMaps;
    for (auto zp :
         llvm::zip(resultRoot->getArgs(), resultRoot->getArgNames())) {
      os << " args[" << idx << "] = ";
      idx++;
      if (isa<UnsetInit>(std::get<0>(zp)) && std::get<1>(zp)) {
        auto name = std::get<1>(zp)->getAsUnquotedString();
        auto ord = nameToOrdinal.find(name);
        if (ord == nameToOrdinal.end())
          PrintFatalError(pattern->getLoc(), Twine("unknown named operand '") +
                                                 name + "'" +
                                                 resultTree->getAsString());
        if (!StringRef(ord->getValue()).startswith("__tmp_")) {
          if (lookup)
            os << "lookup(";
          os << "gutils->getNewFromOriginal(";
        }
        os << ord->getValue();
        if (!StringRef(ord->getValue()).startswith("__tmp_")) {
          os << ")";
          if (lookup)
            os << ", " << builder << ")";
        }
        os << " ;\n";
        vectorValued.push_back(false);
        continue;
      }
      vectorValued.push_back(
          handle(os, pattern, std::get<0>(zp), builder, nameToOrdinal, lookup));
      os << " ;\n";
      if (std::get<1>(zp)) {
        auto name = std::get<1>(zp)->getAsUnquotedString();
        oldMaps.try_emplace(name, nameToOrdinal[name]);
        nameToOrdinal[name] = "__tmp_" + name;
        os << " Value* __tmp_" << name << " = args[" << (idx - 1) << "];\n";
      }

      anyVector |= vectorValued.back();
    }
    for (auto &pair : oldMaps) {
      if (pair.second.size())
        nameToOrdinal[pair.getKey()] = pair.second;
      else
        nameToOrdinal.erase(pair.getKey());
    }

    if (opName == "Call" || Def->isSubClassOf("Call")) {
      getFunction(os, "callval", "FT", "cconv", Def->getValueInit("func"));
    }

    os << " Value *res = nullptr;\n";

    if (anyVector) {
      os << " if (gutils->getWidth() == 1) { \n";
    }

    if (opName == "Call" || Def->isSubClassOf("Call")) {
      os << " CallInst *cubcall = cast<CallInst>(" << builder
         << ".CreateCall(FT, callval, ArrayRef<Value*>({";
    } else {
      os << "   res = " << builder << ".Create" << opName << "(";
    }
    for (size_t i = 0; i < idx; i++) {
      if (i > 0)
        os << ", ";
      os << "args[" << i << "]";
    }
    if (opName == "Call" || Def->isSubClassOf("Call"))
      os << "})";
    os << ")";
    if (opName == "Call" || Def->isSubClassOf("Call"))
      os << ")";
    os << ";\n";
    if (opName == "Call" || Def->isSubClassOf("Call")) {
      os << " cubcall->setDebugLoc(gutils->getNewFromOriginal(orig->"
            "getDebugLoc()));\n";
      os << " cubcall->setCallingConv(cconv);\n";
      for (auto *attr : *cast<ListInit>(Def->getValueAsListInit("fnattrs"))) {
        auto attrDef = cast<DefInit>(attr)->getDef();
        os << "#if LLVM_VERSION_MAJOR >= 14\n"
           << " cubcall->addAttributeAtIndex(AttributeList::FunctionIndex, "
           << "Attribute::"
           << attrDef->getValueInit("name")->getAsUnquotedString() << ");\n";
        os << "#else\n"
           << " cubcall->addAttribute(AttributeList::FunctionIndex, "
           << "Attribute::"
           << attrDef->getValueInit("name")->getAsUnquotedString() << ");\n";
        os << "#endif\n";
      }
      os << " res = cubcall;\n";
    }
    if (anyVector) {
      os << " } else {\n";
      os << " for(unsigned int idx=0, W=gutils->getWidth(); idx<W; idx++) {\n";

      if (opName == "Call" || Def->isSubClassOf("Call")) {
        os << " CallInst *V = cast<CallInst>(" << builder
           << ".CreateCall(FT, callval, ArrayRef<Value*>({";
      } else {
        os << "   Value *V = " << builder << ".Create" << opName << "(";
      }
      for (size_t i = 0; i < idx; i++) {
        if (i > 0)
          os << ", ";
        if (vectorValued[i])
          os << builder << ".CreateExtractValue(args[" << i << "], {idx})";
        else
          os << "args[" << i << "]";
      }
      if (opName == "Call" || Def->isSubClassOf("Call"))
        os << "})";
      os << ")";
      if (opName == "Call" || Def->isSubClassOf("Call")) {
        os << ")";
      }
      os << ";\n";

      if (opName == "Call" || Def->isSubClassOf("Call")) {
        os << "   "
              "V->setDebugLoc(gutils->getNewFromOriginal(orig->getDebugLoc()));"
              "\n";
        os << "   V->setCallingConv(cconv);\n";
        for (auto *attr : *cast<ListInit>(Def->getValueAsListInit("fnattrs"))) {
          auto attrDef = cast<DefInit>(attr)->getDef();
          os << "#if LLVM_VERSION_MAJOR >= 14\n"
             << "   V->addAttributeAtIndex(AttributeList::FunctionIndex, "
                "Attribute::"
             << attrDef->getValueInit("name")->getAsUnquotedString() << ");\n";
          os << "#else \n"
             << "   V->addAttribute(AttributeList::FunctionIndex, "
                "Attribute::"
             << attrDef->getValueInit("name")->getAsUnquotedString() << ");\n";
          os << "#endif \n";
        }
      }
      os << "   if (res == nullptr) res = "
            "UndefValue::get(ArrayType::get(V->getType(), "
            "gutils->getWidth()));\n";
      os << "   res = " << builder << ".CreateInsertValue(res, V, {idx});\n";
      os << " }\n }\n";
    }
    os << " res; })";
    return anyVector;
  }

  PrintFatalError(pattern->getLoc(), Twine("unknown dag"));
}

void emitFullDerivatives(const std::vector<Record *> &patterns,
                         raw_ostream &os) {
  // Ensure unique patterns simply by appending unique suffix.
  unsigned rewritePatternCount = 0;
  std::string baseRewriteName = "GeneratedConvert";
  for (Record *pattern : patterns) {
    DagInit *tree = pattern->getValueAsDag("PatternToMatch");

    StringMap<std::string> nameToOrdinal;
    for (int i = 0, e = tree->getNumArgs(); i != e; ++i)
      nameToOrdinal[tree->getArgNameStr(i)] =
          "orig->getOperand(" + std::to_string(i) + ")";

    if (tree->getNameStr().str().size())
      nameToOrdinal[tree->getNameStr().str()] = "orig";

    for (auto arg : tree->getArgs()) {
      if (isa<DagInit>(arg))
        PrintFatalError(pattern->getLoc(),
                        "only single pattern inputs supported");
    }

    // Emit RewritePattern for Pattern.
    ListInit *argOps = pattern->getValueAsListInit("ArgDerivatives");

    os << "  if (";

    bool prev = false;
    for (auto *nameI : *cast<ListInit>(pattern->getValueAsListInit("name"))) {
      if (prev)
        os << " ||\n      ";
      os << "funcName == " << cast<StringInit>(nameI)->getAsString() << "";
      prev = true;
    }
    os << " ){\n";
    os << "    if (gutils->knownRecomputeHeuristic.find(orig) !=\n";
    os << "        gutils->knownRecomputeHeuristic.end()) {\n";
    os << "        if (!gutils->knownRecomputeHeuristic[orig]) {\n";
    os << "          gutils->cacheForReverse(BuilderZ, newCall,\n";
    os << "                                  getIndex(orig, "
          "CacheType::Self));\n";
    os << "        }\n";
    os << "    }\n";

    os << "    eraseIfUnused(*orig);\n";
    os << "    if (gutils->isConstantInstruction(orig))\n";
    os << "      return;\n";

    os << "    switch (Mode) {\n";
    os << "      case DerivativeMode::ForwardModeSplit:\n";
    os << "      case DerivativeMode::ForwardMode:{\n";
    os << "        IRBuilder<> Builder2(&call);\n";
    os << "        getForwardBuilder(Builder2);\n";
    // TODO

    os << "        Value *res = nullptr;\n";

    for (auto argOpEn : llvm::enumerate(*argOps)) {
      size_t argIdx = argOpEn.index();
      os << "        if (!gutils->isConstantValue(orig->getArgOperand("
         << argIdx << "))) {\n";
      os << "          Value *dif = diffe(orig->getArgOperand(" << argIdx
         << "), Builder2);\n";
      DagInit *resultTree = cast<DagInit>(argOpEn.value());
      os << "          Value *tmp = ";

      bool vectorValued = handle(os, pattern, resultTree, "Builder2",
                                 nameToOrdinal, /*lookup*/ false);
      os << ";\n";

      os << "          if (res == nullptr) res = tmp;\n";
      os << "          else if (gutils->getWidth() == 1) res = "
            "Builder2.CreateFAdd(res, tmp);\n";
      os << "          else {\n";
      if (vectorValued)
        os << "            Value *out = UndefValue::get(res->getType());\n";
      else
        os << "            Value *out = "
              "UndefValue::get(gutils->getShadowType(res->getType()));\n";

      os << "            for(unsigned int idx=0, W=gutils->getWidth(); idx<W; "
            "idx++) {\n";
      os << "              Value *V = "
            "Builder2.CreateFAdd(Builder2.CreateExtractValue(res, {idx}), ";
      if (vectorValued)
        os << "Builder2.CreateExtractValue(tmp, {idx})";
      else
        os << "tmp";
      os << ");\n";
      os << "              out = Builder2.CreateInsertValue(out, V, {idx});\n";
      os << "            }\n";
      os << "            res = out;\n";
      os << "          }\n";
      os << "        }\n";
    }

    os << "        setDiffe(orig, res, Builder2);\n";

    os << "        break;\n";
    os << "      }\n";

    os << "      case DerivativeMode::ReverseModeGradient:\n";
    os << "      case DerivativeMode::ReverseModeCombined:{\n";
    os << "        IRBuilder<> Builder2(&call);\n";
    os << "        getReverseBuilder(Builder2);\n";
    // TODO vector

    os << "        Value *dif = nullptr;\n";
    bool seen = false;
    for (auto argOpEn : llvm::enumerate(*argOps)) {
      size_t argIdx = argOpEn.index();
      os << "        ";
      if (seen)
        os << "} else ";
      seen = true;
      os << "if (!dif && !gutils->isConstantValue(orig->getArgOperand("
         << argIdx << "))) {\n";
      DagInit *resultTree = cast<DagInit>(argOpEn.value());
      if (hasDiffeRet(resultTree)) {
        os << "          dif = diffe(orig, Builder2);\n";
        os << "          setDiffe(orig, "
              "Constant::getNullValue(gutils->getShadowType(orig->getType())), "
              "Builder2);\n";
      }
    }
    if (seen)
      os << "        }\n";

    for (auto argOpEn : llvm::enumerate(*argOps)) {
      size_t argIdx = argOpEn.index();
      DagInit *resultTree = cast<DagInit>(argOpEn.value());

      os << "        if (!gutils->isConstantValue(orig->getArgOperand("
         << argIdx << "))) {\n";
      os << "          Value *tmp = ";
      bool vectorValued = handle(os, pattern, resultTree, "Builder2",
                                 nameToOrdinal, /*lookup*/ true);
      os << ";\n";
      os << "          Value *toadd = tmp;\n";

      if (!vectorValued) {
        os << "          if (gutils->getWidth() > 1) {\n";
        os << "            toadd = "
              "UndefValue::get(gutils->getShadowType(tmp->getType()));\n";
        os << "            for(unsigned int idx=0, W=gutils->getWidth(); "
              "idx<W; idx++) {\n";
        os << "              toadd = Builder2.CreateInsertValue(toadd, tmp, "
              "{idx});\n";
        os << "            }\n";
        os << "          }\n";
      }

      os << "          addToDiffe(orig->getArgOperand(" << argIdx << "), toadd";
      os << ", Builder2, orig->getArgOperand(" << argIdx << ")->getType());\n";
      os << "        }\n";
    }

    os << "        break;\n";
    os << "      }\n";

    os << "      case DerivativeMode::ReverseModePrimal:{\n";
    // TODO
    os << "        break;\n";
    os << "      }\n";
    os << "    }\n";

    os << "    return;\n  }\n";
  }
}


// Completed :)
void emitEnumMatcher(const std::vector<Record *> &blas_modes, raw_ostream &os) {
  for (auto mode : blas_modes) {
    auto name = mode->getName();
    auto sub_modes = mode->getValueAsListOfStrings("modes");
    llvm::errs() << "std::string read_" << name
                 << "(llvm::CallInst &call, size_t pos) {\n"
                 << "  std::string s = call.getArgOperand(pos)->getValue();\n";
    for (auto sub_mode : sub_modes) {
      llvm::errs() << "  if (s == \"" << sub_mode << "\")\n"
                   << "    return \"" << sub_mode << "\";\n";
    }
    llvm::errs() << "  assert(false && \"failed reading " << name << "\");\n"
                 << "}\n\n";
  }
}

void writeEnums(Record *pattern, const std::vector<Record *> &blas_modes,
                raw_ostream &os) {
  std::vector<Record *> inputTypes =
      pattern->getValueAsListOfDefs("inputTypes");
  for (auto inputType : inputTypes) {
    if (inputType->isSubClassOf("blas_modes")) {
      llvm::errs() << inputType->getName() << ": ";
      for (auto a : inputType->getValueAsListOfStrings("modes")) {
        llvm::errs() << a << " ";
      }
      llvm::errs() << "\n";
    }
  }
  DagInit *tree = pattern->getValueAsDag("PatternToMatch");
  for (int i = 0, e = tree->getNumArgs(); i != e; ++i) {
    // llvm::errs() << tree->getArgNameStr(i) << " ";
    //  auto optns = blas_arg->getValueAsListOfStrings("modes");
    //  for (auto optn : optns)
    //    llvm::errs() << optn << " ";
    //  }
  }
}

void readLength() {}

void emit_castvals(Record *pattern, std::vector<size_t> activeArgs,
                   raw_ostream &os) {
  llvm::errs() << "Type *castvalls[" << activeArgs.size() << "];\n";

  for (auto argPos : llvm::enumerate(activeArgs)) {
    size_t argIdx = argPos.index();
    llvm::errs() << "if (auto PT = dyn_cast<PointerType>(call.getArgOperand("
                 << argPos.value() << ")->getType()))\n"
                 << "  castvals[" << argIdx << "] = PT;\n"
                 << "else\n"
                 << "  castvals[" << argIdx
                 << "] = PointerType::getUnqual(innerType);\n";
  }
  //              << "Value *undefinit = UndefValue::get(cachetype);\n"
  llvm::errs() << "Value *cacheval;\n\n";
}

void emit_inttype(Record *pattern, raw_ostream &os) {
  // We only look at the type of the first integer showing up.
  // This allows to learn if we use Fortran abi (byRef)
  // or cabi
  size_t firstIntPos = 0;
  bool found = false;
  std::vector<Record *> inputTypes =
      pattern->getValueAsListOfDefs("inputTypes");
  for (auto val : inputTypes) {
    if (val->getName() == "len") {
      found = true;
      // llvm::errs() << "first integer at: " << firstIntPos << "\n";
      break;
    }
    firstIntPos += val->getValueAsInt("nelem");
  }
  assert(found && "no int type found in blas call");

  llvm::errs()
      << "IntegerType *intType = dyn_cast<IntegerType>(call.getOperand("
      << firstIntPos << ")->getType());\n"
      << "bool byRef = false;\n" // Fortran Abi?
      << "if (!intType) {\n"
      << "  auto PT = cast<PointerType>(call.getOperand(" << firstIntPos
      << ")->getType());\n"
      << "  if (blas.suffix.contains(\" 64 \"))\n"
      << "    intType = IntegerType::get(PT->getContext(), 64);\n"
      << "  else\n"
      << "    intType = IntegerType::get(PT->getContext(), 32);\n"
      << "  byRef = true;\n"
      << "}\n\n";
}


void emit_beginning(Record *pattern, raw_ostream &os) {
  auto name = pattern->getValueAsListOfStrings("name")[0];
  llvm::errs()
      << "bool handle_" << name
      << "(BlasInfo blas, llvm::CallInst &call, "
         "Function *called,\n"
      << "const std::map<Argument *, bool> &uncacheable_args,\n"
      << "Type *innerType) {\n"
      << "CallInst *const newCall = "
         "cast<CallInst>(gutils->getNewFromOriginal(&call));\n"
      << "IRBuilder<> BuilderZ(newCall);\n"
      << "BuilderZ.setFastMathFlags(getFast());\n"
      << "IRBuilder<> allocationBuilder(gutils->inversionAllocs);\n"
      << "allocationBuilder.setFastMathFlags(getFast());\n\n"
      << "auto &DL = gutils->oldFunc->getParent()->getDataLayout();\n\n";
}

std::vector<size_t> getPossiblyActiveArgs(Record *pattern) {
  std::vector<Record *> inputTypes =
      pattern->getValueAsListOfDefs("inputTypes");
  int numTypes = 0;
  std::vector<size_t> activeArgs;
  for (auto val : inputTypes) {
    if (val->getValueAsBit("active"))
      activeArgs.push_back(numTypes);
    numTypes += val->getValueAsInt("nelem");
  }

  // verify correctness of declarations in td file
  auto name = pattern->getValue("name")[0];
  DagInit *tree = pattern->getValueAsDag("PatternToMatch");
  int lenDagArgs = tree->getNumArgs();
  llvm::errs() << activeArgs.size() << name;
  assert(numTypes == lenDagArgs);
  return activeArgs;
}

// only for testing
#include "llvm/IR/Type.h"

void emit_ending(Record *pattern, raw_ostream &os) {

  llvm::errs() << "if (gutils->knownRecomputeHeuristic.find(&call) !=\n"
               << "gutils->knownRecomputeHeuristic.end()) {\n"
               << "if (!gutils->knownRecomputeHeuristic[&call]) {\n"
               << "gutils->cacheForReverse(BuilderZ, newCall,\n"
               << " getIndex(&call, CacheType::Self));\n"
               << "}\n"
               << "}\n";

  llvm::errs() << "if (Mode == DerivativeMode::ReverseModeGradient) {\n"
               << "  eraseIfUnused(*orig, /*erase*/ true, /*check*/ false);\n"
               << "} else {\n"
               << "  eraseIfUnused(*orig);\n"
               << "}\n"
               << "return true;\n"
               << "}\n\n";
}

void emit_vinc_caching(Record *pattern, std::vector<size_t> actArgs,
                       raw_ostream &os) {}

void emit_caching(Record *pattern, std::vector<size_t> actArgs,
                  raw_ostream &os) {

  // TODO:
  // For each active type
  //    traverse gradient-dag and collect used arg positions
  //    cache those
  //    profit

  // 1. No caching for fwd-mode
  // 2. Deactivate caching for uncacheable_args
  // 3. Only caching if we do need the primary for an active gradient.
  llvm::errs()
      << "std::vector<bool> toCache;\n"
      << "if (Mode == DerivativeMode::ForwardMode) {\n"
      << "  toCache = std::vector<bool>(actArgs.size(), false)\n"
      << "} else {\n"
      << "  toCache = std::vector<bool>(actArgs.size(), true);\n"
      << "  auto calledFun = call.getCalledFunction();\n"
      << "  for (auto argPos : llvm::enumerate(actArgs)) {\n"
      << "    if "
         "(!uncacheable_args.find(calledFun->getArg(argPos.value()))->second)\n"
      << "      toCache[argPos.index()] = false;\n"
      << "  }\n"
      << "}\n";

  std::vector<bool> inCache(actArgs.size(), false);
  SmallVector<Type *, 2> cacheTypes(actArgs.size());
  bool countcache = false;
  // if (byRef) {
  //   // count must be preserved if overwritten
  //   if (uncacheable_args.find(countarg)->second) {
  //     cacheTypes.push_back(intType);
  //     countcache = true;
  //   }
  //   // xinc is needed to be preserved if
  //   // 1) it is potentially overwritten
  //   //       AND EITHER
  //   //     a) x is active (for performing the shadow increment) or
  //   //     b) we're not caching x and need xinc to compute the derivative
  //   //        of y
  //   if (uncacheable_args.find(xincarg)->second &&
  //       (!gutils->isConstantValue(call.getArgOperand(1)) ||
  //        (!xcache && !gutils->isConstantValue(call.getArgOperand(3))))) {
  //     cacheTypes.push_back(intType);
  //     xinccache = true;
  //   }
  //   // Similarly for yinc
  //   if (uncacheable_args.find(yincarg)->second &&
  //       (!gutils->isConstantValue(call.getArgOperand(3)) ||
  //        (!ycache && !gutils->isConstantValue(call.getArgOperand(1))))) {
  //     cacheTypes.push_back(intType);
  //     yinccache = true;
  //   }
  // }
}

void emitBlasPrimals(RecordKeeper &RK, const std::vector<Record *> &blasPattern,
                     raw_ostream &os) {
  // get Blas superclass which we will populate afterwards
  Record *BlasInst = RK.getClass("BlasInst");
  const auto &foo = RK.getAllDerivedDefinitions("BlasInst");
  assert(foo.size() == 0); // no user-impl allowed

  // Now create primal defs from all BlasPattern
  for (auto pattern : blasPattern) {
    auto name = pattern->getValueAsString("name");

    // Create a Record
    SMLoc loc{};
    ArrayRef arr(loc);
    Record r = Record(name, arr, RK);
    DagInit *d = pattern->getValueAsDag("PatternToMatch");
    r.addTemplateArg(d);

    // Mark Record as impl BlasInst to easier find them later
    r.addSuperClass(BlasInst, SMRange());

    RK.addDef(std::make_unique<Record>(r));
  }
  const auto asdf = RK.getDef("dot");
  assert(asdf->hasDirectSuperClass(BlasInst));
  const auto &bar = RK.getAllDerivedDefinitions(BlasInst->getName());
  llvm::errs() << asdf->getName() << " " << bar.size() << ":"
               << blasPattern.size() << "AAAAAAAAAAAAAAAAA\n";
  // next fails, interesting
  // next ones doesn't work since RK doesn't update itself here?
  assert(bar.size() == blasPattern.size()); // all impl generated
}

std::vector<std::vector<size_t>> getToCachePos(pattern, posActArgs) {
  // TODO: next
  // Just go trough the ArgDerivatives list and compare
  // each dag in it with the input dag.
}

void emitBlasDerivatives(const std::vector<Record *> &blasPatterns,
                         const std::vector<Record *> &blas_modes,
                         raw_ostream &os) {
  // emitEnumMatcher(blas_modes, os);
  for (auto pattern : blasPatterns) {
    std::vector<size_t> posActArgs = getPossiblyActiveArgs(pattern);
    std::vector<std::vector<size_t>> cacheArgPos =
        getToCachePos(pattern, posActArgs);
    // For each active arg we want to have a list of input args required.
    // We use it to find out if we need to cache them.
    assert(posActArgs.size() == cacheArgPos.size());

    emit_beginning(pattern, os);
    emit_castvals(pattern, posActArgs, os);
    emit_inttype(pattern, os);

    // new:
    emit_caching(pattern, posActArgs, os);

    emit_ending(pattern, os);
    // writeEnums(pattern, blas_modes, os);
  }
}

static void emitDerivatives(RecordKeeper &RK, raw_ostream &os) {
  emitSourceFileHeader("Rewriters", os);
  const auto &patterns = RK.getAllDerivedDefinitions("CallPattern");
  const auto &blasPatterns = RK.getAllDerivedDefinitions("CallBlasPattern");
  const auto &blas_modes = RK.getAllDerivedDefinitions("blas_modes");
  Record *attrClass = RK.getClass("Attr");

  // We have full access to the source code to differentiate it
  // emitFullDerivatives(patterns, os);

  // This allows us to also call blas functions which we can use
  // to differentiate blas functions.
  emitBlasPrimals(RK, blasPatterns, os);
  // Improve UX / comp-time by handling Blas calls extra.
  emitBlasDerivatives(blasPatterns, blas_modes, os);
}

static bool EnzymeTableGenMain(raw_ostream &os, RecordKeeper &records) {
  switch (action) {
  case GenDerivatives:
    emitDerivatives(records, os);
    return false;
  }
}

int main(int argc, char **argv) {
  sys::PrintStackTraceOnErrorSignal(argv[0]);
  PrettyStackTraceProgram X(argc, argv);
  cl::ParseCommandLineOptions(argc, argv);

  llvm_shutdown_obj Y;
  return TableGenMain(argv[0], &EnzymeTableGenMain);
}
