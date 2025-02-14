
licenses(["notice"])

load("@llvm-project//llvm:tblgen.bzl", "gentbl")

package(
    default_applicable_licenses = [],
    default_visibility = ["//:__subpackages__"],
)

cc_library(
    name="enzyme-tblgen-hdrs",
    hdrs=glob(["tools/enzyme-tblgen/*.h"])
)
cc_binary(
    name="enzyme-tblgen",
    srcs=["tools/enzyme-tblgen/enzyme-tblgen.cpp"],
    deps = [
        "@llvm-project//llvm:Support",
        "@llvm-project//llvm:TableGen",
        "@llvm-project//llvm:config",
        ":enzyme-tblgen-hdrs"
    ]
)
gentbl(
    name = "derivatives",
    tbl_outs = [(
        "-gen-derivatives",
        "InstructionDerivatives.inc",
    )],
    tblgen = ":enzyme-tblgen",
    td_file = "Enzyme/InstructionDerivatives.td",
    td_srcs = ["Enzyme/BlasDerivatives.td"],
    deps = [
        ":enzyme-tblgen",
    ],
)
gentbl(
    name = "blas-derivatives",
    tbl_outs = [(
        "-gen-blas-derivatives",
        "BlasDerivatives.inc",
    )],
    tblgen = ":enzyme-tblgen",
    td_file = "Enzyme/InstructionDerivatives.td",
    td_srcs = ["Enzyme/BlasDerivatives.td"],
    deps = [
        ":enzyme-tblgen",
    ],
)
gentbl(
    name = "blas-attributor",
    tbl_outs = [(
        "-update-blas-declarations",
        "BlasAttributor.inc",
    )],
    tblgen = ":enzyme-tblgen",
    td_file = "Enzyme/InstructionDerivatives.td",
    td_srcs = ["Enzyme/BlasDerivatives.td"],
    deps = [
        ":enzyme-tblgen",
    ],
)
gentbl(
    name = "blas-typeanalysis",
    tbl_outs = [(
        "-gen-blas-typeanalysis",
        "BlasTA.inc",
    )],
    tblgen = ":enzyme-tblgen",
    td_file = "Enzyme/InstructionDerivatives.td",
    td_srcs = ["Enzyme/BlasDerivatives.td"],
    deps = [
        ":enzyme-tblgen",
    ],
)
gentbl(
    name = "blas-diffuseanalysis",
    tbl_outs = [(
        "-gen-blas-diffuseanalysis",
        "BlasDiffUse.inc",
    )],
    tblgen = ":enzyme-tblgen",
    td_file = "Enzyme/InstructionDerivatives.td",
    td_srcs = ["Enzyme/BlasDerivatives.td"],
    deps = [
        ":enzyme-tblgen",
    ],
)

cc_library(
    name = "EnzymeStatic",
    srcs = glob(["Enzyme/*.cpp", "Enzyme/TypeAnalysis/*.cpp", "Enzyme/Clang/EnzymeClang.cpp"]),
    hdrs = glob(["Enzyme/*.h", "Enzyme/TypeAnalysis/*.h"]),
    data = ["@llvm-project//clang:builtin_headers_gen"],
    copts = ["-DENZYME_RUNPASS=1"],
    deps = [
        "@llvm-project//clang:ast",
        "@llvm-project//clang:basic",
        "@llvm-project//clang:driver",
        "@llvm-project//clang:frontend",
        "@llvm-project//clang:frontend_tool",
        "@llvm-project//clang:lex",
        "@llvm-project//clang:serialization",
        "@llvm-project//llvm:Support",
        "@llvm-project//llvm:Core",
        "@llvm-project//llvm:IRReader",
        ":derivatives",
        ":blas-derivatives",
        ":blas-attributor",
        ":blas-typeanalysis",
        ":blas-diffuseanalysis"
    ],
    visibility = ["//visibility:public"],
)
