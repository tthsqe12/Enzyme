class Type;
class CallPattern<dag patternToMatch, list<string> funcNames, list<dag> resultOps> {
  dag PatternToMatch = patternToMatch;
  list<string> names = funcNames;
  list<dag> ArgDerivatives = resultOps;
}

class Inst<string mnemonic> {
  string name = mnemonic;
}

// Define ops to rewrite.

def FDiv : Inst<"FDiv">;
def FAdd : Inst<"FAdd">;
def FSub : Inst<"FSub">;
def FMul : Inst<"FMul">;
def FNeg : Inst<"FNeg">;
def Select : Inst<"Select">;
def FCmpOEQ : Inst<"FCmpOEQ">;

class ExtractValue<list<int> indices_> : Inst<"ExtractValue"> {
  list<int> indices = indices_;
}

class InsertValue<list<int> indices_> : Inst<"InsertValue"> {
  list<int> indices = indices_;
}

def DifR : Inst<"DifR">;

// Same function as the one being called
def SameFunc {
}

// Same types / attributes / callingconv as original
// Different name, however
class SameTypesFunc<string name_> {
  string name = name_;
}

class Attribute<string name_> {
  string name = name_;
}

// So we can also extend it by not input related 
// types, eg. for mem related Intrinsics
class TypeInfo {
}

class TypeOf<string val> : TypeInfo {
}

class Intrinsic<string name_, list<dag> types_> {
  string name = name_;
  list<dag> types = types_;
}

def ReadNone : Attribute<"ReadNone">;
def ReadOnly : Attribute<"ReadOnly">;
def NoUnwind : Attribute<"NoUnwind">;

class Call<dag mnemonic, list<Attribute> _fnattrs=[]> {
  dag func = mnemonic;
  list<Attribute> fnattrs = _fnattrs;
}

def Op {
}
def ArrayRet {
}

class ConstantFP<string val> {
  string value = val;
}
class Undef {
}

class DiffeRet<string val> {
}

class Shadow<string val> {
}

def : CallPattern<(Op $x),
                  ["atan", "atanf", "atanl", "__fd_atan_1"],
                  [(FDiv (DiffeRet<"">), (FAdd (FMul $x, $x), (ConstantFP<"1.0"> $x)))]
                  >;

def : CallPattern<(Op $y, $x),
                  ["atan2", "atan2f", "atan2l"],
                  [
                  (FDiv (FMul (DiffeRet<"">), $x), (FAdd (FMul $x, $x), (FMul $y, $y))),
                  (FNeg (FDiv (FMul (DiffeRet<"">), $y), (FAdd (FMul $x, $x), (FMul $y, $y))))
                  ]
                  >;
def : CallPattern<(Op $x),
                  ["cbrt", "cbrtf", "cbrtl"],
                  [(FDiv (FMul (DiffeRet<"">), (Call<(SameFunc), [ReadNone,NoUnwind]> $x) ), (FMul (ConstantFP<"3.0"> $x), $x))]
                  >;

def : CallPattern<(Op $x, $y),
                  ["hypot", "hypotf", "hypotl"],
                  [
                    (FDiv (FMul (DiffeRet<"">), $x), (Call<(SameFunc), [ReadNone,NoUnwind]> $x, $y)),
                    (FDiv (FMul (DiffeRet<"">), $y), (Call<(SameFunc), [ReadNone,NoUnwind]> $x, $y))
                  ]
                  >;

def : CallPattern<(Op $x),
                  ["tanh"],
                  [(FDiv (DiffeRet<"">), (FMul(Call<(SameTypesFunc<"cosh">), [ReadNone,NoUnwind]> $x):$c, $c))]>;

def : CallPattern<(Op $x),
                  ["tanhf"],
                  [(FDiv (DiffeRet<"">), (FMul(Call<(SameTypesFunc<"coshf">), [ReadNone,NoUnwind]> $x):$c, $c))]>;

def : CallPattern<(Op $x),
                  ["cosh"],
                  [(FMul (DiffeRet<"">), (Call<(SameTypesFunc<"sinh">), [ReadNone,NoUnwind]> $x))]>;
def : CallPattern<(Op $x),
                  ["coshf"],
                  [(FMul (DiffeRet<"">), (Call<(SameTypesFunc<"sinhf">), [ReadNone,NoUnwind]> $x))]>;

def : CallPattern<(Op $x),
                  ["sinh"],
                  [(FMul (DiffeRet<"">), (Call<(SameTypesFunc<"cosh">), [ReadNone,NoUnwind]> $x))]>;
def : CallPattern<(Op $x),
                  ["sinhf"],
                  [(FMul (DiffeRet<"">), (Call<(SameTypesFunc<"coshf">), [ReadNone,NoUnwind]> $x))]>;

def : CallPattern<(Op $x),
                  ["exp10"],
                  [(FMul (FMul (DiffeRet<"">), (Call<(SameFunc), [ReadNone,NoUnwind]> $x) ), (ConstantFP<"2.30258509299404568401799145468"> $x))]
                  >;
def : CallPattern<(Op $x),
                  ["tan", "tanf", "tanl"],
                  [(FMul (DiffeRet<"">), (FAdd (ConstantFP<"1.0"> $x), (FMul(Call<(SameFunc), [ReadNone,NoUnwind]> $x):$c, $c)))]>;
def : CallPattern<(Op $x, $y),
                  ["remainder"],
                  [
                    (DiffeRet<"">),
                    (FMul (FNeg (DiffeRet<"">)), (Intrinsic<"round", [(TypeOf<""> $x)]> (FDiv $x, $y)))
                  ]
                  >;
def : CallPattern<(Op $x),
                  ["asin", "asinf", "asinl"],
                  [(FDiv (DiffeRet<"">), (Intrinsic<"sqrt", [(TypeOf<""> $x)]> (FSub (ConstantFP<"1.0"> $x), (FMul $x, $x)))  )]                  
                  >;
def : CallPattern<(Op $x),
                  ["acos", "acosf", "acosl"],
                  [(FNeg (FDiv (DiffeRet<"">), (Intrinsic<"sqrt", [(TypeOf<""> $x)]> (FSub (ConstantFP<"1.0"> $x), (FMul $x, $x)))  ))]                  
                  >;
def : CallPattern<(Op $x),
                  ["log1p", "log1pf", "log1pl"],
                  [(FDiv (DiffeRet<"">), (FAdd $x, (ConstantFP<"1.0"> $x)))]
                  >;
def : CallPattern<(Op $x),
                  ["expm1"],
                  [(FMul (Intrinsic<"exp", [(TypeOf<""> $x)]> $x), (DiffeRet<"">))]
                  >;

def : CallPattern<(Op $x), ["jl_rem2pi", "jl_rem2pif", "jl_rem2pil"],[(DiffeRet<"">)]>;

// Unnormalized sinc(x) = sin(x)/x
def : CallPattern<(Op $x),
                  ["sinc", "sincf", "sincl"],
                  [(Select (FCmpOEQ $x, (ConstantFP<"0"> $x)),
                        (ConstantFP<"0"> $x),
                  (FMul (DiffeRet<"">), (FDiv (FSub (Intrinsic<"cos", [(TypeOf<""> $x)]> $x), (Call<(SameFunc), [ReadNone,NoUnwind]> $x)), $x)))]>;

// Normalized sinc(x) = sin(pi x)/(pi x)
def : CallPattern<(Op $x),
                  ["sincn", "sincnf", "sincnl"],
                  [
                    (Select (FCmpOEQ $x, (ConstantFP<"0"> $x)),
                        (ConstantFP<"0"> $x),
                  (FMul (DiffeRet<"">), (FDiv (FSub (Intrinsic<"cos", [(TypeOf<""> $x)]> (FMul (ConstantFP<"3.1415926535897962684626433"> $x), $x)), (Call<(SameFunc), [ReadNone,NoUnwind]> $x)), $x)))]>;

// Normalized Faddeeva_erfcx_re(x) = Exp[z^2] Erfc[z] -> 2 dx ( x f(x) - 1/sqrt(pi))
def : CallPattern<(Op $x),
                  ["Faddeeva_erfcx_re"],
                  [
                  (FMul (DiffeRet<"">), (FMul (ConstantFP<"2.0"> $x), (FSub (FMul $x, (Call<(SameFunc), [ReadNone,NoUnwind]> $x)), (ConstantFP<"0.56418958354775628694807945156077258584405062932900"> $x) )))
                  ]>;

def : CallPattern<(Op $x, $y),
                  ["fmod", "fmodf", "fmodl"],
                  [
                  (DiffeRet<"">),
                  (FMul (DiffeRet<"">), (FNeg (Intrinsic<"copysign", [(TypeOf<""> $x)]> (Intrinsic<"floor", [(TypeOf<""> $x)]> (Intrinsic<"fabs", [(TypeOf<""> $x)]> (FDiv $x, $y))), (FDiv $x, $y))))
                  ]
                  >;

def : CallPattern<(Op $x),
                  ["__fd_sincos_1", "__fd_sincos_1f", "__fd_sincos_1l"],
                  [
                  (
                   ArrayRet (FMul (ExtractValue<[1]> (Call<(SameFunc), [ReadNone,NoUnwind]> $x):$callset), (DiffeRet<"">) ),
                            (FNeg (FMul (ExtractValue<[0]> $callset), (DiffeRet<"">) ) )
                  )
                  ]
                  >;

def : CallPattern<(Op $x),
                  ["sincospi", "sincospif", "sincospil"],
                  [
                  (
                   ArrayRet (FMul (ConstantFP<"3.1415926535897962684626433"> $x), (FMul (ExtractValue<[1]> (Call<(SameFunc), [ReadNone,NoUnwind]> $x):$callset), (DiffeRet<"">) ) ),
                            (FMul (ConstantFP<"3.1415926535897962684626433"> $x),  (FNeg (FMul (ExtractValue<[0]> $callset), (DiffeRet<"">) ) ) )
                  )
                  ]
                  >;

def : CallPattern<(Op $x),
                  ["sinpi", "sinpif", "sinpil", "cospi", "cospif", "cospil"],
                  [
                  (
                   FMul (ConstantFP<"3.1415926535897962684626433"> $x), (FMul (Call<(SameFunc), [ReadNone,NoUnwind]> (FAdd $x, (ConstantFP<"0.5"> $x))), (DiffeRet<"">) )
                  )
                  ]
                  >;

include "BlasDerivatives.td"                  
