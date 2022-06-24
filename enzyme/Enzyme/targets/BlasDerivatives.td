/*
* Design:
* The elements in the BlasName class, together the blas fnc name 
* should be used to match the actual blas calls in the user src code.
* For every CallBlasPattern we define a corresponding (undifferentiated) 
* blas call, using the same parameters as specified in the PatternToMatch.
* We expect one DAG for each active (see BLASType) parameter.
*/

class BLASType<int _n, bit _active> { 
  int nelem = _n; 
  bit active = _active;
  }

def len   : BLASType<1, false>; // num of elements
def hbw   : BLASType<1, false>; // half matrix-bandwith}
def bw    : BLASType<1, false>; // matrix-bandwith}
def fp    : BLASType<1, true>;  // floating point
def ap    : BLASType<1, true>;  // packed array, alternative to vld
def vinc  : BLASType<2, true>;  // {vector, increment}
def vld   : BLASType<2, true>;  // {vector, leading dim}
def mld   : BLASType<2, true>;  // {matrix, leading dim}

class blas_modes<list<string> _modes> : BLASType<1, false> {
  list<string> modes = _modes;
}

def cblas_layout  : blas_modes<["CblasRowMajor","CblasColMajor"]>;
def uplo          : blas_modes<["U","L"]> ;
def cblas_uplo    : blas_modes<["CblasUpper","CblasLower"]> ;
def trans         : blas_modes<["N","T","C"]> ;
def cblas_trans   : blas_modes<["CblasNoTrans","CblasTrans","CblasConjTrans"]> ;
def diag          : blas_modes<["U","N"]> ;
def cblas_diag    : blas_modes<["CblasUnit","CblasNonUnit"]> ;
def side          : blas_modes<["L", "R"]> ;
def cblas_side    : blas_modes<["CblasLeft","CblasRight"]>;


class BlasNames {
  list<string> types = ["s", "d"]; // float, double, TODO: C, Z
  list<string> prefixes = ["", "cblas_", "cublas_"];
  list<string> suffixes = ["", "_", "_64_"];
}

class CallBlasPattern<dag patternToMatch, string funcName, list<BLASType> paramTypes, 
                      list<dag> resultOps> : BlasNames {
  dag PatternToMatch = patternToMatch;
  string name = funcName;
  list<BLASType> inputTypes = paramTypes;
  list<dag> ArgDerivatives = resultOps;
}

// TODO: let these be generated from CallBlasPattern
//class BlasInst : Inst<"blas">;
class BlasInst<dag d> : Inst<"blas"> {
  dag args = d;
}
//def dot  : BlasInst; 
// class TODO: Inst<"blas">;
// def scal : TODO;
// def gemv : TODO;
// def ger  : TODO;
// 
// def dot : BlasInst<(Op $a, $b)>;

class MagicInst : Inst<"blas">;
def sgn  : MagicInst; // only the sign
def zero : MagicInst; // zeros of given length
def todo : MagicInst; // TODO
def tp   : MagicInst; // transpose the trans param.
def noop : MagicInst; // gradient is zero 

// placeholder name
class b<string _s> {
  string s = _s;
}

// Lv 1 \wo rotations
def : CallBlasPattern<(Op $n, $alpha, $x, $incx),
                  "scal",
                  [len, fp, vinc],
                  [
                  (b<"dot"> $x, DiffeRet<"">), 
                  (b<"scal"> $alpha, DiffeRet<"">)
                  ]
                  >;

def : CallBlasPattern<(Op $n, $x, $incx),
                  "asum",
                  [len, vinc],
                  [(b<"scal"> (sgn $x), DiffeRet<"">)]
                  >;

def : CallBlasPattern<(Op $n, $alpha, $x, $incx, $y, $yinc),
                  "axpy",
                  [len, fp, vinc, vinc],
                  [
                  (b<"dot"> $x, DiffeRet<"">), 
                  (b<"scal"> $alpha, DiffeRet<"">),
                  (DiffeRet<"">)
                  ]
                  >;

def : CallBlasPattern<(Op $n, $x, $incx, $y, $incy),
                  "copy",
                  [len, vinc, vinc],
                  [
                  (noop),
                  (todo)
                  ]
                  >;

def : CallBlasPattern<(Op $n, $x, $incx, $y, $incy),
                  "swap",
                  [len, vinc, vinc],
                  []
                  >;

def : CallBlasPattern<(Op $n, $x, $incx, $y, $incy),
                  "dot",
                  [len, vinc, vinc],
                  [
                  (b<"scal"> $n, DiffeRet<"">, $y, $incy), 
                  (b<"scal"> $n, DiffeRet<"">, $x, $incx)
                  ]
                  >;

def : CallBlasPattern<(Op $n, $x, $incx),
                  "nrm2",
                  [len, vinc],
                  [(FDiv (b<"scal"> $x, DiffeRet<"">), Ret<"">)]
                  >;

// Lv 2

// y = alpha Ax + beta y
def : CallBlasPattern<(Op $layout, $transa, $m, $n, $alpha, $A, $lda, $x, $incx, $beta, $y, $incy),
                  "gemv",
                  [cblas_layout, trans, len, len, fp, mld, vinc, fp, vinc],
                  [
                  /* alpha  */  (b<"dot"> DiffeRet<"">, (b<"gemv"> $A, $x)),
                  /* A      */  //(),
                  /* x      */  //(),
                  /* beta   */  (b<"dot"> $y, DiffeRet<"">),
                  /* y      */  (b<"scal"> $beta, DiffeRet<"">),
                  ]
                  >;

def : CallBlasPattern<(Op $layout, $m, $n, $alpha, $x, $xinc, $y, $yinc, $v, $ld),
                  "ger",
                  [cblas_layout, len, len, fp, vinc, vinc, vld],
                  []
                  >;

// def : CallBlasPattern<(Op $layout, $uplo, $n, $alpha, $v, $ld, $x, $xinc, $beta, $y, $yinc),
//                   ["spmv"],
//                   [cblas_layout, uplo, len, fp, vld, vinc, fp, vinc],
//                   []
//                   >;
// 
// def : CallBlasPattern<(Op $layout, $uplo, $n, $alpha, $x, $incx, $v, $ld),
//                   ["spr"],
//                   [cblas_layout, uplo, len, fp, vinc, vld],
//                   []
//                   >;
// 
// def : CallBlasPattern<(Op $layout, $uplo, $n, $alpha, $x, $incx, $y, $incy, $v, $ld),
//                   ["spr2"],
//                   [cblas_layout, uplo, len, fp, vinc, vinc, vld],
//                   []
//                   >;
// 
// def : CallBlasPattern<(Op $layout, $transa, $m, $n, $ml, $mu, $alpha, $v, $ld, $x, $xinc, $beta, $y, $yinc),
//                   ["gbmv"],
//                   [cblas_layout, trans, len, len, bw, bw, fp, vld, vinc, fp, vinc],
//                   []
//                   >;
// 
// def : CallBlasPattern<(Op $layout, $uplo, $n, $k, $alpha, $v, $ld, $x, $xinc, $beta, $y, $yinc),
//                   ["sbmv"],
//                   [cblas_layout, uplo, len, hbw, fp, vld, vinc, fp, vinc],
//                   []
//                   >;
// 
// def : CallBlasPattern<(Op $layout, $uplo, $transa, $diag, $n, $v, $ld, $x, $incx),
//                   ["trmv"],
//                   [cblas_layout, uplo, trans, diag, len, vld, vinc],
//                   []
//                   >;
// 
// def : CallBlasPattern<(Op $layout, $uplo, $transa, $diag, $n, $v, $ld, $x, $incx),
//                   ["trsv"],
//                   [cblas_layout, uplo, trans, diag, len, vld, vinc],
//                   []
//                   >;
// 
// def : CallBlasPattern<(Op $layout, $uplo, $transa, $diag, $n, $k, $v, $ld, $x, $incx),
//                   ["tbmv"],
//                   [cblas_layout, uplo, trans, diag, len, bw, vld, vinc],
//                   []
//                   >;
// 
// def : CallBlasPattern<(Op $layout, $uplo, $transa, $diag, $n, $k, $v, $ld, $x, $incx),
//                   ["tbsv"],
//                   [cblas_layout, uplo, trans, diag, len, bw, vld, vinc],
//                   []
//                   >;
// 
// // Lv 3
// def : CallBlasPattern<(Op $layout, $transa, $transb, $l, $n, $m, $alpha, $a, $lda, $b, $ldb, $beta, $c, $ldc),
//                   ["gemm"],
//                   [cblas_layout, trans, trans, len, len, len, fp, vld, vld, fp, vld],
//                   []
//                   >;
// 
// def : CallBlasPattern<(Op $layout, $side, $uplo, $m, $n, $alpha, $a, $lda, $b, $ldb, $beta, $c, $ldc),
//                   ["symm"],
//                   [cblas_layout, side, uplo, len, len, fp, vld, vld, fp, vld],
//                   []
//                   >;
// 
// def : CallBlasPattern<(Op $layout, $side, $uplo, $transa, $diag, $m, $n, $alpha, $a, $lda, $b, $ldb),
//                   ["trmm"],
//                   [cblas_layout, side, uplo, trans, diag, len, len, fp, vld, vld],
//                   []
//                   >;
// 
// def : CallBlasPattern<(Op $layout, $side, $uplo, $transa, $diag, $m, $n, $alpha, $a, $lda, $b, $ldb),
//                   ["trsm"],
//                   [cblas_layout, side, uplo, trans, diag, len, len, fp, vld, vld],
//                   []
//                   >;
// 
// def : CallBlasPattern<(Op $layout, $uplo, $trans, $n, $k, $alpha, $a, $lda, $beta, $c, $ldc),
//                   ["syrk"],
//                   [cblas_layout, uplo, trans, len, len, fp, vld, fp, vld],
//                   []
//                   >;
// 
// def : CallBlasPattern<(Op $layout, $uplo, $trans, $n, $k, $alpha, $a, $lda, $b, $ldb, $beta, $c, $ldc),
//                   ["syr2k"],
//                   [cblas_layout, uplo, trans, len, len, fp, vld, vld, fp, vld],
//                   []
//                   >;
// 
