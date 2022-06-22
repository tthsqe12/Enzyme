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

class CallBlasPattern<dag patternToMatch, list<string> funcNames, list<BLASType> paramTypes, 
                      list<dag> resultOps> : BlasNames {
  dag PatternToMatch = patternToMatch;
  list<string> names = funcNames;
  list<BLASType> inputTypes = paramTypes;
  list<dag> ArgDerivatives = resultOps;
}

// Lv 1 \wo rotations
def : CallBlasPattern<(Op $n, $alpha, $x, $incx),
                  ["scal"],
                  [len, fp, vinc], // 1, 3
                  [(FDiv (DiffeRet<"">), (FAdd (ConstantFP<"1.0"> $x), $x))]
                  >;

def : CallBlasPattern<(Op $n, $x, $incx),
                  ["asum"],
                  [len, vinc],
                  [(FDiv (DiffeRet<"">), (FAdd (ConstantFP<"1.0"> $x), $x))]
                  >;

def : CallBlasPattern<(Op $n, $alpha, $x, $incx, $y, $yinc),
                  ["axpy"],
                  [len, fp, vinc, vinc],
                  [(FDiv (DiffeRet<"">), (FAdd (ConstantFP<"1.0"> $x), $x))]
                  >;

def : CallBlasPattern<(Op $n, $x, $incx, $y, $incy),
                  ["copy"],
                  [len, vinc, vinc],
                  [(FDiv (DiffeRet<"">), (FAdd (ConstantFP<"1.0"> $x), $x))]
                  >;

def : CallBlasPattern<(Op $n, $x, $incx, $y, $incy),
                  ["swap"],
                  [len, vinc, vinc],
                  [(FDiv (DiffeRet<"">), (FAdd (ConstantFP<"1.0"> $x), $x))]
                  >;

def : CallBlasPattern<(Op $n, $x, $incx, $y, $incy),
                  ["dot"],
                  [len, vinc, vinc],
                  [(FDiv (DiffeRet<"">), (FAdd (ConstantFP<"1.0"> $x), $x))]
                  >;

def : CallBlasPattern<(Op $n, $x, $incx),
                  ["nrm2"],
                  [len, vinc],
                  [(FDiv (DiffeRet<"">), (FAdd (ConstantFP<"1.0"> $x), $x))]
                  >;

// Lv 2
def : CallBlasPattern<(Op $layout, $transa, $m, $n, $alpha, $a, $lda, $x, $incx, $beta, $y, $incy),
                  ["gemv"],
                  [cblas_layout, trans, len, len, fp, mld, vinc, fp, vinc],
                  [(FDiv (DiffeRet<"">), (FAdd (ConstantFP<"1.0"> $x), $x))]
                  >;

def : CallBlasPattern<(Op $layout, $m, $n, $alpha, $x, $xinc, $y, $yinc, $v, $ld),
                  ["ger"],
                  [cblas_layout, len, len, fp, vinc, vinc, vld],
                  [(FDiv (DiffeRet<"">), (FAdd (ConstantFP<"1.0"> $x), $x))]
                  >;

def : CallBlasPattern<(Op $layout, $uplo, $n, $alpha, $v, $ld, $x, $xinc, $beta, $y, $yinc),
                  ["spmv"],
                  [cblas_layout, uplo, len, fp, vld, vinc, fp, vinc],
                  [(FDiv (DiffeRet<"">), (FAdd (ConstantFP<"1.0"> $x), $x))]
                  >;

def : CallBlasPattern<(Op $layout, $uplo, $n, $alpha, $x, $incx, $v, $ld),
                  ["spr"],
                  [cblas_layout, uplo, len, fp, vinc, vld],
                  [(FDiv (DiffeRet<"">), (FAdd (ConstantFP<"1.0"> $x), $x))]
                  >;

def : CallBlasPattern<(Op $layout, $uplo, $n, $alpha, $x, $incx, $y, $incy, $v, $ld),
                  ["spr2"],
                  [cblas_layout, uplo, len, fp, vinc, vinc, vld],
                  [(FDiv (DiffeRet<"">), (FAdd (ConstantFP<"1.0"> $x), $x))]
                  >;

def : CallBlasPattern<(Op $layout, $transa, $m, $n, $ml, $mu, $alpha, $v, $ld, $x, $xinc, $beta, $y, $yinc),
                  ["gbmv"],
                  [cblas_layout, trans, len, len, bw, bw, fp, vld, vinc, fp, vinc],
                  [(FDiv (DiffeRet<"">), (FAdd (ConstantFP<"1.0"> $x), $x))]
                  >;

def : CallBlasPattern<(Op $layout, $uplo, $n, $k, $alpha, $v, $ld, $x, $xinc, $beta, $y, $yinc),
                  ["sbmv"],
                  [cblas_layout, uplo, len, hbw, fp, vld, vinc, fp, vinc],
                  [(FDiv (DiffeRet<"">), (FAdd (ConstantFP<"1.0"> $x), $x))]
                  >;

def : CallBlasPattern<(Op $layout, $uplo, $transa, $diag, $n, $v, $ld, $x, $incx),
                  ["trmv"],
                  [cblas_layout, uplo, trans, diag, len, vld, vinc],
                  [(FDiv (DiffeRet<"">), (FAdd (ConstantFP<"1.0"> $x), $x))]
                  >;

def : CallBlasPattern<(Op $layout, $uplo, $transa, $diag, $n, $v, $ld, $x, $incx),
                  ["trsv"],
                  [cblas_layout, uplo, trans, diag, len, vld, vinc],
                  [(FDiv (DiffeRet<"">), (FAdd (ConstantFP<"1.0"> $x), $x))]
                  >;

def : CallBlasPattern<(Op $layout, $uplo, $transa, $diag, $n, $k, $v, $ld, $x, $incx),
                  ["tbmv"],
                  [cblas_layout, uplo, trans, diag, len, bw, vld, vinc],
                  [(FDiv (DiffeRet<"">), (FAdd (ConstantFP<"1.0"> $x), $x))]
                  >;

def : CallBlasPattern<(Op $layout, $uplo, $transa, $diag, $n, $k, $v, $ld, $x, $incx),
                  ["tbsv"],
                  [cblas_layout, uplo, trans, diag, len, bw, vld, vinc],
                  [(FDiv (DiffeRet<"">), (FAdd (ConstantFP<"1.0"> $x), $x))]
                  >;

// Lv 3
def : CallBlasPattern<(Op $layout, $transa, $transb, $l, $n, $m, $alpha, $a, $lda, $b, $ldb, $beta, $c, $ldc),
                  ["gemm"],
                  [cblas_layout, trans, trans, len, len, len, fp, vld, vld, fp, vld], // 7, 9
                  [(FDiv (DiffeRet<"">), (FAdd (ConstantFP<"1.0"> $x), $x))]
                  >;

def : CallBlasPattern<(Op $layout, $side, $uplo, $m, $n, $alpha, $a, $lda, $b, $ldb, $beta, $c, $ldc),
                  ["symm"],
                  [cblas_layout, side, uplo, len, len, fp, vld, vld, fp, vld],
                  [(FDiv (DiffeRet<"">), (FAdd (ConstantFP<"1.0"> $x), $x))]
                  >;

def : CallBlasPattern<(Op $layout, $side, $uplo, $transa, $diag, $m, $n, $alpha, $a, $lda, $b, $ldb),
                  ["trmm"],
                  [cblas_layout, side, uplo, trans, diag, len, len, fp, vld, vld],
                  [(FDiv (DiffeRet<"">), (FAdd (ConstantFP<"1.0"> $x), $x))]
                  >;

def : CallBlasPattern<(Op $layout, $side, $uplo, $transa, $diag, $m, $n, $alpha, $a, $lda, $b, $ldb),
                  ["trsm"],
                  [cblas_layout, side, uplo, trans, diag, len, len, fp, vld, vld],
                  [(FDiv (DiffeRet<"">), (FAdd (ConstantFP<"1.0"> $x), $x))]
                  >;

def : CallBlasPattern<(Op $layout, $uplo, $trans, $n, $k, $alpha, $a, $lda, $beta, $c, $ldc),
                  ["syrk"],
                  [cblas_layout, uplo, trans, len, len, fp, vld, fp, vld],
                  [(FDiv (DiffeRet<"">), (FAdd (ConstantFP<"1.0"> $x), $x))]
                  >;

def : CallBlasPattern<(Op $layout, $uplo, $trans, $n, $k, $alpha, $a, $lda, $b, $ldb, $beta, $c, $ldc),
                  ["syr2k"],
                  [cblas_layout, uplo, trans, len, len, fp, vld, vld, fp, vld],
                  [(FDiv (DiffeRet<"">), (FAdd (ConstantFP<"1.0"> $x), $x))]
                  >;

