class BLASType<int _n, bool _active> { 
  int nelem = _n; 
  bool active = _active;
  };

def len<1, false> : BLASType; // num of elements
def fp<1, true> : BLASType; // floating point
def vinc<2, true> : BLASType; // {vector, increment}
def ap<1, true> : BLASType; // packed array, alternative to vld
def vld<2, true> : BLASType; // {vector, leading dim}
def kvld<3, true> : BLASType; // {bandwith, vector, leading dim}
def mld<2, true> : BLASType; // {matrix, leading dim}
def mbw<2, false> : BLASType;; // matrix-bandwith}
def hbw<1, false> : BLASType;; // half matrix-bandwith}

class blas_modes<list<string> _modes> : BLASType {
  let nelem = ?;
  let active = ?;
  list<string> modes = _modes;
}

def cblas_layout<["CblasRowMajor","CblasColMajor"]> : blas_modes;
def uplo<["U","L"]>  : blas_modes{}
def cblas_uplo<["CblasUpper","CblasLower"]> : blas_modes;
def trans<["N","T","C"]> : blas_modes;
def cblas_trans<["CblasNoTrans","CblasTrans","CblasConjTrans"]> : blas_modes;
def diag<["U","N"]> : blas_modes;
def cblas_diag<["CblasUnit","CblasNonUnit"]> : blas_modes;
def side<["L", "R"]> : blas_modes;
def cblas_side<["CblasLeft","CblasRight"]> : blas_modes;


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
                  [len, fp, vinc],
                  []
                  >;

def : CallBlasPattern<(Op $n, $x, $incx),
                  ["asum"],
                  [len, vinc],
                  []
                  >;

def : CallBlasPattern<(Op $n, $alpha, $x, $incx, $y, $yinc),
                  ["axpy"],
                  [len, fp, vinc, vinc],
                  []
                  >;

def : CallBlasPattern<(Op $n, $x, $incx, $y, $incy),
                  ["copy"],
                  [len, vinc, vinc],
                  []
                  >;

def : CallBlasPattern<(Op $n, $x, $incx, $y, $incy),
                  ["swap"],
                  [len, vinc, vinc],
                  []
                  >;

def : CallBlasPattern<(Op $n, $x, $incx, $y, $incy),
                  ["dot"],
                  [len, vinc, vinc],
                  []
                  >;

def : CallBlasPattern<(Op $n, $x, $incx),
                  ["nrm2"],
                  [len, vinc],
                  []
                  >;

// Lv 2
def : CallBlasPattern<(Op $layout, $transa, $m, $n, $alpha, $a, $lda, $x, $incx, $beta, $y, $incy),
                  ["gemv"],
                  [layout, trans, len, len, fp, mld],
                  []
                  >;

def : CallBlasPattern<(Op $layout, $m, $n, $alpha, $x, $xinc, $y, $yinc, $v, $ld),
                  ["ger"],
                  [layout, len, len, fp, vinc, vinc, vld]
                  []
                  >;

def : CallBlasPattern<(Op $layout, $uplo, $n, $alpha, $v, $ld, $x, $xinc, $beta, $y, $yinc),
                  ["spmv"],
                trans,  [layout, uplo, len, fp, vld, vinc, fp, vinc],
                  []
                  >;

def : CallBlasPattern<(Op $layout, $uplo, $n, $alpha, $x, $incx, $v, $ld),
                  ["spr"],
                  [layout, uplo, len, fp, vinc, vld],
                  []
                  >;

def : CallBlasPattern<(Op $layout, $uplo, $n, $alpha, $x, $incx, $y, $incy, $v, $ld),
                  ["spr2"],
                  [layout, uplo, len, fp, vinc, vinc, vld],
                  []
                  >;

def : CallBlasPattern<(Op $layout, $transa, $m, $n, $ml, $mu, $alpha, $v, $ld, $x, $xinc, $beta, $y, $yinc),
                  ["gbmv"],
                  [layout, trans, len, len, mbw, fp, vld, vinc, fp, vinc],
                  []
                  >;

def : CallBlasPattern<(Op $layout, $uplo, $n, $k, $alpha, $v, $ld, $x, $xinc, $beta, $y, $yinc),
                  ["sbmv"],
                  [layout, uplo, len, hbw, fp, vld, vinc, fp, vinc],
                  []
                  >;

def : CallBlasPattern<(Op $layout, $uplo, $transa, $diag, $n, $v, $ld, $x, $incx),
                  ["trmv"],
                  [layout, uplo, trans, diag, len, vld, vinc],
                  []
                  >;

def : CallBlasPattern<(Op $layout, $uplo, $transa, $diag, $n, $v, $ld, $x, $incx),
                  ["trsv"],
                  [layout, uplo, trans, diag, len, vld, vinc],
                  []
                  >;

def : CallBlasPattern<(Op $layout, $uplo, $transa, $diag, $n, $k, $v, $ld, $x, $incx),
                  ["tbmv"],
                  [layout, uplo, trans, diag, len, kvld, vinc],
                  []
                  >;

def : CallBlasPattern<(Op $layout, $uplo, $transa, $diag, $n, $k, $v, $ld, $x, $incx),
                  ["tbsv"],
                  [layout, uplo, trans, diag, len, kvld, vinc],
                  []
                  >;

// Lv 3
def : CallBlasPattern<(Op $layout, $transa, $transb, $l, $n, $m, $alpha, $a, $lda, $b, $ldb, $beta, $c, $ldc),
                  ["gemm"],
                  [layout, trans, trans, len len, len, fp, vld, vld, fp, vld],
                  []
                  >;

def : CallBlasPattern<(Op $layout, $side, $uplo, $m, $n, $alpha, $a, $lda, $b, $ldb, $beta, $c, $ldc),
                  ["symm"],
                  [layout, side, uplo, len, len, fp, vld, vld, fp, vld],
                  []
                  >;

def : CallBlasPattern<(Op $layout, $side, $uplo, $transa, $m, $n, $alpha, $a, $lda, $b, $ldb),
                  ["trmm"],
                  [layout, side, uplo, trans, diag, len, len, fp, vld, vld],
                  []
                  >;

def : CallBlasPattern<(Op $layout, $side, $uplo, $transa, $m, $n, $alpha, $a, $lda, $b, $ldb),
                  ["trsm"],
                  [layout, side, uplo, trans, diag, len, len, fp, vld, vld],
                  []
                  >;

def : CallBlasPattern<(Op $layout, $uplo, $trans, $n, $k, $alpha, $a, $lda, $beta, $c, $ldc),
                  ["syrk"],
                  [layout, uplo, trans, len, len, fp, vld, fp, vld],
                  []
                  >;

def : CallBlasPattern<(Op $layout, $uplo, $trans, $n, $k, $alpha, $a, $lda, $b, $ldb, $beta, $c, $ldc),
                  ["syr2k"],
                  [layout, uplo, trans, len, len, fp, vld, vld, fp, vld],
                  []
                  >;

