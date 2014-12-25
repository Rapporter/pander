// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// splitLine_cpp
std::string splitLine_cpp(std::string str, int max_width, bool use_hyphening, Rcpp::Function hyphen);
RcppExport SEXP pander_splitLine_cpp(SEXP strSEXP, SEXP max_widthSEXP, SEXP use_hypheningSEXP, SEXP hyphenSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< std::string >::type str(strSEXP );
        Rcpp::traits::input_parameter< int >::type max_width(max_widthSEXP );
        Rcpp::traits::input_parameter< bool >::type use_hyphening(use_hypheningSEXP );
        Rcpp::traits::input_parameter< Rcpp::Function >::type hyphen(hyphenSEXP );
        std::string __result = splitLine_cpp(str, max_width, use_hyphening, hyphen);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// tableExpand_cpp
std::string tableExpand_cpp(CharacterVector cells, IntegerVector colsWidth, CharacterVector justify, CharacterVector sepCols, std::string style);
RcppExport SEXP pander_tableExpand_cpp(SEXP cellsSEXP, SEXP colsWidthSEXP, SEXP justifySEXP, SEXP sepColsSEXP, SEXP styleSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< CharacterVector >::type cells(cellsSEXP );
        Rcpp::traits::input_parameter< IntegerVector >::type colsWidth(colsWidthSEXP );
        Rcpp::traits::input_parameter< CharacterVector >::type justify(justifySEXP );
        Rcpp::traits::input_parameter< CharacterVector >::type sepCols(sepColsSEXP );
        Rcpp::traits::input_parameter< std::string >::type style(styleSEXP );
        std::string __result = tableExpand_cpp(cells, colsWidth, justify, sepCols, style);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
