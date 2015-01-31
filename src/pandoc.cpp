#include <Rcpp.h>
#include <string.h>
using namespace Rcpp;

// simplified version of format that fits the need for table.expand
// checks justify param and adds needed number of whitespaces based on it
std::string format_cpp(const std::string &x, const std::string &justify, int width) {
  std::string result = "";
  Rcpp::Function nchar("nchar");
  int xlen = 0;
//  const char *s = x.c_str();
//  while (*s) xlen += (*s++ & 0xc0) != 0x80;
  xlen = as<int>(nchar(x, Rcpp::Named("type") = "width"));
  if (justify == "left"){
    result += x;
    result += std::string(width - xlen, ' ');
  } else if (justify == "right"){
    result += std::string(width - xlen, ' ');
    result += x;
  } else {
    result += std::string((width - xlen)/2, ' ');
    result += x;
    for (int j = (width + xlen)/2; j < width; j++){
      result += " ";
    }
  }
  return result;
}

//[[Rcpp::export]]
std::string tableExpand_cpp(CharacterVector cells, IntegerVector colsWidth, CharacterVector justify, CharacterVector sepCols, std::string style) {
  std::string res = "", word, *line;
  size_t pos;
  char endline = '\n';
  std::vector<std::string> cellsC(cells.size()), *resSplit;
  int i, j, n = 0;
  std::size_t maxLengthCells = 0;
  bool hasLineBreak = false;

  // check for having a line break and convert to string vector for easiness
  for (std::size_t i = 0; i < cellsC.size(); i++){
    cellsC[i] = std::string(cells[i]);
    if (cellsC[i].find(endline) != std::string::npos)
      hasLineBreak = true;
  }

  if (hasLineBreak){
    // in case of line break, we need to recursively call tableExpand_cpp
    // before that we need to find number of resulting calls (max number of line breaks) and prepare arguments for recursive calls
    if (style == "simple" || style == "rmarkdown")
      stop("Pandoc does not support newlines in simple or Rmarkdown table format!");
    n = cellsC.size();
    resSplit = new std::vector<std::string>[n];
    // find max number of line breaks by going from cell to cell
    for (i = 0; i < n; i++){
      line = &cellsC[i];
      do {
        pos = line->find(endline);
        if (line->substr(0, pos) != "")
          resSplit[i].push_back(line->substr(0, pos));
        line->erase(0, pos + 1);
      }
      while (pos != std::string::npos);
      if (resSplit[i].size() > maxLengthCells)
        maxLengthCells = resSplit[i].size();
    }
    // prepare arguments and do a recursive call
    for (std::size_t i = 0; i < maxLengthCells; i++){
      CharacterVector newCells(n);
      for (j = 0; j < n; j++)
        newCells[j] = resSplit[j].size() > i ? resSplit[j][i] : "  ";
      res +=  tableExpand_cpp(newCells, colsWidth, justify, sepCols, style);
      if (i != (maxLengthCells - 1)) // because of collapse usage
        res += '\n';
    }
  } else {
    // in case of no line breaks format each cell (add needed number of whitespaces and justify) and concatenate cells
    res = sepCols[0];
    for (i = 0; i < cells.size(); i++){
      res += format_cpp(as<std::string>(cells[i]), as<std::string>(justify[i]), colsWidth[i]);
      if (i != (cells.length() - 1)) // because of collapse usage
        res += sepCols[1];
    }
    res += sepCols[2];
  }
  return res;
}
