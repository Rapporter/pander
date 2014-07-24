#include <Rcpp.h>
#include <string.h>
#include <cstdio> 
#include <cstdlib>
using namespace Rcpp;

std::string format_cpp(std::string x, std::string justify, int width){
  std::string result = "";
  if (justify == "left"){
    result += x;
    result += std::string(width - x.length(), ' ');
  } else if (justify == "right"){
    result += std::string(width - x.length(), ' ');
    result += x;
  } else {
    result += std::string((width - x.length())/2, ' ');
    result += x;
    for (int j = (width + x.length())/2; j < width; j++)
      result += " ";
  } 
  return result;
}

//[[Rcpp::export]]
std::string tableExpand_cpp(CharacterVector cells, IntegerVector colsWidth, CharacterVector justify, CharacterVector sepCols, std::string style) {
    std::string res = "", word, *line;
    size_t pos;
    char endline = '\n';
    std::vector<std::string> cellsC(cells.size()), *resSplit;
    int i, j, maxLengthCells = 0, n = 0;
    bool hasLineBreak = false;
    // check for having a line break and convert to string vector for easiness
    for (i = 0; i < cellsC.size(); i++){
      cellsC[i] = std::string(cells[i]);
      if (cellsC[i].find(endline) != std::string::npos)
        hasLineBreak = true;
    }
    if (hasLineBreak){
      if (style == "simple" || style == "rmarkdown")
        stop("Pandoc does not support newlines in simple or Rmarkdown table format!");
      n = cellsC.size();
      resSplit = new std::vector<std::string>[n];
      for (i = 0; i < n; i++){
        line = &cellsC[i];
        do
        {
          pos = line->find(endline);
          if (line->substr(0, pos) != "")
            resSplit[i].push_back(line->substr(0, pos));
          line->erase(0, pos + 1);
        }
        while (pos != std::string::npos);
        if (resSplit[i].size() > maxLengthCells)
          maxLengthCells = resSplit[i].size();
      }
      for (i = 0; i < maxLengthCells; i++){
        CharacterVector newCells(n);
        for (j = 0; j < n; j++)
          newCells[j] = resSplit[j].size() > i ? resSplit[j][i] : "  ";
        res +=  tableExpand_cpp(newCells, colsWidth, justify, sepCols, style);
        if (i != (maxLengthCells - 1)) // because of collapse usage
          res += '\n';
      }
    } else {
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
