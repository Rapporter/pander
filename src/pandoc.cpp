#include <Rcpp.h>
#include <string.h>
#include <cstdio> 
#include <cstdlib>
using namespace Rcpp;

std::string format_cpp(std::string x, std::string justify, int width){
  int i, j;
  std::string result = "";
  if (justify == "left"){
    result += x;
    for (i = x.length(); i < width; i++)
      result += " ";
  } else if (justify == "right"){
    for (i = 0; i < width - x.length(); i++)
      result += " ";
    result += x;
  } else {
    for (i = 0; i < (width - x.length())/2; i++)
      result += " ";
    result += x;
    for (j = i + x.length(); j < width; j++)
      result += " ";
  } 
  return result;
}
//[[Rcpp::export]]
std::string tableExpand_cpp(CharacterVector cells, IntegerVector colsWidth, CharacterVector justify, CharacterVector sepCols, std::string style) {
    std::string res = "", temp, word;
    int i, j;
    bool hasLineBreak = false;
    int width;
    // check for having a line break
    for (i = 0; i < cells.length(); i++){
      temp = as<std::string>(cells[i]);
      if (temp.find('\n') != std::string::npos){
        hasLineBreak = true;
        break;
      }
    }
    if (hasLineBreak){
      if (style == "simple" || style == "rmarkdown")
        stop("Pandoc does not support newlines in simple or Rmarkdown table format!");
      int n = cells.length(), maxLengthCells = 0;
      std::vector<std::string> *resSplit = new std::vector<std::string>[n];
      for (i = 0; i < n; i++){
        temp = std::string(cells[i]);
        char *tempStr = new char[cells[i].size()], *end, *pch;
        std::strcpy(tempStr, temp.c_str());
        pch = strtok_r(tempStr,"\n", &end);
        while (pch != NULL)
        {
          word = std::string(pch);
          resSplit[i].push_back(word);
          pch = strtok_r(NULL, " ", &end);
        }
        if (resSplit[i].size() > maxLengthCells)
          maxLengthCells = resSplit[i].size();
      }
      for (i = 0; i < maxLengthCells; i++){
        CharacterVector newCells(n);
        for (j = 0; j < n; j++){
          if (resSplit[j].size() > i)
            newCells[j] = resSplit[j][i];
          else
            newCells[j] = "  ";
        }
        res += tableExpand_cpp(newCells, colsWidth, justify, sepCols, style);
        if (i != (maxLengthCells - 1))
          res += "\n";
      }
    } else {
      res = sepCols[0];
      for (i = 0; i < cells.length(); i++){
        std::string temp = as<std::string>(cells[i]);
        temp = as<std::string>(justify[i]);
        width = colsWidth[i] + std::count(cells[i].begin(), cells[i].end(), '\\');
        std::string fres = format_cpp(as<std::string>(cells[i]), as<std::string>(justify[i]), width);
        res += fres;
        if (i != (cells.length() - 1)) // because of collapse usage
          res += sepCols[1];
      }
      res += sepCols[2];
    }
    return res;
}
