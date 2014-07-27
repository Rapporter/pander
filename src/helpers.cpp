#include <Rcpp.h>
#include <string>
#include <cstdio> 
#include <cstdlib>

using namespace std;

int paste_wrapper(std::string &result, std::string s, char sep, size_t pos){
    char last = *result.rbegin();
    int n = 0;
    if (result.empty() || last == '\n' || last == '\n'){ 
      result += s;
      if (pos != std::string::npos){
            result += "\n";
            n = 0;
      }
    }else{
      result += sep + s;
      if (sep == '\n')
        n = s.length();
    }
    return n;
}

void process_hyphenation(std::string &result, std::string &word, int &nline, int &nword, int max_width,  Rcpp::Function hyphen){
    char chyphen = '-';
    Rcpp::CharacterVector hyphened_word_vec;
    std::string hyphened_word, syllable;
    int nsyl, hyph;
    size_t pos = std::string::npos;
    nline = nline - nword;
    hyphened_word_vec = hyphen(word);
    hyphened_word = std::string(hyphened_word_vec[0]);

    pos = hyphened_word.find(chyphen);
    if (pos == std::string::npos){ // no hyphen
      if (nline == 0){
        result += word + '\n';
      } else {
        result += '\n' + word;
        nline = nword;
      }
    } else {
      do
      {
        pos = hyphened_word.find(chyphen);
        syllable = hyphened_word.substr(0, pos);
        nsyl = syllable.length();
        hyph = (pos != std::string::npos) ? 1 : 0;
        if (nsyl + nline + hyph > max_width){
          if (nline == 0){ // must add at least something
            result += syllable; 
          } else {
            if (word.substr(0, syllable.length()) == syllable){
              result += '\n' + syllable;
            } else {
              result += "-\n" + syllable;
            }
          } 
          nline = nsyl;
        } else {
          if (word.substr(0, syllable.length()) == syllable && nline != 0){
            result += ' ';
          }
          result += syllable;
          nline += nsyl;
        }
        hyphened_word.erase(0, pos + 1);
      } while (pos != std::string::npos);
    }
}

//[[Rcpp::export]]
std::string splitLine_cpp(std::string str, int max_width, bool use_hyphening, Rcpp::Function hyphen){
  char whitespace = ' ';
  size_t pos;
  std::string result, word;
  int nline = 0, nword;
  
// break line into tokens by whitespace and iterate through those tokens
  do
  {
    pos = str.find(whitespace);
    word = str.substr(0, pos);
    nword = word.length();
    nline = nline + nword;
    if (nline > max_width) {
      if (use_hyphening) {
        process_hyphenation(result, word, nline, nword, max_width, hyphen);
      } else {
        nline = paste_wrapper(result, word, '\n', pos);
      }
    } else{
      paste_wrapper(result, word, ' ', std::string::npos);
    }
    nline += 1;
    str.erase(0, pos + 1);
  } while (pos != std::string::npos);
  
  return result;
}
