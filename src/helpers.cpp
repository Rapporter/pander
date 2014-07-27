#include <Rcpp.h>
#include <string>
#include <cstdio> 
#include <cstdlib>

using namespace std;

int paste_wrapper(std::string &result, std::string s, char sep, char* pch_word){
    char last = *result.rbegin();
    int n = 0;
    if (result.empty() || last == '\n' || last == '\n'){ 
      result += s;
      if (pch_word != NULL){
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
    char *pch_syl, *cword, *end_syllable;
    Rcpp::CharacterVector hyphened_word_vec;
    std::string hyphened_word, syllable;
    int nsyl;
    
    nline = nline - nword;
    hyphened_word_vec = hyphen(word);
    hyphened_word = std::string(hyphened_word_vec[0]);
    cword = new char[hyphened_word.length() + 1];
    strcpy(cword, hyphened_word.c_str());
    pch_syl = strtok_r(cword, "-", &end_syllable);
    if (hyphened_word == word){ // no hyphen
      if (nline == 1){
        result += word + '\n';
      } else {
        result += '\n' + word;
        nline = nword;
      }
    } else {
      while (pch_syl != NULL){
        syllable = std::string(pch_syl);
        nsyl = syllable.length();
        if (nsyl + nline + 1 > max_width){
          if (nline == 1){ // must add at least something
            result += syllable; 
          } else {
            if (pch_syl == cword){
              result += '\n' + syllable;
            } else {
              result += "-\n" + syllable;
            }
          } 
          nline = nsyl;
        } else {
          if (pch_syl == cword && nline != 1){
            result += ' ';
          }
          result += syllable;
          nline += nsyl;
        }
        pch_syl = strtok_r(NULL, "-", &end_syllable);
      }
    }
    delete []cword;
}

//[[Rcpp::export]]
std::string splitLine_cpp(std::string str, int max_width, bool use_hyphening, Rcpp::Function hyphen){
  char *pch_word, *cstr, *end_word;
  std::string result, word, hyphened_word, syllable;
  int nline = 0, nword;
  cstr = new char[str.length() + 1];
  strcpy(cstr, str.c_str());
  // break line into tokens by whitespace and iterate through those tokens
  pch_word = strtok_r(cstr," ", &end_word);
  while (pch_word != NULL)
  {
    word = std::string(pch_word);
    pch_word = strtok_r(NULL, " ", &end_word);
    nword = word.length();
    nline = nline + nword + 1;
    if (nline - 1 > max_width) {
      if (use_hyphening) {
        process_hyphenation(result, word, nline, nword, max_width, hyphen);
      } else {
        nline = paste_wrapper(result, word, '\n', pch_word);
      }
    } else{
      paste_wrapper(result, word, ' ', NULL);
    }
  }
  delete [] cstr;
  return result;
}
