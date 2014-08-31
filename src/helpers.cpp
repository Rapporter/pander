#include <Rcpp.h>
#include <string>

using namespace std;

// wrapper around concatenation. Based on string given decides of line break is needed and how to insert a separator
// pos is a check of paste_wrapper is called in the end of tokenization
void paste_wrapper(std::string& result, std::string& s, char sep, size_t pos) {
  char last = *result.rbegin(); // last character in result
  if (result.empty() || last == '\n'){
    result += s;
    result = (pos != std::string::npos) ? result + '\n' : result;
  } else {
    result += sep + s;
  }
}

// helper to process word hyphenation. Hyphenates word and adds it to the result also respectively changing line width
void process_hyphenation(std::string &result, std::string &word, int &lline, int max_width,  Rcpp::Function hyphen) {
  char chyphen = '-';
  Rcpp::CharacterVector hyphened_word_vec;
  std::string hyphened_word, syllable;
  int lsyl, hyph, lword = word.length(); // l is short for length (syl length and word length)
  size_t pos = std::string::npos;

  // roll line length back and hyphenate word
  lline = lline - lword;
  hyphened_word_vec = hyphen(word);
  hyphened_word = std::string(hyphened_word_vec[0]);

  // start tokenization by hyphens
  pos = hyphened_word.find(chyphen);
  if (pos == std::string::npos){ // no hyphenation is possible
    if (lline == 0){ // need to and something to an empty line
      result += word + '\n';
    } else { // add word at the next line
      result += '\n' + word;
      lline = lword;
    }
  } else {
    do {
      pos = hyphened_word.find(chyphen);
      syllable = hyphened_word.substr(0, pos);
      lsyl = syllable.length();
      hyph = (pos != std::string::npos) ? 1 : 0; // check if hyphen will be needed (in case of end - no, yes in other case)
      if (lsyl + lline + hyph > max_width){
        if (lline == 0) // must add at least something
          result += syllable;
        else // if syllable is last possible syllable no hyphen is needed, in other case add hyphen
          result = (word.substr(0, syllable.length()) == syllable) ? (result + '\n' + syllable) : (result + "-\n" + syllable);
        lline = lsyl;
      } else {
        // if syllable is last possible syllable and line is not empty than we need a whitespace
        result = (word.substr(0, syllable.length()) == syllable && lline != 0) ? result + ' ' : result;
        result += syllable;
        lline += lsyl;
      }
      hyphened_word.erase(0, pos + 1);
    } while (pos != std::string::npos);
  }
}

//[[Rcpp::export]]
std::string splitLine_cpp(std::string str, int max_width, bool use_hyphening, Rcpp::Function hyphen) {
  char whitespace = ' ';
  size_t pos;
  std::string result, word;
  int lline = 0, lword; // l is short for length (line length and word length)

  // break line into tokens by whitespace and iterate through those tokens
  do {
    pos = str.find(whitespace);
    word = str.substr(0, pos);
    lword = word.length();
    lline = lline + lword;
    if (lline > max_width) {
      if (!use_hyphening) {
        paste_wrapper(result, word, '\n', pos);
        lline = word.length();
      } else process_hyphenation(result, word, lline, max_width, hyphen);
    } else{
      paste_wrapper(result, word, ' ', std::string::npos);
    }
    lline += 1;
    str.erase(0, pos + 1);
  } while (pos != std::string::npos);

  return result;
}
