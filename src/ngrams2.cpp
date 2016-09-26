#include <Rcpp.h>
#include <string>
#include <algorithm>
// [[Rcpp::plugins(cpp11)]]
#include <unordered_set>

using namespace Rcpp;

String join2(std::vector< std::string > ngram, 
             std::string delim){
    if(ngram.size() == 0) return "";
    String token_ngram = ngram[0];
    int len_ngram = ngram.size();
    for (int i = 1; i < len_ngram; i++) {
        token_ngram += delim;
        token_ngram += ngram[i];
    }
    token_ngram.set_encoding(CE_UTF8);
    return token_ngram;
}

// [[Rcpp::export]]
void skip(const std::vector<std::string> &tokens,
          const unsigned int start,
          const unsigned int n, 
          const std::vector<int> skips,
          std::vector<std::string> ngram,
          std::vector< std::vector<std::string> > &ngrams,
          int e, int &f
){
    
    //Rcout << "Start " << tokens[start] << " " << start << " " << n << "\n";
    
    ngram[e] = tokens[start];
    e++;
    
    if(e < n){
        for (int j = 0; j < skips.size(); j++){
            int next = start + skips[j];
            if(next < 0 || tokens.size() - 1 < next) break;
            //Rcout << "Join " << join2(ngram, "+").get_cstring() << " " << e << " " << next << "\n";
            skip(tokens, next, n, skips, ngram, ngrams, e, f);
        }
    }else{
        //Rcout << "Add " << join2(ngram, "+").get_cstring() << " " << f << "/" << ngrams.size() << "\n";
        ngrams[f] = ngram;
        //ngram.clear();
        e = 0;
        f++;
    }
}



// [[Rcpp::export]]
CharacterVector skipgram_cpp2(const std::vector <std::string> &tokens,
                              const std::vector <int> &ns, 
                              const std::vector <int> &skips, 
                              const std::string &delim) {
    
    // Generate skipgrams recursively
    int len_ns = ns.size();
    int len_skips = skips.size();
    int len_tokens = tokens.size();
    int e = 0; // Local index for tokens in ngram
    int f = 0; // Global index for ngrams 
    std::vector< std::vector<std::string> > ngrams(std::pow(len_ns * len_tokens, len_skips)); // For the recursive function
    //Rcout << "Allocate " << ngrams.size() << "\n";
    
    for (int g = 0; g < len_ns; g++) {
        int n = ns[g];
        std::vector<std::string> ngram(n);
        for (int start = 0; start < len_tokens - (n - 1); start++) {
          skip(tokens, start, n, skips, ngram, ngrams, e, f); // Get ngrams as reference
        }
    }
    
    // Join elements of ngrams
    CharacterVector tokens_ngram(f);
    if(f > 0){
      for (int k = 0; k < f; k++) {
          tokens_ngram[k] = join2(ngrams[k], delim);
      }
      return tokens_ngram[seq(0, f - 1)];
    }else{
      return tokens_ngram;
    }
}

/*** R
#toks <- rep(LETTERS, 20)
#microbenchmark::microbenchmark(skipgramcpp(toks, 1:2, 0:1, "_"), 
#                               skipgram_cpp2(toks, 1:2, 0:1, "_"))
*/


// [[Rcpp::export]]
List skipgram_cppl2(SEXP x, 
                    const std::vector <int> &ns, 
                    const std::vector <int> &skips, 
                    const std::string &delim) {
  
  List texts(x);
  int len = texts.size();
  List texts_skipgram(len);
  for (int h = 0; h < len; h++){
    texts_skipgram[h] = skipgram_cpp2(texts[h], ns, skips, delim);
  }
  return texts_skipgram;
}

// [[Rcpp::export]]
List bigram_selective_cppl(SEXP x,
                           const std::vector<std::string> &types,
                           const std::vector<std::string> &types_stop,
                           const std::vector<std::string> &types_ignore,
                           const std::vector<int> &skips, 
                           const std::string &delim,
                           const std::string &token_sub
) {
  
  List texts(x);
  List texts_temp(texts.size());
  int len = texts.size();
  std::unordered_set<std::string> set_types (types.begin(), types.end());
  std::unordered_set<std::string> set_types_stop (types_stop.begin(), types_stop.end());
  std::unordered_set<std::string> set_types_ignore (types_ignore.begin(), types_ignore.end());
  for (int h = 0; h < len; h++){
    //Rcout << "Text " <<  h << "\n";
    CharacterVector text = texts[h];
    CharacterVector text_temp = clone(text);
    int len = text.size();
    int len_skips = skips.size();
    for (int i=0; i < len; i++){
      //Rcout << "Now " << text[i] << " " << i << "\n";
      String token = text[i];
      bool is_in = set_types.find(token) != set_types.end();
      if(token_sub != "") token = token_sub; // substitute by arbitary token
      if(is_in){
        int k;
        //Rcout << "Match " << text[i] << " " << i << "\n";
        for (int j=0; j < len_skips; j++){
          //Rcout << "Skip " << skips[j] << "\n";
          k = i + skips[j];
          if(k < 0 || k > len - 1) break; // only within the length of text
          String token2 = text[k];
          
          // Interrupt
          bool is_in_stop = set_types_stop.find(token2) != set_types_stop.end();
          if(is_in_stop) break;
          
          // Skip
          bool is_in_ignore = set_types_ignore.find(token2) != set_types_ignore.end();
          if(is_in_ignore) continue;
          
          // JOin tokens
          String token_bigram;
          if(k < i){
            //Rcout << "Join left " << text[k] << " " << k << "\n";
            token_bigram = token2;
            token_bigram += delim;
            token_bigram += token;
          }else if(i < k){
            //Rcout << "Join right " << text[k] << " " << k << "\n";
            token_bigram =  token;
            token_bigram += delim;
            token_bigram += token2;
          }
          //token_bigram.set_encoding(CE_UTF8); // This causes crash somethimes 
          text_temp[k] = token_bigram;
        }
      }
    }
    texts_temp[h] = text_temp;
  }
  return texts_temp;
}
