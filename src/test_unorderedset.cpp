#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]
#include <unordered_set>
using namespace Rcpp;


// template <typename WHAT>
// class ListOf : public List {
// public:
//   template <typename T>
//   ListOf( const T& x) : List(x){}
//   WHAT operator[](int i){ return as<WHAT>( ( (List*)this)->operator[]( i) ) ; }
//   
// };

// [[Rcpp::export]]
CharacterVector set_std(CharacterVector tokens,
                                CharacterVector types){

  std::unordered_set<String> set_types (types.begin(), types.end());
  for(int i=0; i < tokens.size(); i++){
    String token = tokens[i];
    bool is_in = set_types.find(token) != set_types.end();
    if(is_in == true){
      tokens[i] = "";
    }
  }
  
  return tokens;
}


// [[Rcpp::export]]
CharacterVector set_rcpp(CharacterVector tokens,
                        CharacterVector types){
  
  RCPP_UNORDERED_SET<String> set_types (types.begin(), types.end());
  for(int i=0; i < tokens.size(); i++){
    String token = tokens[i];
    bool is_in = set_types.find(token) != set_types.end();
    if(is_in == true){
      tokens[i] = "";
    }
  }
  
  return tokens;
}

/*** R
tokens <- rep(letters, 100)
microbenchmark::microbenchmark(
  set_std(tokens, c('a', 'c', 'd')),
  set_rcpp(tokens, c('a', 'c', 'd'))
)
*/
