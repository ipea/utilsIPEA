

// Algoritmo da Caixa

// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/split.hpp>

using namespace Rcpp;
using namespace boost;
using namespace std;

#define cc         *t
#define nc         *(t + 1)
#define nnc        *(t + 2)
#define nnnc       *(t + 3)
#define NULLCHAR    (char)NULL
#define pc         *(t - 1)


// Define métodos utilizados
bool is(std::string x, char c) {
  return (c != NULLCHAR && x.find_first_of(c) != std::string::npos);
}

string substr(std::string x, int i, int n) {

  try {
    return x.substr(i, n);
  } catch(const out_of_range& e) {
    return "";
  }
}

// int CountWords(const char* str){
//   bool inSpaces = true;
//   int numWords = 0;
//
//   while (*str != NULL){
//     if (std::isspace(*str)){
//       inSpaces = true;
//     }
//     else if (inSpaces){
//       numWords++;
//       inSpaces = false;
//     }
//     ++str;
//   }
//   return numWords;
// }


string caixa_single(string x, int max){

  //Inicia indicadores de posição e variaveis
  string word = "";
  string::iterator t;
  string temp = "";
  int c;

  //Inicia constantes utilizadas
  string alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ ";
  string consonants = "BCDFGHJKLMNPQRSTVWXYZ";
  string soft = "AOU";
  string softer = "EI";
  string ptc = "PTC";
  string vowels = "AEIOUY";
  string RS = "RS";
  string empty = "";

  //Faz tratamentos
  string pre_word = x.substr();
  boost::trim(pre_word);
  boost::to_upper(pre_word);

  //Corre todas as letras e retira itens estranhos
  t = pre_word.begin();
  while(word.length() < max && word.length() < pre_word.length()){
    if(is(alpha, cc)){
      word += cc;
      t += 1;
    } else t += 1;
  }

  //Inicia sequenca de loops para cada regra

  // Casos especiais:


  /* Regra 1: Tratamento para consoantes
   * Retira letras repetidas seguidas
   */
  t = word.begin();
  for (c = 0; c < word.length(); c++){
    if(nc == cc){
      //cout << " Caiu regra 1 \n";
      word.replace(c,1,"");
      t += 1;
    } else t += 1;
  }

  //Regra 28: Troca OA por UA, PH por F, ING por IMG, MN por M, W por V
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'O' && nc == 'A' ){
      //cout << " Caiu regra OA \n";
      word.replace(c,2,"UA");
      t += 2;
      c += 1;
    } else if( cc == 'P' && nc == 'H' ){
      //cout << " Caiu regra PH \n";
      word.replace(c,2,"F");
      t += 2;
      c += 1;
    } else if( cc == 'I' && nc == 'N' && nnc == 'G' ){
      //cout << " Caiu regra ING \n";
      word.replace(c,3,"IMG");
      t += 3;
      c += 2;
    } else if( cc == 'M' && nc == 'N' ){
      //cout << " Caiu regra MN \n";
      word.replace(c,2,"M");
      t += 2;
      c += 1;
    } else if( cc == 'W' ){
      //cout << " Caiu regra W \n";
      word.replace(c,1,"V");
      t += 1;
    } else {
      t += 1;
    }
  }

  //Regra 2: Letra S entre vogais (inclusive Y), ou no final de palavras, troca por Z
  t = word.begin();
  for (c = 0; c < word.length(); c++){
    //cout << "cc " << cc;
    if( (cc == 'S' && is(vowels,pc) && is(vowels,nc)) || (cc == 'S' && nc == ' ') || (cc == 'S' && !is(alpha,nc)) ){
      //cout << " Caiu regra 2 \n";
      word.replace(c,1,"Z");
      t += 1;
    } else t += 1;
  }

  //Regra 3: letra Y trocda por I
  t = word.begin();
  for (c = 0; c < word.length(); c++){
    if( cc == 'Y' ){
      //cout << " Caiu regra 3 \n";
      word.replace(c,1,"I");
      t += 1;
    } else t += 1;
  }

  //Regra 4: Letra N ao final da palavra ou seguida de consoante (tirando o "H"), troca por M
  t = word.begin();
  for (c = 0; c < word.length(); c++){
    if( ((cc == 'N' && nc == ' ') || (cc == 'N' && !is(alpha,nc)) || (cc == 'N' && is(consonants,nc))) && nc != 'H'){
      //cout << " Caiu regra 4 \n";
      word.replace(c,1,"M");
      t += 1;
    } else t += 1;
  }

  /* Regra 5: Vogal seguida de L na mesma silaba, troca L por U
   * obs: atualmente, todas as vogais seguidas de L são observadas
   * Necessário desenvolver método para identificar sílabas.
   * Palavras como Seilandia, será traduzida erroneamente.
   * Para que a Regra 13 funcione, vou adicionar uma regra de nnc != H
   */
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( is(vowels,cc) && nc == 'L' && nnc != 'H'){
      //cout << " Caiu regra 5 \n";
      word.replace(c+1,1,"U");
      t += 2;
      c += 1;
    } else t += 1;
  }

  //Regra 6: Dígrafos XS, KS, CS, CZ, KZ e XZ entre vogais substituídos por KIZ
  /*
   * ERRO: "nobertoxsa" funciona, "nobertoxsa nobertoxsa" funciona,
   * mas "nobertoxsa nobertoxsa nobertoxsa" NÃO funciona.
   */
  t = word.begin();
  for(c = 0; c < word.length();c++){
    if( cc == 'X' || cc == 'K' || cc == 'C' ){
      if(nc == 'S' || nc == 'Z'){
        if(is(vowels,pc) && is(vowels,nnc)){
          //cout << " Caiu regra 6 \n";
          word.replace(c,2,"KIZ");
          t += 3;
          c += 2;
        }
      }
    } else{
      t += 1;
    }
  }

  //Regra 7: Conjunção OEL, seguida de vogal ou ao final da palavra, troca por UEL
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( (cc=='O' && nc=='E' && nnc=='L' && is(vowels,nnnc)) || (cc=='O' && nc=='E' && nnc=='L' && nnnc==' ') || (cc=='O' && nc=='E' && nnc=='L' && !is(alpha,nnnc)) ){
      //cout << " Caiu regra 7 \n";
      word.replace(c,3,"UEL");
      t += 3;
      c += 2;
    } else t += 1;
  }

  //Regra 8: Dígrafo NH, seguido de vogal, troca por NI
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if(cc == 'N' && nc == 'H' && is(vowels,nnc)){
      //cout << " Caiu regra 8 \n";
      word.replace(c,2,"NI");
      t += 2;
      c += 1;
    } else{
      t += 1;
      //c += 1;
    }
  }

  //Regra 9: Junção SCH seguido de vogal ou final da palavra, troca por X
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( (cc == 'S' && nc == 'C' && nnc == 'H' && is(vowels,nnnc)) || (cc == 'S' && nc == 'C' && nnc == 'H' && !is(alpha,nnnc)) ){
      //cout << " Caiu regra 9 \n";
      word.replace(c,3,"X");
      t += 3;
      c += 2;
    } else {
      t += 1;
    }
  }

  //Regra 10: Junção SC, seguido de E ou I no meio da palavra, troca por S
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'S' && nc == 'C' && is("EI",nnc) ){
      //cout << " Caiu regra 10 \n";
      word.replace(c,2,"S");
      t += 2;
      c += 1;
    } else {
      t += 1;
    }
  }

  //Regra 11: Junção ICT seguida de vogal, troca por IT
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'I' && nc == 'C' && nnc == 'T' && is(vowels,nnnc) ){
      //cout << " Caiu regra 11 \n";
      word.replace(c,3,"IT");
      t += 3;
      c += 2;
    } else{
      t += 1;
    }
  }

  //Regra 12: Junção CH no final de palavra, troca por K, no meio da palavra, troca por X
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if ( cc == 'C' && nc == 'H' ){
      if ( nnc == ' ' || !is(alpha,nnc) ){
        //cout << " Caiu regra 12 \n";
        word.replace(c,2,"K");
        t += 2;
        c += 1;
      } else {
        //cout << " Caiu regra 12 \n";
        word.replace(c,2,"X");
        t += 2;
        c += 1;
      }
    } else{
      t += 1;
    }
  }

  //Regra 13: LH seguido de vogal, troca por LI
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'L' && nc == 'H' && is(vowels,nnc) ){
      //cout << " Caiu regra 13 \n";
      word.replace(c,2,"LI");
      t += 2;
      c += 1;
    } else{
      t += 1;
    }
  }

  //Regra 14: DTH troca por DTI
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'D' && nc == 'T' && nnc == 'H' ){
      //cout << " Caiu regra 14 \n";
      word.replace(c,3,"DTI");
      t += 3;
      c += 2;
    } else {
      t += 1;
    }
  }

  //Regra 15: TH no final de palavra, troca por TI, se seguido por vogal, troca por T
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'T' && nc == 'H' ){
      if( nnc == ' ' || !is(alpha,nnc) ){
        //cout << " Caiu regra 15 \n";
        word.replace(c,2,"TI");
        t += 2;
        c += 1;
      } else{
        if( is(vowels,nnc) ){
          //cout << " Caiu regra 15 \n";
          word.replace(c,2,"T");
          t += 2;
          c += 1;
        }
      }
    } else{
      t += 1;
    }
  }

  //Regra 16: Letra G seguida de E ou I, troca por J
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'G' && is("EI",nc) ){
      //cout << " Caiu regra 16 \n";
      word.replace(c,1,"J");
      t += 2;
      c += 1;
    } else{
      t += 1;
    }
  }

  //Regra 17: GU se seguido por E ou I, troca por G
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'G' && nc == 'U' && is("EI",nnc) ){
      //cout << " Caiu regra 17 \n";
      word.replace(c,2,"G");
      t += 2;
      c += 1;
    } else {
      t += 1;
    }
  }

  //Regra 18: QU seguido de A ou O, troca por KU, se seguido de E ou I, troca por K
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'Q' && nc == 'U' ){
      if( nnc == 'A' || nnc == 'O' ){
        //cout << " Caiu regra 18 \n";
        word.replace(c,2,"KU");
        t += 2;
        c += 1;
      } else{
        if( nnc == 'E' || nnc == 'I' ){
          //cout << " Caiu regra 18 \n";
          word.replace(c,2,"K");
          t += 2;
          c += 1;
        }
      }
    } else {
      t += 1;
    }
  }

  /* Regra 19: C seguido de qlqr letra que nao E ou I, troca por K, se seguido de E ou I, troca por S
   * Observação: A regra 25 pede para trocar CK por K. Vou incluir uma excessão à regra 19 para ela.
   */
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'C' ){
      if( is("EI",nc) ){
        //cout << " Caiu regra 19 \n";
        word.replace(c,1,"S");
        t += 1;
      } else {
        if( nc != 'K' ){
          //cout << " Caiu regra 19 \n";
          word.replace(c,1,"K");
          t += 1;
        } else{
          t += 1;
        }
      }
    } else {
      t += 1;
    }
  }

  //Regra 20: A letra H, caso nao fizer parte dos digrafos NH, CH ou LH, será suprimida
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'H' && pc != 'N' && pc != 'C' && pc != 'L' ){
      //cout << " Caiu regra 20 \n";
      word.replace(c,1,"");
      t += 1;
    } else{
      t += 1;
    }
  }

  /* Regra 21: Letras D, F, P, B e T seguidas de consoantes ou final das palavras, mantem e acresce o I
   * Vai uma consideração: A palavra EMPRESA cai nessa regra (EMPIREZA) mas nao me parece correto...
   * FRANCISCO se torna FIRAMSISKO.
   */
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( (is("DFPBT",cc) && is(consonants,nc)) || (is("DFPBT",cc) && (nc == ' ' || !is(alpha,nc))) ){
      //cout << " Caiu regra 21 \n";
      temp = word[c];
      temp.append("I");
      word.replace(c,1,temp);
      t += 2;
      c += 1;
    } else {
      t += 1;
    }
  }

  // Regra 22: X ao final das palavras, troca por KIZ, se depois de uma vogal, NA MESMA SILABA, TROCA POR Z
  /* Mesmo problema da Regra 5.
   * Necessário identificar silabas!
   */
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'X' ){
      if( nc == ' ' || !is(alpha,nc) ){
        //cout << " Caiu regra 22 \n";
        word.replace(c,1,"KIZ");
        t += 1;
      }
      // else{
      //   if( is(vowels,pc) ){
      //     word.replace(c,1,"Z");
      //     t += 1;
      //   }
      // }
    } else{
      t += 1;
    }
  }

  /* Regra 23: ES no inicio ou meio da palavra NA MESMA SILABA, e seguida de uma consoante, troca por IS
   * Se seguido de vogal ou branco, troca por IZ (no entanto, a Regra 2 diz que S entre vogais ou no final de palavras,
   * troca por Z, então nao acho que essa regra vai cair alguma vez.)
   */
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'E' && nc == 'S' ){
      if( is(vowels,nnc) || nnc == ' ' || !is(alpha,nnc) ){
        //cout << " Caiu regra 23 \n";
        word.replace(c,2,"IZ");
        t += 2;
        c += 1;
      }
      // else{
      //   if( !is(alpha,pc) || pc == ' ' ){
      //     //adicionar regra para mesma silaba
      //     word.replace(c,2,"IS");
      //     t += 2;
      //     c += 1;
      //   }
      // }
    } else{
      t += 1;
    }
  }

  //Regra 24: letra E seguida de qualquer consoante ao final da palavra, é substituida por I.
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'E' && is(consonants,nc) && (nnc == ' ' || !is(alpha,nnc)) ){
      //cout << " Caiu regra 24 \n";
      word.replace(c,1,"I");
      t += 1;
    } else {
      t += 1;
    }
  }

  /* Regra 25: CK troca por K
   * Observação: a regra 19 diz que C seguido dequalquer letra que nao E ou I, troca por K.
   * CK cai dentro desta regra. Vou incluir uma excessão na regra 19 para isso.
   */
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'C' && nc == 'K' ){
      //cout << " Caiu regra 25 \n";
      word.replace(c,2,"K");
      t += 2;
      c += 1;
    } else {
      t += 1;
    }
  }

  // Regra 26: ST no inicio da palavra, troca por IST
  t = word.begin();
  for(c = 0; c < word.length(); c++){
    if( cc == 'S' && nc == 'T' && (pc == ' ' || !is(alpha,pc)) ){
      //cout << " Caiu regra 26 \n";
      word.replace(c,2,"IST");
      t += 2;
      c += 1;
    } else{
      t += 1;
    }
  }

  return word;
}



//' @rdname caixa
//' @name caixa
//' @title Generate phonetic versions of strings with Caixa's method
//'
//' @description
//' The function \code{caixa} phonentically encodes the given input
//' using algorithm developed by Caixa Economica Federal.
//'
//' @param word string or vector of strings to encode
//' @param max maximum length of the resulting encodings, in characters
//'
//' @details To be inserted
//'
//' @return a character vector containing the phonetical output of
//' \code{word}, or an NA if the \code{word} value is NA
//'
//' @family utilsIPEA
//'
//' @examples
//' caixa("")
//' caixa(c("school", "benji"))
//'
//' @useDynLib utilsIPEA
//' @importFrom Rcpp evalCpp
//' @export
//[[Rcpp::export]]
CharacterVector caixa_(CharacterVector word, int max = 20){

  unsigned int input_size = word.size();
  CharacterVector res(input_size);

  for(unsigned int i = 0; i < input_size; i++){
    if((i % 10000) == 0){
      Rcpp::checkUserInterrupt();
    }
    if(word[i] == NA_STRING){
      res[i] = NA_STRING;
    } else {
      res[i] = caixa_single(Rcpp::as<std::string>(word[i]), max);
    }
  }

  return res;
}


