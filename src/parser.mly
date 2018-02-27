%token Null
%token <bool> Bool
%token <string> String
%token <float> Float
%token <int> Int
%token <string> Name
%token As
%token Ae
%token Os
%token Oe
%token Comma
%token Colon
%token Eof
%token Infinity
%token Neg_infinity
%token Nan

%start <Json.value option> prog
%%
