%{
	int bad_input = 1;
%}
/*** Definitions ***/ 

VALUEI 			[0-9]|[1-9][0-9]*
VALUEF 			[1-9]+[0-9]*f+[1-9]+[0-9]*
ID				[a-zA-Z][a-zA-Z0-9]*
VALUESTR 		["](.*?)["]
COMMENT			";;".*
OP_PLUS			"+"
OP_MINUS		"-"
OP_DIV			"/"
OP_MULT			"*"
OP_OP 			"\("
OP_CP			"\)"
OP_DBLMULT		"**"
OP_OC 			"“"
OP_CC 			"”"
OP_COMMA		","
KW_AND 			"and"
KW_OR 			"or"
KW_NOT  		"not"
KW_EQUAL  		"equal"
KW_LESS 		"less"
KW_NIL 			"nil"
KW_LIST  		"list"
KW_APPEND  		"append"
KW_CONCAT 		"concat"
KW_SET 			"set"
KW_DEFFUN  		"deffun"
KW_FOR  		"for"
KW_IF  			"if"
KW_EXIT  		"exit"
KW_LOAD 		"load"
KW_DISP 		"disp"
KW_TRUE 		"true"
KW_FALSE	 	"false"

NEWLINE			"n"

/*** Rule Section ***/
%%

(\r\n|\r|\n) {return 0;}
{KW_AND}	{printf("%s: KW_AND\n", yytext);}

{KW_OR}		{printf("%s: KW_OR\n", yytext);}
{KW_NOT}	{printf("%s: KW_NOT\n", yytext);}
{KW_EQUAL}	{printf("%s: KW_EQUAL\n", yytext);}
{KW_LESS}	{printf("%s: KW_LESS\n", yytext);}
{KW_NIL}	{printf("%s: KW_NIL\n", yytext);}
{KW_LIST}	{printf("%s: KW_LIST\n", yytext);}
{KW_APPEND}	{printf("%s: KW_APPEND\n", yytext);}
{KW_CONCAT}	{printf("%s: KW_CONCAT\n", yytext);}
{KW_SET}	{printf("%s: KW_SET\n", yytext);}
{KW_DEFFUN}	{printf("%s: KW_DEFFUN\n", yytext);}
{KW_FOR}	{printf("%s: KW_FOR\n", yytext);}
{KW_IF}		{printf("%s: KW_IF\n", yytext);}
{KW_EXIT}	{printf("%s: KW_EXIT\n", yytext);}
{KW_LOAD}	{printf("%s: KW_LOAD\n", yytext);}
{KW_DISP}	{printf("%s: KW_DISP\n", yytext);}
{KW_TRUE}	{printf("%s: KW_TRUE\n", yytext);}
{KW_FALSE}	{printf("%s: KW_FALSE\n", yytext);}

{OP_PLUS}		{printf("%s: OP_PLUS\n", yytext);}
{OP_MINUS}		{printf("%s: OP_MINUS\n", yytext);}
{OP_DIV}		{printf("%s: OP_DIV\n", yytext);}
{OP_MULT}		{printf("%s: OP_MULT\n", yytext);}
{OP_CP}			{printf("%s: OP_CP\n", yytext);}
{OP_OP}			{printf("%s: OP_OP\n", yytext);}
{OP_DBLMULT}	{printf("%s: OP_DBLMULT\n", yytext);}
{OP_OC} 		{printf("%s: OP_OC\n", yytext);}
{OP_CC} 		{printf("%s: OP_CC\n", yytext);}
{OP_COMMA}		{printf("%s: OP_COMMA\n", yytext);}
{VALUESTR}		{
	printf("\" : OP_OC\n%s: VALUESTR\n\" : OP_CC\n", yytext);}
[[:space:]]+ 		/* ignore space */
{COMMENT}		{printf("%s: COMMENT\n", yytext);}


[[:alpha:]][[:alnum:]]*   {printf("%s: IDENTIFIER\n", yytext);}
[[1-9][:digit:]]+         {printf("%s: VALUEI\n", yytext);}
[1-9]+[0-9]*f+[1-9]+[0-9]* {printf("%s: VALUEF\n", yytext);}

{VALUEI}					  {printf("%s: VALUEI\n", yytext);}

	
[a-zA-Z]*[,+-/*]	 	  	  {printf("%s: SYNTAX ERROR\n", yytext); return 0;} 
[,][A-zA-Z0-9+-/*,]*	 	  {printf("%s: SYNTAX ERROR\n", yytext); return 0;}
[+][A-zA-Z0-9,-/*+]*	 	  {printf("%s: SYNTAX ERROR\n", yytext); return 0;}	
[-][A-zA-Z0-9+,/*-]*	 	  {printf("%s: SYNTAX ERROR\n", yytext); return 0;}	
[/][A-zA-Z0-9+-,*/]*	 	  {printf("%s: SYNTAX ERROR\n", yytext); return 0;}
[*][A-zA-Z0-9+-,/*]*	 	  {printf("%s: SYNTAX ERROR\n", yytext); return 0;}
[[:digit:]]+[[:alpha:]]  	  {printf("%s: SYNTAX ERROR\n", yytext); return 0;}
[[:digit:]]+[,+-/*]   	 	  {printf("%s: SYNTAX ERROR\n", yytext); return 0;}
[0][0-9]*				 	  {printf("%s: SYNTAX ERROR\n", yytext); return 0;}                       	
.		 				 	  {printf("%s: SYNTAX ERROR\n", yytext); return 0;}	
%%

int yywrap(){}
int main(int argc, char **argv){
	++argv, --argc;

	if(argc > 0){ 
		yyin = fopen(argv[0], "r");
		yylex();
	}
	else{	
		yyin = stdin;

		printf(">>> \n");
		while(1) {
			yylex();
		} 
	}

	
	
	return 0;
}
