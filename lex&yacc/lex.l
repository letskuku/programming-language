%{
/* 정의절 */
#include <stdio.h>
#include "y.tab.h"
%}

/* 변수 선언 */
D		[0-9]
L		[a-zA-Z_]
H		[a-fA-F0-9]
E		[Ee][+-]?{D}+
FS		(f|F|l|L)
IS		(u|U|l|L)*
%%
    /* 규칙절 */
    
    /* 주석 처리 */
"/*"		{comment();}
"//".*  {}

    /* 키워드 처리 */
"auto"		{return AUTO;}
"break"		{return BREAK;}
"case"		{return CASE;}
"char"		{return CHAR;}
"const"		{return CONST;}
"continue"	{return CONTINUE;}
"default"	{return DEFAULT;}
"do"		{return DO;}
"double"	{return DOUBLE;}
"else"		{return ELSE;}
"enum"		{return ENUM;}
"extern"	{return EXTERN;}
"float"		{return FLOAT;}
"for"		{return FOR;}
"goto"		{return GOTO;}
"if"		{return IF;}
"int"		{return INT;}
"long"		{return LONG;}
"register"	{return REGISTER;}
"return"	{return RETURN;}
"short"		{return SHORT;}
"signed"	{return SIGNED;}
"sizeof"	{return SIZEOF;}
"static"	{return STATIC;}
"struct"	{return STRUCT;}
"switch"	{return SWITCH;}
"typedef"	{return TYPEDEF;}
"union"		{return UNION;}
"unsigned"	{return UNSIGNED;}
"void"		{return VOID;}
"volatile"	{return VOLATILE;}
"while"		{return WHILE;}

    /* 전처리기 관련 단어 처리 */
"define"  {return DEFINE;}
"include"  {return INCLUDE;}

    /* identifier, type_name 처리 */
{L}({L}|{D})*	{return check_type();}

    /* 헤더파일 처리 */
{L}+"."h  {return HEADER;}

    /* 상수 처리 */
0[xX]{H}+{IS}? {return CONSTANT;}
0{D}+{IS}? {return CONSTANT;}
{D}+{IS}? {return CONSTANT;}
L?'(\\.|[^\\'])+' {return CONSTANT;}
{D}+{E}{FS}? {return CONSTANT;}
{D}*"."{D}+({E})?{FS}? {return CONSTANT;}
{D}+"."{D}*({E})?{FS}? {return CONSTANT;}

    /* 문자열 처리 */
L?\"(\\.|[^\\"])*\" {return STRING_LITERAL;}

    /* 연산자 및 기타 기호 처리 */
"..." {return(ELLIPSIS);}
">>=" {return(RIGHT_ASSIGN);}
"<<=" {return(LEFT_ASSIGN);}
"+=" {return(ADD_ASSIGN);}
"-=" {return(SUB_ASSIGN);}
"*=" {return(MUL_ASSIGN);}
"/=" {return(DIV_ASSIGN);}
"%=" {return(MOD_ASSIGN);}
"&=" {return(AND_ASSIGN);}
"^=" {return(XOR_ASSIGN);}
"|=" {return(OR_ASSIGN);}
">>" {return(RIGHT_OP);}
"<<" {return(LEFT_OP);}
"++" {return(INC_OP);}
"--" {return(DEC_OP);}
"->" {return(PTR_OP);}
"&&" {return(AND_OP);}
"||" {return(OR_OP);}
"<=" {return(LE_OP);}
">=" {return(GE_OP);}
"==" {return(EQ_OP);}
"!=" {return(NE_OP);}
";" {return(';');}
("{"|"<%") {return('{');}
("}"|"%>") {return('}');}
"," {return(',');}
":" {return(':');}
"=" {return('=');}
"(" {return('(');}
")" {return(')');}
("["|"<:") {return('[');}
("]"|":>") {return(']');}
"." {return('.');}
"&" {return('&');}
"!" {return('!');}
"~" {return('~');}
"-" {return('-');}
"+" {return('+');}
"*" {return('*');}
"/" {return('/');}
"%" {return('%');}
"<" {return('<');}
">" {return('>');}
"^" {return('^');}
"|" {return('|');}
"?" {return('?');}
"#" {return('#');}
[ \t\v\n\f] {}
. { /* ignore bad characters */ }
%%

/* 서브루틴절 */

/* yylex 정상종료 도와주는 함수 */
int yywrap() {
  return(1);
}

/* 문단 단위 주석 처리 도와주는 함수 */
comment()
{
	char c, c1;

loop:
	while ((c = input()) != '*' && c != 0)
		;
   
	if ((c1 = input()) != '/' && c != 0)
	{
		unput(c1);
		goto loop;
	}
}

/* identifier, type_name 처리 도와주는 함수 */
int check_type()
{
  /*
  if (yytext == type_name)
    return(TYPE_NAME);
  */

  return(IDENTIFIER);
}
