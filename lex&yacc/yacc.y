%{
/* 정의절 */
#include <stdio.h> //출력 위한 헤더파일
int yylex(); //스캐너 함수

/* 다중포인터 관리하는 변수 */
int ptr = 0;
int mpNum = 0;

/* int, char 카운트 관리하는 변수 */
int intDec = 0;
int intVar = 0;
int charDec = 0;
int charVar = 0;

/* int형 포인터, char형 포인터 개수 관리하는 변수 */
int intPtr = 0;
int charPtr = 0;

int ptrFun = 0; //포인터 함수 생성 여부 표시할 변수

/* 
int, char, 포인터가 알맞게 카운트되도록 돕는 함수
check : 변수 여러 개를 한 번에 선언하거나 다중포인터, 포인터 함수, 
함수 전방선언 사용 시 포인터, int, char가 올바르게 카운트되도록 
돕는 함수
check2 : 구조체에서 포인터, int, char가 올바르게 카운트되도록 
돕는 함수
count : int, char, 포인터 개수 세기를 돕는 함수 
cancel : sizeof, cast operator, 함수 정의부의 반환형에서 int, char가 
카운트된 것을 다시 되돌리는 함수
*/
void check(int num1, int num2);
void check2(int num1, int num2);
void count(int num);
void cancel(int num);

/* 
앞에서부터 순서대로 함수, 연산자, int, char, 포인터, 배열, 선택문,
반복문, 리턴문 개수를 저장하는 배열
*/
int ary[9] = {0,0,0,0,0,0,0,0,0};
%}

/* yylex 함수로 받을 토큰 */
%token DEFINE HEADER INCLUDE
%token IDENTIFIER CONSTANT STRING_LITERAL SIZEOF
%token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP 
%token LE_OP GE_OP EQ_OP NE_OP
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN TYPE_NAME
%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE 
%token CONST VOLATILE VOID
%token STRUCT UNION ENUM ELLIPSIS
%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO 
%token CONTINUE BREAK RETURN

/* 문법 시작 부분 */
%start translation_unit
%%

primary_expression
    : IDENTIFIER
    | CONSTANT
    | STRING_LITERAL
    | '(' expression ')'
    ;

postfix_expression
    : primary_expression
    | postfix_expression '[' expression ']'
    | postfix_expression '(' ')' 
    {ary[0]++;} //함수 사용으로 인해 함수 카운트 증가
    | postfix_expression '(' argument_expression_list ')' 
    {ary[0]++;} //함수 사용으로 인해 함수 카운트 증가
    | postfix_expression '.' IDENTIFIER 
    {ary[1]++;} //참조 연산자 사용으로 인해 연산자 카운트 증가
    | postfix_expression PTR_OP IDENTIFIER 
    {ary[1]++;} //참조 연산자 사용으로 인해 연산자 카운트 증가
    | postfix_expression INC_OP 
    {ary[1]++;} //증가 연산자 사용으로 인해 연산자 카운트 증가
    | postfix_expression DEC_OP 
    {ary[1]++;} //감소 연산자 사용으로 인해 연산자 카운트 증가
    ;

argument_expression_list
    : assignment_expression
    | argument_expression_list ',' assignment_expression
    ;

unary_expression
    : postfix_expression
    | INC_OP unary_expression 
    {ary[1]++;} //증가 연산자 사용으로 인해 연산자 카운트 증가
    | DEC_OP unary_expression 
    {ary[1]++;} //감소 연산자 사용으로 인해 연산자 카운트 증가
    | unary_operator cast_expression
    | SIZEOF unary_expression
    | SIZEOF '(' type_name ')' {cancel($3);} 
    /*
    sizeof(int) 또는 sizeof(char) 사용 시 cancel 함수 통해
    int 또는 char 카운트 1 감소
    */
    ;
    
unary_operator
    : '&'
    | '*'
    | '+'
    | '-'
    | '~'
    | '!'
    ;
    
cast_expression
    : unary_expression
    | '(' type_name ')' cast_expression {cancel($2); ary[1]++;} 
    /*
    1) (int) cast_expression, (char) cast_expression 사용 시 
    cancel 함수 통해 int 또는 char 카운트 1 감소
    2) cast 연산자 사용으로 인해 연산자 카운트 증가
    */
    ;
    
multiplicative_expression
    : cast_expression
    | multiplicative_expression '*' cast_expression 
    {ary[1]++;} //산술 연산자 사용으로 인해 연산자 카운트 증가
    | multiplicative_expression '/' cast_expression 
    {ary[1]++;} //산술 연산자 사용으로 인해 연산자 카운트 증가
    | multiplicative_expression '%' cast_expression 
    {ary[1]++;} //산술 연산자 사용으로 인해 연산자 카운트 증가
    ;
    
additive_expression
    : multiplicative_expression
    | additive_expression '+' multiplicative_expression 
    {ary[1]++;} //산술 연산자 사용으로 인해 연산자 카운트 증가
    | additive_expression '-' multiplicative_expression 
    {ary[1]++;} //산술 연산자 사용으로 인해 연산자 카운트 증가
    ;
    
shift_expression
    : additive_expression
    | shift_expression LEFT_OP additive_expression 
    {ary[1]++;} //비트 연산자 사용으로 인해 연산자 카운트 증가
    | shift_expression RIGHT_OP additive_expression 
    {ary[1]++;} //비트 연산자 사용으로 인해 연산자 카운트 증가
    ;
    
relational_expression
    : shift_expression
    | relational_expression '<' shift_expression 
    {ary[1]++;} //논리 연산자 사용으로 인해 연산자 카운트 증가
    | relational_expression '>' shift_expression 
    {ary[1]++;} //논리 연산자 사용으로 인해 연산자 카운트 증가
    | relational_expression LE_OP shift_expression 
    {ary[1]++;} //논리 연산자 사용으로 인해 연산자 카운트 증가
    | relational_expression GE_OP shift_expression 
    {ary[1]++;} //논리 연산자 사용으로 인해 연산자 카운트 증가
    ;
    
equality_expression
    : relational_expression
    | equality_expression EQ_OP relational_expression 
    {ary[1]++;} //논리 연산자 사용으로 인해 연산자 카운트 증가
    | equality_expression NE_OP relational_expression 
    {ary[1]++;} //논리 연산자 사용으로 인해 연산자 카운트 증가
    ;

and_expression
    : equality_expression
    | and_expression '&' equality_expression 
    {ary[1]++;} //비트 연산자 사용으로 인해 연산자 카운트 증가
    ;
    
exclusive_or_expression
    : and_expression
    | exclusive_or_expression '^' and_expression 
    {ary[1]++;} //비트 연산자 사용으로 인해 연산자 카운트 증가
    ;
    
inclusive_or_expression
    : exclusive_or_expression
    | inclusive_or_expression '|' exclusive_or_expression 
    {ary[1]++;} //비트 연산자 사용으로 인해 연산자 카운트 증가
    ;

logical_and_expression
    : inclusive_or_expression
    | logical_and_expression AND_OP inclusive_or_expression 
    {ary[1]++;} //관계 연산자 사용으로 인해 연산자 카운트 증가
    ;
    
logical_or_expression
    : logical_and_expression
    | logical_or_expression OR_OP logical_and_expression 
    {ary[1]++;} //관계 연산자 사용으로 인해 연산자 카운트 증가
    ;
    
conditional_expression
    : logical_or_expression
    | logical_or_expression '?' expression 
    ':' conditional_expression
    ;

assignment_expression
    : conditional_expression
    | unary_expression assignment_operator assignment_expression
    ;
    
assignment_operator
    : '=' {ary[1]++;}
    | MUL_ASSIGN {ary[1]++;}
    | DIV_ASSIGN {ary[1]++;}
    | MOD_ASSIGN {ary[1]++;}
    | ADD_ASSIGN {ary[1]++;}
    | SUB_ASSIGN {ary[1]++;}
    | LEFT_ASSIGN {ary[1]++;}
    | RIGHT_ASSIGN {ary[1]++;}
    | AND_ASSIGN {ary[1]++;}
    | XOR_ASSIGN {ary[1]++;}
    | OR_ASSIGN {ary[1]++;}
    ; //대입 연산자 사용으로 인해 연산자 카운트 증가
    
expression
    : assignment_expression
    | expression ',' assignment_expression
    ;
    
constant_expression
    : conditional_expression
    ;

declaration
    : declaration_specifiers ';'
    | declaration_specifiers init_declarator_list ';' 
    {check($1, $2);}
    /*
    다수의 변수를 한 번에 선언하거나 함수 선언부의 반환형, 함수 
    
    선언부의 파라미터에 int, char가 사용된 경우 알맞게 개수를 
    
    카운트해주기 위해 check 함수 호출
    */
    ;
    
declaration_specifiers
    : storage_class_specifier {$$ = 0;}
    | storage_class_specifier declaration_specifiers {$$ = 0;}
    | type_specifier {$$ = $1;}
    | type_specifier declaration_specifiers {$$ = $1;}
    | type_qualifier {$$ = 0;}
    | type_qualifier declaration_specifiers {$$ = 0;}
    ;
    
init_declarator_list
    : init_declarator {$$ = $1 + 1;}
    | init_declarator_list ',' init_declarator {$$ = $1 + 1;}
    ;
    
init_declarator
    : declarator {$$ = $1;}
    | declarator '=' initializer {$$ = $1; ary[1]++;}
    //대입 연산자 사용으로 인해 연산자 카운트 증가
    ;
    
storage_class_specifier
    : TYPEDEF
    | EXTERN
    | STATIC
    | AUTO
    | REGISTER
    ;

type_specifier
    : VOID {$$ = 0;}
    | CHAR {ary[3]++; $$ = 3;}
    //char 카운트 증가, type_specifier에 3값 넘김
    | SHORT {$$ = 0;}
    | INT {ary[2]++; $$ = 2;}
    //int 카운트 증가, type_specifier에 2값 넘김
    | LONG {$$ = 0;}
    | FLOAT {$$ = 0;}
    | DOUBLE {$$ = 0;}
    | SIGNED {$$ = 0;}
    | UNSIGNED {$$ = 0;}
    | struct_or_union_specifier {$$ = 0;}
    | enum_specifier {$$ = 0;}
    | TYPE_NAME {$$ = 0;}
    ;

struct_or_union_specifier
    : struct_or_union IDENTIFIER '{' struct_declaration_list '}'
    | struct_or_union '{' struct_declaration_list '}'
    | struct_or_union IDENTIFIER
    ;
    
struct_or_union
    : STRUCT
    | UNION
    ;
    
struct_declaration_list
    : struct_declaration
    | struct_declaration_list struct_declaration
    ;

struct_declaration
    : specifier_qualifier_list struct_declarator_list ';' 
    {check2($1, $2);}
    /*
    구조체 내에서 int 또는 char 변수 여러개가 한번에 선언되는 경우 
    
    그 개수만큼 카운트를 더해주기 위해 check2 함수 사용
    */
    ;

specifier_qualifier_list
    : type_specifier specifier_qualifier_list {$$ = $1;}
    | type_specifier {$$ = $1;}
    | type_qualifier specifier_qualifier_list {$$ = 0;}
    | type_qualifier {$$ = 0;}
    ;
    
struct_declarator_list
    : struct_declarator {$$ = $1 + 1;}
    | struct_declarator_list ',' struct_declarator {$$ = $1 + 1;}
    ;

struct_declarator
    : declarator {$$ = $1;}
    | ':' constant_expression
    | declarator ':' constant_expression {$$ = $1;}
    ;
    
    enum_specifier
    : ENUM '{' enumerator_list '}'
    | ENUM IDENTIFIER '{' enumerator_list '}'
    | ENUM IDENTIFIER
    ;
    
enumerator_list
    : enumerator
    | enumerator_list ',' enumerator
    ;
    
enumerator
    : IDENTIFIER
    | IDENTIFIER '=' constant_expression {ary[1]++;}
    ;
    
type_qualifier
    : CONST
    | VOLATILE
    ;
    
declarator
    : pointer direct_declarator 
    {
      $$ = $2; ptrFun = 1;
      
      if ($2 == 0 && ptr > 1) {
        mpNum++;
      }
    }
    | direct_declarator {$$ = $1;}
    ;

direct_declarator
    : IDENTIFIER {$$ = 0;}
    | '(' declarator ')' {$$ = -3;}
    | direct_declarator '[' constant_expression ']' 
    {$$ = -2; ary[5]++;} //배열 카운트 증가
    | direct_declarator '[' ']' {$$ = -2; ary[5]++;}
    //배열 카운트 증가
    | direct_declarator '(' parameter_type_list ')' 
    {
      $$ = -1;
      
      if ($1 != -3) {
        ptrFun = 0;
      }
    }
    | direct_declarator '(' identifier_list ')' {$$ = -2;}
    | direct_declarator '(' ')' 
    {
      $$ = -1;
      
      if ($1 != -3) {
        ptrFun = 0;
      }
    }  
    ;
    
pointer
    : '*' {ptr = 1; ary[4]++;} //포인터 카운트 증가
    | '*' type_qualifier_list 
    {ptr = 1; ary[4]++;} //포인터 카운트 증가
    | '*' pointer {ptr++;}
    | '*' type_qualifier_list pointer {ptr++;}
    ;

type_qualifier_list
    : type_qualifier
    | type_qualifier_list type_qualifier
    ;
    
parameter_type_list
    : parameter_list
    | parameter_list ',' ELLIPSIS
    ;
    
parameter_list
    : parameter_declaration
    | parameter_list ',' parameter_declaration
    ;

parameter_declaration
    : declaration_specifiers declarator {count($1);}
    /*
    함수 선언부의 파라미터에서 int, char가 카운트된 것을 빼주기
    위해 count 함수를 통해 int, char 개수를 카운트
    */
    | declaration_specifiers abstract_declarator
    | declaration_specifiers {count($1);}
    /*
    함수 선언부의 파라미터에서 int, char가 카운트된 것을 빼주기
    위해 count 함수를 통해 int, char 개수를 카운트
    */
    ;
    
identifier_list
    : IDENTIFIER
    | identifier_list ',' IDENTIFIER
    ;

type_name
    : specifier_qualifier_list {$$ = $1;}
    | specifier_qualifier_list abstract_declarator {$$ = $1;}
    ;

abstract_declarator
    : pointer
    | direct_abstract_declarator
    | pointer direct_abstract_declarator
    ;
    
direct_abstract_declarator
    : '(' abstract_declarator ')'
    | '[' ']'
    | '[' constant_expression ']'
    | direct_abstract_declarator '[' ']'
    | direct_abstract_declarator '[' constant_expression ']'
    | '(' ')'
    | '(' parameter_type_list ')'
    | direct_abstract_declarator '(' ')'
    | direct_abstract_declarator '(' parameter_type_list ')'
    ;

initializer
    : assignment_expression
    | '{' initializer_list '}'
    | '{' initializer_list ',' '}'
    ;
    
initializer_list
    : initializer
    | initializer_list ',' initializer
    ;
    
statement
    : labeled_statement
    | compound_statement
    | expression_statement
    | selection_statement
    | iteration_statement
    | jump_statement
    ;

labeled_statement
    : IDENTIFIER ':' statement
    | CASE constant_expression ':' statement
    | DEFAULT ':' statement
    ;
    
compound_statement
    : '{' '}'
    | '{' compound_list '}'
    ;
    
compound_list
    : compound
    | compound_list compound
    ;
    
compound
    : statement
    | declaration
    ;

declaration_list
    : declaration
    | declaration_list declaration
    ;
    
expression_statement
    : ';'
    | expression ';'
    ;

selection_statement
    : IF '(' expression ')' statement {ary[6]++;}
    | SWITCH '(' expression ')' statement {ary[6]++;}
    ; //선택문 카운트 개수 증가
    
iteration_statement
    : WHILE '(' expression ')' statement {ary[7]++;}
    | DO statement WHILE '(' expression ')' ';' {ary[7]++;}
    | FOR '(' expression_statement expression_statement ')' 
    statement {ary[7]++;}
    | FOR '(' expression_statement expression_statement 
    expression ')' statement {ary[7]++;}
    ; //반복문 카운트 개수 증가

jump_statement
    : GOTO IDENTIFIER ';'
    | CONTINUE ';'
    | BREAK ';'
    | RETURN ';' {ary[8]++;} //리턴문 카운트 개수 증가
    | RETURN expression ';' {ary[8]++;} //리턴문 개수 증가
    ;
    
translation_unit
    : external_declaration
    | translation_unit external_declaration
    ;
    
external_declaration
    : function_definition
    | declaration
    | preprocessor
    ;

function_definition
    : declaration_specifiers declarator declaration_list 
    compound_statement 
    {cancel($1), ary[0]++;} 
    /*
    1) 함수 반환형으로 int 또는 char 사용 시 cancel 함수 통해 
    카운트 1 감소
    2) 함수 정의로 인해 함수 카운트 증가
    */
    | declaration_specifiers declarator compound_statement 
    {cancel($1); ary[0]++;}
    /*
    1) 함수 반환형으로 int 또는 char 사용 시 cancel 함수 통해 
    카운트 1 감소
    2) 함수 정의로 인해 함수 카운트 증가
    */
    | declarator declaration_list compound_statement 
    {ary[0]++;} //함수 정의로 인해 함수 카운트 증가
    | declarator compound_statement 
    {ary[0]++;} //함수 정의로 인해 함수 카운트 증가
    ;
    
preprocessor
    : '#' INCLUDE '<' HEADER '>' 
    | '#' INCLUDE '"' HEADER '"' 
    | '#' DEFINE IDENTIFIER CONSTANT
    ;
%%

void check(int num1, int num2) {
  if (num1 && num2 > 0) { //int 또는 char 변수 선언
    if (mpNum == num2) { //다중포인터 1개 선언
      ary[num1]--;
      mpNum = 0;
    }
    else {
      if (mpNum) { //선언된 변수 중 일부만 다중포인터
        num2 -= mpNum;
        mpNum = 0;
      }
      //int 또는 char 변수가 다수이므로 카운트 증가
      ary[num1] += (num2 - 1);
    }
  }
  else if (num2 == 0) { //함수 선언부
    if (num1) { //함수 선언부의 반환형이 int 또는 char
      ary[num1]--;
    }
    
    if (ptrFun) { //선언된 것이 포인터 함수임
      ary[0]++; //함수 카운트 증가
      /*
      int 또는 char형 포인터 함수면 intPtr 또는 charPtr을 
      통해 포인터 카운트가 감소하므로 상쇄하기 위해 ary[4] 값 
      2 증가
      */
      if (num1 == 2 || num1 == 3) {
        ary[4] += 2;
      }
    }
    
    //파라미터가 int인 경우
    if (intDec) {
      ary[2] -= intVar;
      ary[4] -= intPtr; //int형 포인터가 파라미터인 경우
      intDec = intVar = intPtr = ptr = 0;
    }
    
    //파라미터가 char인 경우
    if (charDec) {
      ary[3] -= charVar;
      ary[4] -= charPtr; //char형 포인터가 파라미터인 경우
      charDec = charVar = charPtr = ptr = 0;
    }
  }
}

void check2(int num1, int num2) {
  if (num1) { //구조체에서 int 또는 char형 변수 선언
    if (mpNum == num2) { //다중포인터 1개 선언
      ary[num1]--;
      mpNum = 0;
    }
    else {
      if (mpNum) { //변수 중 일부만 다중포인터임
        num2 -= mpNum;
      }
      //int 또는 char 변수가 다수이므로 카운트 증가
      ary[num1] += (num2 - 1);
    }
  }
  
  if (ptrFun) { //선언된 것이 포인터 함수임
    ary[0]++; //함수 카운트 증가
    /*
      int 또는 char형 포인터 함수면 intPtr 또는 charPtr을 
      통해 포인터 카운트가 감소하므로 상쇄하기 위해 ary[4] 값 
      2 증가
      */
    if ((intDec && num1 == 2) || (charDec && num1 == 3)) {
      ary[4] += 2;
    }
    
    if (intDec) {
      ary[2] -= intVar;
      ary[4] -= intPtr; //int형 포인터가 파라미터인 경우
      intDec = intVar = intPtr = ptr = 0;
    }
    
    if (charDec) {
      ary[3] -= charVar;
      ary[4] -= charPtr; //char형 포인터가 파라미터인 경우
      charDec = charVar = charPtr = ptr = 0;
    }
  }
}

void count(int num) {
  if (num == 2) { //선언된 변수가 int인 경우
    intDec = 1;
    intVar++;
    
    if (ptr) { //선언된 변수가 int형 포인터
      intPtr++;
      ptr = 0;
    }
  }
  
  if (num == 3) { //선언된 변수가 char인 경우
    charDec = 1;
    charVar++;
    
    if (ptr) { //선언된 변수가 char형 포인터
      charPtr++;
      ptr = 0;
    }
  }
}

void cancel(int num) {
  if (num) {
    ary[num]--;
  }  
}

int main(void)
{
	yyparse();
	printf("function = %d\n", ary[0]);
	printf("operator = %d\n", ary[1]);
	printf("int = %d\n", ary[2]);
	printf("char = %d\n", ary[3]);
	printf("pointer = %d\n", ary[4]);
	printf("array = %d\n", ary[5]);
	printf("selection = %d\n", ary[6]);
	printf("loop = %d\n", ary[7]);
	printf("return = %d\n", ary[8]);
	return 0;
}

void yyerror(const char *str)
{
	fprintf(stderr, "error: %s\n", str);
}
