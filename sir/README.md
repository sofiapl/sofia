# SIR

> sofia intermetiate representation

Язык промежуточного представления языка sofia.

## Синтаксис

```ebnf
file = {top_level_object};
top_level_object = external_symbol_declaration
                 | type_definition
                 | function_definition
                 | global_variable_definition
                 ;

external_symbol_declaration = "extern", name, ":", type, ";";
type_definition = "type", name, "{", {type_definition_field}, "}";
type_definition_field = name, ":", type, ";";
function_definition = ["extern"], "function", name, "(", [function_definition_parameter_list], ")",
    ":", type, "{", {stmt}, "}";
function_definition_parameter_list = function_definition_parameter, [","]
                                   | function_definition_parameter, ",", function_definition_parameter_list
                                   ;
function_definition_parameter = name, ":", ["ref"], type;
global_variable_definition = ["extern"], "let", name, ":", type, "=", literal, ";";

stmt = "{" {stmt} "}"
     | expr, ";"
     | "let", name, ":", type, "=", expr, ";"
     | "switch", "(", name, ")", "{", {stmt_case}, "}"
     | "while", "(", expr, ")", stmt
     | "break", ";"
     | "continue", ";"
     | "return", [expr], ";"
     ;
stmt_case = "case", literal, ":", stmt
          | "default", ":", stmt
          ;

expr = "(", ")"
     | "(", expr, ",", expr_list, ")"
     | type, "[", expr, "]"
     | "{", {expr_struct_field_list}, "}"
     | expr, "(", [expr_list], ")"
     | expr, "[", expr, "]"
     | expr, ".", name
     | literal
     | name
     ;
expr_list = expr, [","]
          | expr, ",", expr_list
          ;
expr_struct_field_list = expr_struct_field, [","]
                       | expr_struct_field, ",", expr_struct_field_list
                       ;
expr_struct_field = name, ":", expr;

type = inner_type | name;
inner_type = ["unsigned"], "int", literal_positive_integer (* minimal size in bits *)
           | "float", literal_positive_integer (* minimal size in bits *)
           | "bool"
           | "char"
           | "[", type, "]" (* array type *)
           | "(", ")" (* unit *)
           | "(", type, ",", types_list, ")" (* tuple *)
           | type, "->", type (* function *)
           ;
types_list = type, [","]
           | type, ",", types_list
           ;

literal = "true"
        | "false"
        | "NaN"
        | "Infinity"
        | "+Infinity"
        | "-Infinity"
        | literal_positive_integer
        | literal_floating_point_number
        | literal_char
        | literal_string
        ;
literal_positive_integer = ? [1-9][0-9]*|0 ?;
literal_floating_point_number = ? -?(([1-9][0-9]*)?\.([0-9]*[1-9]([eE]-?[1-9][0-9]*)?|0)|[1-9][0-9]*[eE]-?[1-9][0-9]*) ?;
literal_char = ? '(\\.|[^\\'])' ?;
literal_string = ? "(\\.|[^\\"])*" ?;

name = ? [a-zA-Z_][a-zA-Z0-9_]* ?; (* C-style identifier *)
```

## Типы данных

Типы данных существуют встроенные и определяемые пользователем структуры.

### Встроенные типы данных

- Целое число: `["unsigned"], "int", literal_positive_integer`
  Целые числа могут быть знаковыми и беззнаковыми, для всех целочисленных типов указывается
  минимальная длина в битах, которую может содержать переменная такого типа
- Дробное число с плавающей запятой: `"float", literal_positive_integer`
  Для дробных чисел также указывается минимальная ширина в предположении, что они представлены
  в формате IEEE 754
- Булев тип: `bool`
  Может содержать "true" или "false"
- Символьный тип: `char`
  Представлен в виде UTF-16
- Массив: `"[", type, "]"`
  Имеет константный размер, не инициализированные значения не имеют определённого значения
- Пустой кортеж, unit type: `()`
  Тип, имеющий одно возможное значение: `()`
- Кортеж: `"(", type, ",", types_list, ")"`
  Может содержать не меньше двух значений любых типов
- Функция: `type, "->", type`
