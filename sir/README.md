# SIR

> sofia intermetiate representation

Язык промежуточного представления языка sofia.

## Синтаксис

```ebnf
file = {top_level_object};
top_level_object = external_symbol_declaration
                 | type_definition
                 | function_definition
                 ;

external_symbol_declaration = "extern", name, ":", type;
type_definition = "type", name, "{", {type_definition_field}, "}";
type_definition_field = name, ":", type, ";";
function_definition = "function", name, "(", [function_definition_parameter_list], ")",
    "{", {stmt}, "}";
function_definition_parameter_list = function_definition_parameter, [","]
                                   | function_definition_parameter, ",", function_definition_parameter_list
                                   ;
function_definition_parameter = name, ":", ["ref"], type;

stmt = "{" {stmt} "}"
     | expr, ";"
     | "let", name, "=", expr, ";"
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
     | "[", [expr_list], "]"
     | "{", {expr_struct_field_list}, "}"
     | expr, "(", [expr_list], ")"
     | expr, "[", expr, "]"
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
        | literal_positive_integer
        | literal_floating_point_number
        | literal_char
        | literal_string
        ;
literal_positive_integer = ? [1-9][0-9]*|0 ?;
literal_floating_point_number = ? -?(([1-9][0-9]*)?\.([0-9]*[1-9]([eE]-?[1-9][0-9]*)?|0)|[1-9][0-9]*[eE]-?[1-9][0-9]*) ?;
literal_char = ? '.' ?;
literal_string = ? "(\\.|[^\\"])*" ?;

name = ? [a-zA-Z_][a-zA-Z0-9_]* ?; (* C-style identifier *)
```


