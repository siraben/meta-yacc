#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void meta_program();
void meta_exp1();
void meta_exp3();
void meta_arg();
void meta_output();
void meta_exp2();
void meta_exp1();
void meta_comment();
void meta_stat();
void meta_support();
void meta_declist();
void meta_main();

// Output name, input string, token
char *on, *s, *t;
// Output
FILE *o;
// Labels
int ac, bc;
// Label locks
int ll = 3;
// Current location in string, flag, indentation level, newline flag
int p, flag, indent, nl;
// Line counter
int line = 1;

void skip_whitespace() {
  while (isspace(s[p])) {
    if (s[p] == '\n') {
      line++;
    }
    p++;
  }
}

void make_token(int sp) {
  int length = p - sp;
  free(t);
  t = malloc(length + 1);
  t[length] = 0;
  memcpy(t, &s[sp], length);
}

void emit_token() {
  int i;
  char d;
  // We introduce a new quoting operator, the tilde, which can quote
  // single quotes.  In this way, we don't need an escape character.
  if (t[0] == '\'' || t[0] == '~') {
    d = t[0];
    fprintf(o, "\"");
    for (i = 1; t[i] && t[i] != d; i++) {
      switch (t[i]) {
        case '\n':
          fprintf(o, "\\n");
          break;
        case '\"':
          fprintf(o, "\\\"");
          break;
        case '\'':
          fprintf(o, "\\\'");
          break;
        case '\\':
          fprintf(o, "\\\\");
          break;
        default:
          fprintf(o, "%c", t[i]);
          break;
      }
    }
    fprintf(o, "\"");
    return;
  }
  fprintf(o, "%s", t);
}

void emit(const char *s) {
  // fprintf(o, "%s", s);
  fprintf(o, "%*s%s", nl ? indent : 0, "", s);
  nl = 0;
}

void emit_label_a() {
  ac += (ll & 1);
  fprintf(o, "a%02d ", ac);
}

void emit_label_b() {
  bc += (ll & 2) >> 1;
  fprintf(o, "b%02d ", bc);
}

void unlock_labels() {
  ll = 3;
}

void emit_newline() {
  nl = 1;
  fprintf(o, "\n");
}

void read_literal(const char *l) {
  int e, i;
  skip_whitespace();
  e = p;
  i = 0;
  while (s[p] && l[i] && s[p] == l[i]) {
    p++;
    i++;
  }
  if (!l[i]) {
    flag = 1;
    make_token(e);
  } else {
    p = e;
    flag = 0;
  }
}

void read_id() {
  int e;
  skip_whitespace();
  e = p;
  if (isalpha(s[p])) {
    p++;
    flag = 1;
  } else {
    flag = 0;
    return;
  }
  while (isalnum(s[p])) {
    p++;
  }
  make_token(e);
}

void read_number() {
  int e;
  skip_whitespace();
  e = p;
  if (s[p] == '-') {
    p++;
  }
  if (isdigit(s[p])) {
    p++;
    flag = 1;
  } else {
    flag = 0;
    return;
  }
  while (isdigit(s[p])) {
    p++;
  }
  make_token(e);
}

void read_string() {
  int e;
  char delim;
  skip_whitespace();
  e = p;
  if (s[p] == '\'' || s[p] == '~') {
    delim = s[p++];
    while (s[p] && s[p] != delim) {
      if (s[p] == '\n') {
        line++;
      }
      p++;
    }
    if (s[p] == delim) {
      p++;
      flag = 1;
      make_token(e);
    } else if (!s[p]) {
      p = e;
      flag = 0;
    }
  } else {
    flag = 0;
    return;
  }
}

void maybe_error() {
  if (!flag) {
    fprintf(stderr, "Error in line %i at token '%s'\n", line, t);
    fclose(o);
    remove(on);
    free(s);
    free(t);
    exit(1);
  }
}

int main(int argc, char *argv[]) {
    FILE *input;
    int length;

    if (argc < 3) {
        fprintf(stderr, "usage: meta <input> <output>\n");
        exit(1);
    }
    input = fopen(argv[1], "r");
    if (!input) {
        fprintf(stderr, "invalid input file\n");
        exit(1);
    }
    on = argv[2];
    o = fopen(on, "w");
    if (!o) {
        fprintf(stderr, "invalid output file\n");
        exit(1);
    }
    fseek(input, 0, SEEK_END);
    length = (int)ftell(input);
    fseek(input, 0, SEEK_SET);
    s = malloc(length + 1);
    fread(s, 1, length, input);
    s[length] = 0;
    fclose(input);

    t = malloc(1);
    t[0] = 0;

    meta_program();

    skip_whitespace();
    if (p == strlen(s)) {
      fprintf(stdout, "Fully parsed %d characters.\n", p);
    } else {
      fprintf(stderr, "%d/%d characters processed\n", p, strlen(s));
    }

    fclose(o);
    free(s);
    free(t);
    return 0;
}
void meta_arg() {
  do {
    read_literal("*1");
    if (flag) {
      emit("emit_label_a();");
      emit_newline();
      emit("ll &= 2;");
      emit_newline();
    }
    if (flag) { break; }
    read_literal("*2");
    if (flag) {
      emit("emit_label_b();");
      emit_newline();
      emit("ll &= 1;");
      emit_newline();
    }
    if (flag) { break; }
    read_literal("*");
    if (flag) {
      emit("emit_token();");
      emit_newline();
    }
    if (flag) { break; }
    read_string();
    if (flag) {
      emit("emit(");
      emit_token();
      emit(");");
      emit_newline();
    }
  } while (0);
}

void meta_output() {
  do {
    do {
      read_literal("{");
      if (flag) {
        do {
          meta_arg();
        } while (flag);
        flag = 1;
        maybe_error();
        read_literal("}");
        maybe_error();
        emit("emit_newline();");
        emit_newline();
      }
      if (flag) { break; }
      read_literal("<");
      if (flag) {
        do {
          meta_arg();
        } while (flag);
        flag = 1;
        maybe_error();
        read_literal(">");
        maybe_error();
      }
    } while (0);
    if (flag) {
    }
  } while (0);
}

void meta_exp3() {
  do {
    read_id();
    if (flag) {
      emit("meta_");
      emit_token();
      emit("();");
      emit_newline();
    }
    if (flag) { break; }
    read_string();
    if (flag) {
      emit("read_literal(");
      emit_token();
      emit(");");
      emit_newline();
    }
    if (flag) { break; }
    read_literal(".id");
    if (flag) {
      emit("read_id();");
      emit_newline();
    }
    if (flag) { break; }
    read_literal(".number");
    if (flag) {
      emit("read_number();");
      emit_newline();
    }
    if (flag) { break; }
    read_literal(".string");
    if (flag) {
      emit("read_string();");
      emit_newline();
    }
    if (flag) { break; }
    read_literal(".lm+");
    if (flag) {
      emit("indent += 2;");
      emit_newline();
    }
    if (flag) { break; }
    read_literal(".lm-");
    if (flag) {
      emit("indent -= 2;");
      emit_newline();
    }
    if (flag) { break; }
    read_literal("(");
    if (flag) {
      meta_exp1();
      maybe_error();
      read_literal(")");
      maybe_error();
    }
    if (flag) { break; }
    read_literal(".e");
    if (flag) {
      emit("flag = 1;");
      emit_newline();
    }
    if (flag) { break; }
    read_literal("$");
    if (flag) {
      emit("do {");
      emit_newline();
      indent += 2;
      maybe_error();
      meta_exp3();
      maybe_error();
      indent -= 2;
      maybe_error();
      emit("} while (flag);");
      emit_newline();
      emit("flag = 1;");
      emit_newline();
    }
  } while (0);
}

void meta_exp2() {
  do {
    do {
      meta_exp3();
      if (flag) {
        emit("if (flag) {");
        emit_newline();
      }
      if (flag) { break; }
      meta_output();
      if (flag) {
        emit("if (1) {");
        emit_newline();
      }
    } while (0);
    if (flag) {
      indent += 2;
      maybe_error();
      do {
        do {
          meta_exp3();
          if (flag) {
            emit("maybe_error();");
            emit_newline();
          }
          if (flag) { break; }
          meta_output();
          if (flag) {
          }
        } while (0);
      } while (flag);
      flag = 1;
      maybe_error();
      indent -= 2;
      maybe_error();
      emit("}");
      emit_newline();
    }
  } while (0);
}

void meta_exp1() {
  do {
    emit("do {");
    emit_newline();
    if (1) {
      indent += 2;
      maybe_error();
      meta_exp2();
      maybe_error();
      do {
        do {
          read_literal("|");
          if (flag) {
            emit("if (flag) { break; }");
            emit_newline();
            meta_exp2();
            maybe_error();
          }
        } while (0);
      } while (flag);
      flag = 1;
      maybe_error();
      indent -= 2;
      maybe_error();
      emit("} while (0);");
      emit_newline();
    }
  } while (0);
}

void meta_comment() {
  do {
    read_literal("[");
    if (flag) {
      read_string();
      maybe_error();
      read_literal("]");
      maybe_error();
    }
  } while (0);
}

void meta_stat() {
  do {
    read_id();
    if (flag) {
      emit("void meta_");
      emit_token();
      emit("() {");
      emit_newline();
      indent += 2;
      maybe_error();
      read_literal("=");
      maybe_error();
      meta_exp1();
      maybe_error();
      read_literal(";");
      maybe_error();
      indent -= 2;
      maybe_error();
      emit("}");
      emit_newline();
      emit_newline();
    }
    if (flag) { break; }
    meta_comment();
    if (flag) {
    }
  } while (0);
}

void meta_support() {
  do {
    emit("\n// Output name, input string, token\nchar *on, *s, *t;\n// Output\nFILE *o;\n// Labels\nint ac, bc;\n// Label locks\nint ll = 3;\n// Current location in string, flag, indentation level, newline flag\nint p, flag, indent, nl;\n// Line counter\nint line = 1;\n\nvoid skip_whitespace() {\n  while (isspace(s[p])) {\n    if (s[p] == \'\\n\') {\n      line++;\n    }\n    p++;\n  }\n}\n\nvoid make_token(int sp) {\n  int length = p - sp;\n  free(t);\n  t = malloc(length + 1);\n  t[length] = 0;\n  memcpy(t, &s[sp], length);\n}\n\nvoid emit_token() {\n  int i;\n  char d;\n  // We introduce a new quoting operator, the tilde, which can quote\n  // single quotes.  In this way, we don\'t need an escape character.\n  if (t[0] == \'\\\'\' || t[0] == \'");
    emit("~");
    emit("\') {\n    d = t[0];\n    fprintf(o, \"\\\"\");\n    for (i = 1; t[i] && t[i] != d; i++) {\n      switch (t[i]) {\n        case \'\\n\':\n          fprintf(o, \"\\\\n\");\n          break;\n        case \'\\\"\':\n          fprintf(o, \"\\\\\\\"\");\n          break;\n        case \'\\\'\':\n          fprintf(o, \"\\\\\\\'\");\n          break;\n        case \'\\\\\':\n          fprintf(o, \"\\\\\\\\\");\n          break;\n        default:\n          fprintf(o, \"%c\", t[i]);\n          break;\n      }\n    }\n    fprintf(o, \"\\\"\");\n    return;\n  }\n  fprintf(o, \"%s\", t);\n}\n\nvoid emit(const char *s) {\n  // fprintf(o, \"%s\", s);\n  fprintf(o, \"%*s%s\", nl ? indent : 0, \"\", s);\n  nl = 0;\n}\n\nvoid emit_label_a() {\n  ac += (ll & 1);\n  fprintf(o, \"a%02d \", ac);\n}\n\nvoid emit_label_b() {\n  bc += (ll & 2) >> 1;\n  fprintf(o, \"b%02d \", bc);\n}\n\nvoid unlock_labels() {\n  ll = 3;\n}\n\nvoid emit_newline() {\n  nl = 1;\n  fprintf(o, \"\\n\");\n}\n\nvoid read_literal(const char *l) {\n  int e, i;\n  skip_whitespace();\n  e = p;\n  i = 0;\n  while (s[p] && l[i] && s[p] == l[i]) {\n    p++;\n    i++;\n  }\n  if (!l[i]) {\n    flag = 1;\n    make_token(e);\n  } else {\n    p = e;\n    flag = 0;\n  }\n}\n\nvoid read_id() {\n  int e;\n  skip_whitespace();\n  e = p;\n  if (isalpha(s[p])) {\n    p++;\n    flag = 1;\n  } else {\n    flag = 0;\n    return;\n  }\n  while (isalnum(s[p])) {\n    p++;\n  }\n  make_token(e);\n}\n\nvoid read_number() {\n  int e;\n  skip_whitespace();\n  e = p;\n  if (s[p] == \'-\') {\n    p++;\n  }\n  if (isdigit(s[p])) {\n    p++;\n    flag = 1;\n  } else {\n    flag = 0;\n    return;\n  }\n  while (isdigit(s[p])) {\n    p++;\n  }\n  make_token(e);\n}\n\nvoid read_string() {\n  int e;\n  char delim;\n  skip_whitespace();\n  e = p;\n  if (s[p] == \'\\\'\' || s[p] == \'");
    emit("~");
    emit("\') {\n    delim = s[p++];\n    while (s[p] && s[p] != delim) {\n      if (s[p] == \'\\n\') {\n        line++;\n      }\n      p++;\n    }\n    if (s[p] == delim) {\n      p++;\n      flag = 1;\n      make_token(e);\n    } else if (!s[p]) {\n      p = e;\n      flag = 0;\n    }\n  } else {\n    flag = 0;\n    return;\n  }\n}\n\nvoid maybe_error() {\n  if (!flag) {\n    fprintf(stderr, \"Error in line %i at token \'%s\'\\n\", line, t);\n    fclose(o);\n    remove(on);\n    free(s);\n    free(t);\n    exit(1);\n  }\n}\n");
    emit_newline();
    if (1) {
    }
  } while (0);
}

void meta_declist() {
  do {
    emit("#include <ctype.h>\n#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n");
    emit_newline();
    if (1) {
      read_literal("[");
      maybe_error();
      do {
        do {
          read_id();
          if (flag) {
            emit("void meta_");
            emit_token();
            emit("();");
            emit_newline();
          }
        } while (0);
      } while (flag);
      flag = 1;
      maybe_error();
      read_literal("]");
      maybe_error();
    }
  } while (0);
}

void meta_main() {
  do {
    read_id();
    if (flag) {
      emit("int main(int argc, char *argv[]) {\n    FILE *input;\n    int length;\n\n    if (argc < 3) {\n        fprintf(stderr, \"usage: meta <input> <output>\\n\");\n        exit(1);\n    }\n    input = fopen(argv[1], \"r\");\n    if (!input) {\n        fprintf(stderr, \"invalid input file\\n\");\n        exit(1);\n    }\n    on = argv[2];\n    o = fopen(on, \"w\");\n    if (!o) {\n        fprintf(stderr, \"invalid output file\\n\");\n        exit(1);\n    }\n    fseek(input, 0, SEEK_END);\n    length = (int)ftell(input);\n    fseek(input, 0, SEEK_SET);\n    s = malloc(length + 1);\n    fread(s, 1, length, input);\n    s[length] = 0;\n    fclose(input);\n\n    t = malloc(1);\n    t[0] = 0;\n\n    meta_");
      emit_token();
      emit("();\n\n    skip_whitespace();\n    if (p == strlen(s)) {\n      fprintf(stdout, \"Fully parsed %d characters.\\n\", p);\n    } else {\n      fprintf(stderr, \"%d/%d characters processed\\n\", p, strlen(s));\n    }\n\n    fclose(o);\n    free(s);\n    free(t);\n    return 0;\n}");
      emit_newline();
    }
  } while (0);
}

void meta_program() {
  do {
    read_literal(".syntax");
    if (flag) {
      meta_declist();
      maybe_error();
      meta_support();
      maybe_error();
      meta_main();
      maybe_error();
      do {
        meta_stat();
      } while (flag);
      flag = 1;
      maybe_error();
      read_literal(".end");
      maybe_error();
    }
  } while (0);
}

