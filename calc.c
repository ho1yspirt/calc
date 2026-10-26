// Amir Asakeev 251ADC002
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
// Resources used: lab task hints, en.cppreference.com for detailed
// understanding of cited functions by AI requests, Google Gemini (web chat
// interface),

#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#define STUDENT_NAME "amir"
#define STUDENT_LASTNAME "asakeev"
#define STUDENT_ID "251ADC002"

enum {
  TOK_NUM = 1,
  TOK_PLUS,
  TOK_MINUS,
  TOK_MUL,
  TOK_DIV,
  TOK_POW,
  TOK_LPAREN,
  TOK_RPAREN,
  TOK_EOL,
  TOK_EOF,
  TOK_ERR
};

typedef struct {
  int type;
  long pos;
  int is_float;
  long long ival;
  double fval;
} Token;

// AI (Gemini web): prompt "how to return either int or double from function in
// C for a calculator" Used a struct with an is_float flag to hold polymorphic
// values since C doesn't have dynamic typing.
typedef struct {
  int is_float;
  long long ival;
  double fval;
} Value;

static char *src_buf = NULL;
static size_t src_len = 0;
static size_t src_idx = 0;
static Token cur_tok;
static long err_pos = 0;

static void set_error(long pos) {
  if (err_pos == 0)
    err_pos = pos;
}

// Position is src_idx + 1 because we never cut out comments or newlines
static void next_token(void) {
  while (src_idx < src_len &&
         (src_buf[src_idx] == ' ' || src_buf[src_idx] == '\t' ||
          src_buf[src_idx] == '\r')) {
    src_idx++;
  }

  if (src_idx >= src_len) {
    cur_tok.type = TOK_EOF;
    cur_tok.pos = (long)src_len + 1;
    return;
  }

  char c = src_buf[src_idx];
  cur_tok.pos = (long)src_idx + 1;

  if (c == '\n') {
    cur_tok.type = TOK_EOL;
    return;
  }

  if (c >= '0' && c <= '9') {
    size_t start = src_idx;
    long long ival = 0;
    while (src_idx < src_len && src_buf[src_idx] >= '0' &&
           src_buf[src_idx] <= '9') {
      ival = ival * 10 + (src_buf[src_idx] - '0');
      src_idx++;
    }

    // Float needs digits on both sides of dot
    if (src_idx + 1 < src_len && src_buf[src_idx] == '.' &&
        src_buf[src_idx + 1] >= '0' && src_buf[src_idx + 1] <= '9') {
      src_idx++;
      while (src_idx < src_len && src_buf[src_idx] >= '0' &&
             src_buf[src_idx] <= '9') {
        src_idx++;
      }
      // AI (Gemini web): prompt "how to parse float with strtod without letting
      // it parse scientific notation like 1.5e3" Gemini suggested temporarily
      // putting '\0' at current position so strtod stops right here.
      char saved = src_buf[src_idx];
      src_buf[src_idx] = '\0';
      cur_tok.type = TOK_NUM;
      cur_tok.is_float = 1;
      cur_tok.fval = strtod(src_buf + start, NULL);
      cur_tok.ival = 0;
      src_buf[src_idx] = saved;
    } else {
      cur_tok.type = TOK_NUM;
      cur_tok.is_float = 0;
      cur_tok.ival = ival;
      cur_tok.fval = (double)ival;
    }
    return;
  }

  if (c == '+') {
    src_idx++;
    cur_tok.type = TOK_PLUS;
    return;
  }
  if (c == '-') {
    src_idx++;
    cur_tok.type = TOK_MINUS;
    return;
  }
  if (c == '*') {
    if (src_idx + 1 < src_len && src_buf[src_idx + 1] == '*') {
      src_idx += 2;
      cur_tok.type = TOK_POW;
    } else {
      src_idx++;
      cur_tok.type = TOK_MUL;
    }
    return;
  }
  if (c == '/') {
    src_idx++;
    cur_tok.type = TOK_DIV;
    return;
  }
  if (c == '(') {
    src_idx++;
    cur_tok.type = TOK_LPAREN;
    return;
  }
  if (c == ')') {
    src_idx++;
    cur_tok.type = TOK_RPAREN;
    return;
  }

  src_idx++;
  cur_tok.type = TOK_ERR;
}

static Value parse_expr(void);

// Convert int to double if one of operands is float
static double to_double(Value v) {
  return v.is_float ? v.fval : (double)v.ival;
}

static Value parse_primary(void) {
  Value v = {0, 0, 0.0};
  if (err_pos != 0)
    return v;

  if (cur_tok.type == TOK_NUM) {
    v.is_float = cur_tok.is_float;
    v.ival = cur_tok.ival;
    v.fval = cur_tok.fval;
    if (v.is_float && (isnan(v.fval) || isinf(v.fval))) {
      set_error(cur_tok.pos);
      return v;
    }
    next_token();
    return v;
  }

  if (cur_tok.type == TOK_LPAREN) {
    next_token();
    v = parse_expr();
    if (err_pos != 0)
      return v;

    if (cur_tok.type != TOK_RPAREN) {
      set_error(cur_tok.pos);
      return v;
    }
    next_token();
    return v;
  }

  // Unary minus or wrong token
  set_error(cur_tok.pos);
  return v;
}

// Power calls itself so 2**3**2 evaluates from right to left
static Value parse_power(void) {
  Value left = parse_primary();
  if (err_pos != 0)
    return left;

  if (cur_tok.type == TOK_POW) {
    long op_pos = cur_tok.pos;
    next_token();
    long rhs_pos = cur_tok.pos;
    Value right = parse_power();
    if (err_pos != 0)
      return left;

    if (!left.is_float && !right.is_float) {
      if (right.ival < 0) {
        set_error(rhs_pos);
        return left;
      }
      // AI (Gemini web): prompt "fast integer exponentiation in C using bitwise
      // operations" Used binary exponentiation to calculate 64-bit integer
      // powers without float rounding.
      long long base = left.ival;
      long long exp = right.ival;
      long long res = 1;
      while (exp > 0) {
        if (exp & 1)
          res *= base;
        exp >>= 1;
        if (exp > 0)
          base *= base;
      }
      left.ival = res;
    } else {
      double r = pow(to_double(left), to_double(right));
      if (isnan(r) || isinf(r)) {
        set_error(op_pos);
        return left;
      }
      left.is_float = 1;
      left.fval = r;
    }
  }
  return left;
}

static Value parse_term(void) {
  Value left = parse_power();
  if (err_pos != 0)
    return left;

  while (cur_tok.type == TOK_MUL || cur_tok.type == TOK_DIV) {
    int op = cur_tok.type;
    long op_pos = cur_tok.pos;
    next_token();
    long rhs_pos = cur_tok.pos;
    Value right = parse_power();
    if (err_pos != 0)
      return left;

    if (op == TOK_MUL) {
      if (!left.is_float && !right.is_float) {
        left.ival = left.ival * right.ival;
      } else {
        double r = to_double(left) * to_double(right);
        if (isnan(r) || isinf(r)) {
          set_error(op_pos);
          return left;
        }
        left.is_float = 1;
        left.fval = r;
      }
    } else {
      if ((!right.is_float && right.ival == 0) ||
          (right.is_float && right.fval == 0.0)) {
        set_error(rhs_pos);
        return left;
      }
      if (!left.is_float && !right.is_float) {
        left.ival = left.ival / right.ival;
      } else {
        double r = to_double(left) / to_double(right);
        if (isnan(r) || isinf(r)) {
          set_error(op_pos);
          return left;
        }
        left.is_float = 1;
        left.fval = r;
      }
    }
  }
  return left;
}

static Value parse_expr(void) {
  Value left = parse_term();
  if (err_pos != 0)
    return left;

  while (cur_tok.type == TOK_PLUS || cur_tok.type == TOK_MINUS) {
    int op = cur_tok.type;
    long op_pos = cur_tok.pos;
    next_token();
    Value right = parse_term();
    if (err_pos != 0)
      return left;

    if (!left.is_float && !right.is_float) {
      if (op == TOK_PLUS)
        left.ival += right.ival;
      else
        left.ival -= right.ival;
    } else {
      double r = (op == TOK_PLUS) ? (to_double(left) + to_double(right))
                                  : (to_double(left) - to_double(right));
      if (isnan(r) || isinf(r)) {
        set_error(op_pos);
        return left;
      }
      left.is_float = 1;
      left.fval = r;
    }
  }
  return left;
}

// AI (Gemini web): prompt "how to read entire file of unknown size into memory
// in C with fread" Used dynamic buffer with realloc doubling to handle large
// multi-megabyte files without line length limit.
static char *read_entire_file(const char *path, size_t *out_size) {
  struct stat st;
  if (stat(path, &st) != 0 || !S_ISREG(st.st_mode)) {
    return NULL;
  }

  FILE *f = fopen(path, "rb");
  if (!f)
    return NULL;

  size_t cap = (st.st_size > 0) ? (size_t)st.st_size : 4096;
  char *buf = malloc(cap + 1);
  if (!buf) {
    fclose(f);
    return NULL;
  }

  size_t len = 0;
  size_t n;
  while ((n = fread(buf + len, 1, cap - len, f)) > 0) {
    len += n;
    if (len == cap) {
      size_t new_cap = cap * 2;
      char *tmp = realloc(buf, new_cap + 1);
      if (!tmp) {
        free(buf);
        fclose(f);
        return NULL;
      }
      buf = tmp;
      cap = new_cap;
    }
  }

  fclose(f);
  buf[len] = '\0';
  *out_size = len;
  return buf;
}

// Dynamic buffer to avoid strcat on big files
static int append_line(char **buf, size_t *len, size_t *cap, const char *line) {
  size_t l = strlen(line);
  if (*len + l + 1 > *cap) {
    size_t new_cap = (*cap == 0) ? 4096 : *cap * 2;
    while (new_cap < *len + l + 1)
      new_cap *= 2;
    char *tmp = realloc(*buf, new_cap);
    if (!tmp)
      return -1;
    *buf = tmp;
    *cap = new_cap;
  }
  memcpy(*buf + *len, line, l);
  *len += l;
  (*buf)[*len] = '\0';
  return 0;
}

static int process_single_file(const char *in_path, const char *out_dir) {
  size_t flen = 0;
  char *content = read_entire_file(in_path, &flen);
  if (!content) {
    fprintf(stderr, "Error: cannot read input file '%s'\n", in_path);
    return 1;
  }

  const char *slash = strrchr(in_path, '/');
  const char *fname = slash ? slash + 1 : in_path;
  size_t base_len = strlen(fname);
  if (base_len > 4 && strcmp(fname + base_len - 4, ".txt") == 0) {
    base_len -= 4;
  }

  size_t out_path_len = strlen(out_dir) + 1 + base_len + 1 +
                        strlen(STUDENT_NAME) + 1 + strlen(STUDENT_LASTNAME) +
                        1 + strlen(STUDENT_ID) + 5;
  char *out_path = malloc(out_path_len);
  if (!out_path) {
    free(content);
    fprintf(stderr, "Error: out of memory\n");
    return 1;
  }
  snprintf(out_path, out_path_len, "%s/%.*s_%s_%s_%s.txt", out_dir,
           (int)base_len, fname, STUDENT_NAME, STUDENT_LASTNAME, STUDENT_ID);

  src_buf = content;
  src_len = flen;
  src_idx = 0;
  err_pos = 0;

  char *out_buf = NULL;
  size_t out_len = 0;
  size_t out_cap = 0;

  while (src_idx < src_len) {
    while (src_idx < src_len &&
           (src_buf[src_idx] == ' ' || src_buf[src_idx] == '\t' ||
            src_buf[src_idx] == '\r')) {
      src_idx++;
    }
    if (src_idx >= src_len)
      break;

    if (src_buf[src_idx] == '\n') {
      src_idx++;
      continue;
    }

    if (src_buf[src_idx] == '#') {
      while (src_idx < src_len && src_buf[src_idx] != '\n') {
        src_idx++;
      }
      if (src_idx < src_len && src_buf[src_idx] == '\n') {
        src_idx++;
      }
      continue;
    }

    next_token();
    Value val = parse_expr();

    if (err_pos == 0 && cur_tok.type != TOK_EOL && cur_tok.type != TOK_EOF) {
      set_error(cur_tok.pos);
    }

    if (err_pos != 0) {
      break;
    }

    char line_str[128];
    if (!val.is_float) {
      snprintf(line_str, sizeof(line_str), "%lld\n", val.ival);
    } else {
      double d = val.fval;
      if (d == 0.0)
        d = 0.0;
      snprintf(line_str, sizeof(line_str), "%.15g\n", d);
    }

    if (append_line(&out_buf, &out_len, &out_cap, line_str) != 0) {
      free(out_buf);
      free(out_path);
      free(content);
      fprintf(stderr, "Error: out of memory\n");
      return 1;
    }

    if (cur_tok.type == TOK_EOL && src_idx < src_len &&
        src_buf[src_idx] == '\n') {
      src_idx++;
    }
  }

  FILE *out_f = fopen(out_path, "wb");
  if (!out_f) {
    fprintf(stderr, "Error: cannot write output file '%s'\n", out_path);
    free(out_buf);
    free(out_path);
    free(content);
    return 1;
  }

  if (err_pos != 0) {
    fprintf(out_f, "ERROR:%ld\n", err_pos);
  } else if (out_len > 0) {
    if (fwrite(out_buf, 1, out_len, out_f) != out_len) {
      fprintf(stderr, "Error: failed writing to '%s'\n", out_path);
      fclose(out_f);
      free(out_buf);
      free(out_path);
      free(content);
      return 1;
    }
  }

  fclose(out_f);
  free(out_buf);
  free(out_path);
  free(content);
  return 0;
}

static int ensure_dir_exists(const char *dir_path) {
  struct stat st;
  if (stat(dir_path, &st) == 0) {
    if (S_ISDIR(st.st_mode))
      return 0;
    fprintf(stderr, "Error: '%s' exists and is not a directory\n", dir_path);
    return -1;
  }
  if (mkdir(dir_path, 0755) != 0) {
    fprintf(stderr, "Error: cannot create output directory '%s'\n", dir_path);
    return -1;
  }
  return 0;
}

int main(int argc, char **argv) {
  const char *dir_arg = NULL;
  const char *out_arg = NULL;
  const char *file_arg = NULL;

  if (argc < 2) {
    fprintf(stderr,
            "Usage: %s [-d DIR | --dir DIR] [-o OUTDIR | --output-dir OUTDIR] "
            "[input.txt]\n",
            argv[0]);
    return 1;
  }

  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "--dir") == 0) {
      if (i + 1 >= argc || dir_arg != NULL) {
        fprintf(stderr, "Error: invalid or duplicate -d option\n");
        return 1;
      }
      dir_arg = argv[++i];
    } else if (strcmp(argv[i], "-o") == 0 ||
               strcmp(argv[i], "--output-dir") == 0) {
      if (i + 1 >= argc || out_arg != NULL) {
        fprintf(stderr, "Error: invalid or duplicate -o option\n");
        return 1;
      }
      out_arg = argv[++i];
    } else if (argv[i][0] == '-') {
      fprintf(stderr, "Error: unknown option '%s'\n", argv[i]);
      return 1;
    } else {
      if (file_arg != NULL) {
        fprintf(stderr, "Error: too many input arguments\n");
        return 1;
      }
      file_arg = argv[i];
    }
  }

  if ((dir_arg == NULL && file_arg == NULL) ||
      (dir_arg != NULL && file_arg != NULL)) {
    fprintf(stderr, "Error: specify either an input file or -d DIR\n");
    return 1;
  }

  char *default_outdir = NULL;
  const char *out_dir = out_arg;

  if (!out_dir) {
    const char *src_name = dir_arg ? dir_arg : file_arg;
    size_t len = strlen(src_name);
    while (len > 1 && src_name[len - 1] == '/') {
      len--;
    }
    const char *base = src_name;
    for (size_t i = 0; i < len; i++) {
      if (src_name[i] == '/') {
        base = src_name + i + 1;
      }
    }
    size_t base_len = (src_name + len) - base;
    if (!dir_arg && base_len > 4 &&
        strncmp(base + base_len - 4, ".txt", 4) == 0) {
      base_len -= 4;
    }

    size_t def_len =
        base_len + 1 + strlen(STUDENT_NAME) + 1 + strlen(STUDENT_ID) + 1;
    default_outdir = malloc(def_len);
    if (!default_outdir) {
      fprintf(stderr, "Error: out of memory\n");
      return 1;
    }
    snprintf(default_outdir, def_len, "%.*s_%s_%s", (int)base_len, base,
             STUDENT_NAME, STUDENT_ID);
    out_dir = default_outdir;
  }

  if (file_arg != NULL) {
    struct stat st;
    if (stat(file_arg, &st) != 0 || !S_ISREG(st.st_mode)) {
      fprintf(stderr, "Error: missing or invalid input file '%s'\n", file_arg);
      free(default_outdir);
      return 1;
    }
    if (ensure_dir_exists(out_dir) != 0) {
      free(default_outdir);
      return 1;
    }
    int rc = process_single_file(file_arg, out_dir);
    free(default_outdir);
    return rc;
  }

  // AI (Gemini web): prompt "how to iterate files in folder in C and check if
  // file is .txt and not a directory" Used opendir/readdir and S_ISREG from
  // sys/stat as suggested to filter only *.txt files.
  DIR *dp = opendir(dir_arg);
  if (!dp) {
    fprintf(stderr, "Error: cannot open directory '%s'\n", dir_arg);
    free(default_outdir);
    return 1;
  }

  if (ensure_dir_exists(out_dir) != 0) {
    closedir(dp);
    free(default_outdir);
    return 1;
  }

  struct dirent *entry;
  int status = 0;
  while ((entry = readdir(dp)) != NULL) {
    const char *name = entry->d_name;
    size_t nlen = strlen(name);
    if (nlen <= 4 || strcmp(name + nlen - 4, ".txt") != 0) {
      continue;
    }

    size_t path_len = strlen(dir_arg) + 1 + nlen + 1;
    char *full_path = malloc(path_len);
    if (!full_path) {
      fprintf(stderr, "Error: out of memory\n");
      status = 1;
      break;
    }
    snprintf(full_path, path_len, "%s/%s", dir_arg, name);

    struct stat st;
    if (stat(full_path, &st) == 0 && S_ISREG(st.st_mode)) {
      if (process_single_file(full_path, out_dir) != 0) {
        status = 1;
      }
    }
    free(full_path);
    if (status != 0)
      break;
  }

  closedir(dp);
  free(default_outdir);
  return status;
}
