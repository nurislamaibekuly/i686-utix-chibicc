// i wanna fucking kill myself there's a lots of 64 bit shit

#include "chibicc.h"

#define GP_MAX 6
#define FP_MAX 8

static FILE *output_file;
static int depth;
static char *argreg8[] = {"%al", "%dl", "%cl"};
static char *argreg16[] = {"%ax", "%dx", "%cx"};
static char *argreg32[] = {"%eax", "%edx", "%ecx"};
static Obj *current_fn;

static void gen_expr(Node *node);
static void gen_stmt(Node *node);

__attribute__((format(printf, 1, 2)))
static void println(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(output_file, fmt, ap);
  va_end(ap);
  fprintf(output_file, "\n");
}

static int count(void) {
  static int i = 1;
  return i++;
}

static void push(void) {
  println("  push %%eax");
  depth++;
}

static void pop(char *arg) {
  println("  pop %s", arg);
  depth--;
}

static void pushf(void) {
  println("  sub $8, %%esp");
  println("  movsd %%xmm0, (%%esp)");
  depth++;
}

static void popf(int reg) {
  println("  movsd (%%esp), %%xmm%d", reg);
  println("  add $8, %%esp");
  depth--;
}

int align_to(int n, int align) {
  return (n + align - 1) / align * align;
}

static char *reg_ax(int sz) {
  switch (sz) {
  case 1: return "%al";
  case 2: return "%ax";
  case 4: return "%eax";
  }
  unreachable();
}

static char *reg_dx(int sz) {
  switch (sz) {
  case 1: return "%dl";
  case 2: return "%dx";
  case 4: return "%edx";
  }
  unreachable();
}

static void gen_addr(Node *node) {
  switch (node->kind) {
  case ND_VAR:
      if (node->var->ty->kind == TY_VLA) {
        println("  mov %d(%%ebp), %%eax", node->var->offset);
        return;
      }

      if (node->var->is_local) {
        println("  lea %d(%%ebp), %%eax", node->var->offset);  // Always use lea here
        return;
      }

      println("  lea %s, %%eax", node->var->name);
      return;
  case ND_DEREF:
    gen_expr(node->lhs);
    return;
  case ND_COMMA:
    gen_expr(node->lhs);
    gen_addr(node->rhs);
    return;
  case ND_MEMBER:
    gen_addr(node->lhs);
    println("  add $%d, %%eax", node->member->offset);
    return;
  case ND_FUNCALL:
    if (node->ret_buffer) {
      gen_expr(node);
      return;
    }
    break;
  case ND_ASSIGN:
  case ND_COND:
    if (node->ty->kind == TY_STRUCT || node->ty->kind == TY_UNION) {
      gen_expr(node);
      return;
    }
    break;
  case ND_VLA_PTR:
    println("  lea %d(%%ebp), %%eax", node->var->offset);
    return;
  }

  error_tok(node->tok, "not an lvalue");
}

static void load(Type *ty) {
  switch (ty->kind) {
  case TY_ARRAY:
  case TY_STRUCT:
  case TY_UNION:
  case TY_FUNC:
  case TY_VLA:
    return;
  case TY_FLOAT:
    println("  movss (%%eax), %%xmm0");
    return;
  case TY_DOUBLE:
    println("  movsd (%%eax), %%xmm0");
    return;
  case TY_LDOUBLE:
    println("  fldt (%%eax)");
    return;
  }

  char *insn = ty->is_unsigned ? "movz" : "movs";

  if (ty->size == 1)
    println("  %sbl (%%eax), %%eax", insn);
  else if (ty->size == 2)
    println("  %swl (%%eax), %%eax", insn);
  else if (ty->size == 4)
    println("  mov (%%eax), %%eax");
}

static void store(Type *ty) {
  pop("%edi");

  switch (ty->kind) {
  case TY_STRUCT:
  case TY_UNION:
    for (int i = 0; i < ty->size; i++) {
      println("  mov %d(%%eax), %%cl", i);
      println("  mov %%cl, %d(%%edi)", i);
    }
    return;
  case TY_FLOAT:
    println("  movss %%xmm0, (%%edi)");
    return;
  case TY_DOUBLE:
    println("  movsd %%xmm0, (%%edi)");
    return;
  case TY_LDOUBLE:
    println("  fstpt (%%edi)");
    return;
  }

  if (ty->size == 1)
    println("  mov %%al, (%%edi)");
  else if (ty->size == 2)
    println("  mov %%ax, (%%edi)");
  else // ty->size >= 4, treat as 32-bit for i386
    println("  mov %%eax, (%%edi)");
}

static void cmp_zero(Type *ty) {
  switch (ty->kind) {
  case TY_FLOAT:
    println("  xorps %%xmm1, %%xmm1");
    println("  ucomiss %%xmm1, %%xmm0");
    return;
  case TY_DOUBLE:
    println("  xorpd %%xmm1, %%xmm1");
    println("  ucomisd %%xmm1, %%xmm0");
    return;
  case TY_LDOUBLE:
    println("  fldz");
    println("  fucomip");
    println("  fstp %%st(0)");
    return;
  }

  println("  cmp $0, %%eax");
}

enum { I8, I16, I32, I64, U8, U16, U32, U64, F32, F64, F80 };

static int getTypeId(Type *ty) {
  switch (ty->kind) {
  case TY_CHAR:
    return ty->is_unsigned ? U8 : I8;
  case TY_SHORT:
    return ty->is_unsigned ? U16 : I16;
  case TY_INT:
    return ty->is_unsigned ? U32 : I32;
  case TY_LONG:
    return ty->is_unsigned ? U32 : I32;
  case TY_FLOAT:
    return F32;
  case TY_DOUBLE:
    return F64;
  case TY_LDOUBLE:
    return F80;
  }
  return U32; // fuck u64
}

static char i32i8[] = "movsbl %al, %eax";
static char i32u8[] = "movzbl %al, %eax";
static char i32i16[] = "movswl %ax, %eax";
static char i32u16[] = "movzwl %ax, %eax";
static char i32f32[] = "cvtsi2ssl %eax, %xmm0";
static char i32i64[] = "movsxd %eax, %eax";
static char i32f64[] = "cvtsi2sdl %eax, %xmm0";
static char i32f80[] = "mov %eax, -4(%esp); fildl -4(%esp)";

static char u32f32[] = "mov %eax, %eax; cvtsi2ssq %eax, %xmm0";
static char u32i64[] = "mov %eax, %eax";
static char u32f64[] = "mov %eax, %eax; cvtsi2sdq %eax, %xmm0";
static char u32f80[] = "mov %eax, %eax; mov %eax, -8(%esp); fildll -8(%esp)";

static char i64f32[] = "cvtsi2ssq %eax, %xmm0";
static char i64f64[] = "cvtsi2sdq %eax, %xmm0";
static char i64f80[] = "movq %eax, -8(%esp); fildll -8(%esp)";

static char u64f32[] = "cvtsi2ssq %eax, %xmm0";
static char u64f64[] =
  "test %eax,%eax; js 1f; pxor %xmm0,%xmm0; cvtsi2sd %eax,%xmm0; jmp 2f; "
  "1: mov %eax,%edi; and $1,%eax; pxor %xmm0,%xmm0; shr %edi; "
  "or %eax,%edi; cvtsi2sd %edi,%xmm0; addsd %xmm0,%xmm0; 2:";
static char u64f80[] =
  "mov %eax, -8(%esp); fildq -8(%esp); test %eax, %eax; jns 1f;"
  "mov $1602224128, %eax; mov %eax, -4(%esp); fadds -4(%esp); 1:";

static char f32i8[] = "cvttss2sil %xmm0, %eax; movsbl %al, %eax";
static char f32u8[] = "cvttss2sil %xmm0, %eax; movzbl %al, %eax";
static char f32i16[] = "cvttss2sil %xmm0, %eax; movswl %ax, %eax";
static char f32u16[] = "cvttss2sil %xmm0, %eax; movzwl %ax, %eax";
static char f32i32[] = "cvttss2sil %xmm0, %eax";
static char f32u32[] = "cvttss2siq %xmm0, %eax";
static char f32i64[] = "cvttss2siq %xmm0, %eax";
static char f32u64[] = "cvttss2siq %xmm0, %eax";
static char f32f64[] = "cvtss2sd %xmm0, %xmm0";
static char f32f80[] = "movss %xmm0, -4(%esp); flds -4(%esp)";

static char f64i8[] = "cvttsd2sil %xmm0, %eax; movsbl %al, %eax";
static char f64u8[] = "cvttsd2sil %xmm0, %eax; movzbl %al, %eax";
static char f64i16[] = "cvttsd2sil %xmm0, %eax; movswl %ax, %eax";
static char f64u16[] = "cvttsd2sil %xmm0, %eax; movzwl %ax, %eax";
static char f64i32[] = "cvttsd2sil %xmm0, %eax";
static char f64u32[] = "cvttsd2siq %xmm0, %eax";
static char f64i64[] = "cvttsd2siq %xmm0, %eax";
static char f64u64[] = "cvttsd2siq %xmm0, %eax";
static char f64f32[] = "cvtsd2ss %xmm0, %xmm0";
static char f64f80[] = "movsd %xmm0, -8(%esp); fldl -8(%esp)";

#define FROM_F80_1                                           \
  "fnstcw -10(%esp); movzwl -10(%esp), %eax; or $12, %ah; " \
  "mov %ax, -12(%esp); fldcw -12(%esp); "

#define FROM_F80_2 " -24(%esp); fldcw -10(%esp); "

static char f80i8[] = FROM_F80_1 "fistps" FROM_F80_2 "movsbl -24(%esp), %eax";
static char f80u8[] = FROM_F80_1 "fistps" FROM_F80_2 "movzbl -24(%esp), %eax";
static char f80i16[] = FROM_F80_1 "fistps" FROM_F80_2 "movzbl -24(%esp), %eax";
static char f80u16[] = FROM_F80_1 "fistpl" FROM_F80_2 "movswl -24(%esp), %eax";
static char f80i32[] = FROM_F80_1 "fistpl" FROM_F80_2 "mov -24(%esp), %eax";
static char f80u32[] = FROM_F80_1 "fistpl" FROM_F80_2 "mov -24(%esp), %eax";
static char f80i64[] = FROM_F80_1 "fistpq" FROM_F80_2 "mov -24(%esp), %eax";
static char f80u64[] = FROM_F80_1 "fistpq" FROM_F80_2 "mov -24(%esp), %eax";
static char f80f32[] = "fstps -8(%esp); movss -8(%esp), %xmm0";
static char f80f64[] = "fstpl -8(%esp); movsd -8(%esp), %xmm0";

static char *cast_table[][11] = {
  {NULL,  NULL,   NULL,   i32i64, i32u8, i32u16, NULL,   i32i64, i32f32, i32f64, i32f80},
  {i32i8, NULL,   NULL,   i32i64, i32u8, i32u16, NULL,   i32i64, i32f32, i32f64, i32f80},
  {i32i8, i32i16, NULL,   i32i64, i32u8, i32u16, NULL,   i32i64, i32f32, i32f64, i32f80},
  {i32i8, i32i16, NULL,   NULL,   i32u8, i32u16, NULL,   NULL,   i64f32, i64f64, i64f80},

  {i32i8, NULL,   NULL,   i32i64, NULL,  NULL,   NULL,   i32i64, i32f32, i32f64, i32f80},
  {i32i8, i32i16, NULL,   i32i64, i32u8, NULL,   NULL,   i32i64, i32f32, i32f64, i32f80},
  {i32i8, i32i16, NULL,   u32i64, i32u8, i32u16, NULL,   u32i64, u32f32, u32f64, u32f80},
  {i32i8, i32i16, NULL,   NULL,   i32u8, i32u16, NULL,   NULL,   u64f32, u64f64, u64f80},

  {f32i8, f32i16, f32i32, f32i64, f32u8, f32u16, f32u32, f32u64, NULL,   f32f64, f32f80},
  {f64i8, f64i16, f64i32, f64i64, f64u8, f64u16, f64u32, f64u64, f64f32, NULL,   f64f80},
  {f80i8, f80i16, f80i32, f80i64, f80u8, f80u16, f80u32, f80u64, f80f32, f80f64, NULL},
};

static void cast(Type *from, Type *to) {
  if (to->kind == TY_VOID)
    return;

  if (to->kind == TY_BOOL) {
    cmp_zero(from);
    println("  setne %%al");
    println("  movzx %%al, %%eax");
    return;
  }

  int t1 = getTypeId(from);
  int t2 = getTypeId(to);
  if (cast_table[t1][t2])
    println("  %s", cast_table[t1][t2]);
}

// Structs or unions equal or smaller than 16 bytes are passed
// using up to two registers.
//
// If the first 8 bytes contains only floating-point type members,
// they are passed in an XMM register. Otherwise, they are passed
// in a general-purpose register.
//
// If a struct/union is larger than 8 bytes, the same rule is
// applied to the the next 8 byte chunk.
//
// This function returns true if `ty` has only floating-point
// members in its byte range [lo, hi). -rui314

// erm actually this has to be a i686 compiler not a fucking x86_64 bit shit

static bool has_flonum(Type *ty, int lo, int hi, int offset) {
  if (ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
    for (Member *mem = ty->members; mem; mem = mem->next)
      if (!has_flonum(mem->ty, lo, hi, offset + mem->offset))
        return false;
    return true;
  }

  if (ty->kind == TY_ARRAY) {
    for (int i = 0; i < ty->array_len; i++)
      if (!has_flonum(ty->base, lo, hi, offset + ty->base->size * i))
        return false;
    return true;
  }

  return offset < lo || hi <= offset || ty->kind == TY_FLOAT || ty->kind == TY_DOUBLE;
}

static bool has_flonum1(Type *ty) {
  return has_flonum(ty, 0, 8, 0);
}

static bool has_flonum2(Type *ty) {
  return has_flonum(ty, 8, 16, 0);
}

static void push_struct(Type *ty) {
  int sz = align_to(ty->size, 4);
  println("  sub $%d, %%esp", sz);
  depth += sz / 4;

  for (int i = 0; i < ty->size; i++) {
    println("  mov %d(%%eax), %%cl", i);
    println("  mov %%cl, %d(%%esp)", i);
  }
}

static void push_args2(Node *args, bool first_pass) {
  if (!args)
    return;
  push_args2(args->next, first_pass);

  if ((first_pass && !args->pass_by_stack) || (!first_pass && args->pass_by_stack))
    return;

  gen_expr(args);

  switch (args->ty->kind) {
  case TY_STRUCT:
  case TY_UNION:
    push_struct(args->ty);
    break;
  case TY_FLOAT:
  case TY_DOUBLE:
    pushf();
    break;
  case TY_LDOUBLE:
    println("  sub $16, %%esp");
    println("  fstpt (%%esp)");
    depth += 2;
    break;
  default:
    push();
  }
}

static void push_args_recursive(Node *args, bool dummy) {
  if (!args)
    return;

  push_args_recursive(args->next, dummy);
  gen_expr(args);

  switch (args->ty->kind) {
  case TY_STRUCT:
  case TY_UNION:
    push_struct(args->ty);
    break;
  case TY_FLOAT:
  case TY_DOUBLE:
    pushf();
    break;
  case TY_LDOUBLE:
    println("  sub $12, %%esp");
    println("  fstpt (%%esp)");
    depth += 3;
    break;
  default:
    push();
  }
}

// Load function call arguments. Arguments are already evaluated and
// stored to the stack as local variables. What we need to do in this
// function is to load them to registers or push them to the stack as
// specified by the x86-64 psABI. Here is what the spec says:
//
// - Up to 6 arguments of integral type are passed using edi, RSI,
//   edx, ecx, R8 and R9.
//
// - Up to 8 arguments of floating-point type are passed using XMM0 to
//   XMM7.
//
// - If all registers of an appropriate type are already used, push an
//   argument to the stack in the right-to-left order.
//
// - Each argument passed on the stack takes 8 bytes, and the end of
//   the argument area must be aligned to a 16 byte boundary.
//
// - If a function is variadic, set the number of floating-point type
//   arguments to eax. -rui314

// yeah yeah rui314 we get it

static int push_args(Node *node) {
	int stack = 0;

  for (Node *arg = node->args; arg; arg = arg->next) {
    Type *ty = arg->ty;

    switch (ty->kind) {
    case TY_STRUCT:
    case TY_UNION:
      stack += align_to(ty->size, 4) / 4;
      break;
    case TY_FLOAT:
    case TY_DOUBLE:
      stack += (ty->size + 3) / 4;
      break;
    case TY_LDOUBLE:
      stack += 3;
      break;
    default:
      stack++;  // 4 bytes for pointer/int
    }
  }

  if ((depth + stack) % 4 != 0) {
    int padding = 4 - ((depth + stack) % 4);
    println("  sub $%d, %%esp", padding * 4);
    depth += padding;
    stack += padding;
  }

  push_args_recursive(node->args, true);

  return stack;
}

static void copy_ret_buffer(Obj *var) {
  Type *ty = var->ty;
  int gp = 0, fp = 0;

  if (has_flonum1(ty)) {
    assert(ty->size == 4 || 8 <= ty->size);
    if (ty->size == 4)
      println("  movss %%xmm0, %d(%%ebp)", var->offset);
    else
      println("  movsd %%xmm0, %d(%%ebp)", var->offset);
    fp++;
  } else {
    for (int i = 0; i < MIN(8, ty->size); i++) {
      println("  mov %%al, %d(%%ebp)", var->offset + i);
      println("  shr $8, %%eax");
    }
    gp++;
  }

  if (ty->size > 8) {
    if (has_flonum2(ty)) {
      assert(ty->size == 12 || ty->size == 16);
      if (ty->size == 12)
        println("  movss %%xmm%d, %d(%%ebp)", fp, var->offset + 8);
      else
        println("  movsd %%xmm%d, %d(%%ebp)", fp, var->offset + 8);
    } else {
      char *reg1 = (gp == 0) ? "%al" : "%dl";
      char *reg2 = (gp == 0) ? "%eax" : "%edx";
      for (int i = 8; i < MIN(16, ty->size); i++) {
        println("  mov %s, %d(%%ebp)", reg1, var->offset + i);
        println("  shr $8, %s", reg2);
      }
    }
  }
}

static void copy_struct_reg(void) {
  Type *ty = current_fn->ty->return_ty;
  int gp = 0, fp = 0;

  println("  mov %%eax, %%edi");

  if (has_flonum(ty, 0, 8, 0)) {
    assert(ty->size == 4 || 8 <= ty->size);
    if (ty->size == 4)
      println("  movss (%%edi), %%xmm0");
    else
      println("  movsd (%%edi), %%xmm0");
    fp++;
  } else {
    println("  mov $0, %%eax");
    for (int i = MIN(8, ty->size) - 1; i >= 0; i--) {
      println("  shl $8, %%eax");
      println("  mov %d(%%edi), %%al", i);
    }
    gp++;
  }

  if (ty->size > 8) {
    if (has_flonum(ty, 8, 16, 0)) {
      assert(ty->size == 12 || ty->size == 16);
      if (ty->size == 4)
        println("  movss 8(%%edi), %%xmm%d", fp);
      else
        println("  movsd 8(%%edi), %%xmm%d", fp);
    } else {
      char *reg1 = (gp == 0) ? "%al" : "%dl";
      char *reg2 = (gp == 0) ? "%eax" : "%edx";
      println("  mov $0, %s", reg2);
      for (int i = MIN(16, ty->size) - 1; i >= 8; i--) {
        println("  shl $8, %s", reg2);
        println("  mov %d(%%edi), %s", i, reg1);
      }
    }
  }
}

static void copy_struct_mem(void) {
  Type *ty = current_fn->ty->return_ty;
  Obj *var = current_fn->params;

  println("  mov %d(%%ebp), %%edi", var->offset);

  for (int i = 0; i < ty->size; i++) {
    println("  mov %d(%%eax), %%dl", i);
    println("  mov %%dl, %d(%%edi)", i);
  }
}

static void builtin_alloca(void) {
  // assign it to 16 bytessssssswqweoip2
  println("  add $15, %%edi");
  println("  and $0xfffffff0, %%edi");

  println("  mov %d(%%ebp), %%ecx", current_fn->alloca_bottom->offset);
  println("  sub %%esp, %%ecx");
  println("  mov %%esp, %%eax");
  println("  sub %%edi, %%esp");
  println("  mov %%esp, %%edx");
  println("1:");
  println("  cmp $0, %%ecx");
  println("  je 2f");
  println("  mov (%%eax), %%r8b");
  println("  mov %%r8b, (%%edx)");
  println("  inc %%edx");
  println("  inc %%eax");
  println("  dec %%ecx");
  println("  jmp 1b");
  println("2:");

  // move motherfucker
  println("  mov %d(%%ebp), %%eax", current_fn->alloca_bottom->offset);
  println("  sub %%edi, %%eax");
  println("  mov %%eax, %d(%%ebp)", current_fn->alloca_bottom->offset);
}

static void gen_expr(Node *node) {
  println("  .loc %d %d", node->tok->file->file_no, node->tok->line_no);

  switch (node->kind) {
  case ND_NULL_EXPR:
    return;
    case ND_NUM: {
      switch (node->ty->kind) {
      case TY_FLOAT: {
        union { float f32; uint32_t u32; } u = { node->fval };
        println("  mov $%u, %%eax  # float %Lf", u.u32, node->fval);
        println("  movd %%eax, %%xmm0");
        return;
      }
      case TY_DOUBLE: {
        union { double f64; uint64_t u64; } u = { node->fval };
        println("  push $%lu  # double %Lf", (unsigned long)(u.u64 >> 32), node->fval);
        println("  push $%lu", (unsigned long)(u.u64 & 0xFFFFFFFF));
        println("  movsd (%%esp), %%xmm0");
        println("  add $8, %%esp");
        return;
      }
      case TY_LDOUBLE: {
        union { long double f80; uint64_t u64[2]; } u;
        memset(&u, 0, sizeof(u));
        u.f80 = node->fval;
        println("  push $%lu  # long double %Lf", (unsigned long)(u.u64[1] >> 32), node->fval);
        println("  push $%lu", (unsigned long)(u.u64[1] & 0xFFFFFFFF));
        println("  push $%lu", (unsigned long)(u.u64[0] >> 32));
        println("  push $%lu", (unsigned long)(u.u64[0] & 0xFFFFFFFF));
        println("  fldt (%%esp)");
        println("  add $16, %%esp");
        return;
      }
      }

      println("  mov $%d, %%eax", (int)node->val);
      return;
    }
    case ND_NEG:
        gen_expr(node->lhs);

        switch (node->ty->kind) {
        case TY_FLOAT:
          println("  mov $0x80000000, %%eax");
          println("  movd %%eax, %%xmm1");
          println("  xorps %%xmm1, %%xmm0");
          return;
        case TY_DOUBLE:
          println("  push $0x80000000");
          println("  push $0");
          println("  movsd (%%esp), %%xmm1");
          println("  add $8, %%esp");
          println("  xorpd %%xmm1, %%xmm0");
          return;
        case TY_LDOUBLE:
          println("  fchs");
          return;
        }

    println("  neg %%eax");
    return;
  case ND_VAR:
    gen_addr(node);
    load(node->ty);
    return;
  case ND_MEMBER: {
    gen_addr(node);
    load(node->ty);

    Member *mem = node->member;
       if (mem->is_bitfield) {
         println("  shl $%d, %%eax", 32 - mem->bit_width - mem->bit_offset);
         if (mem->ty->is_unsigned)
           println("  shr $%d, %%eax", 32 - mem->bit_width);
         else
           println("  sar $%d, %%eax", 32 - mem->bit_width);
       }
    return;
  }
  case ND_DEREF:
      gen_expr(node->lhs);
      load(node->ty);
      return;
  case ND_ADDR:
    gen_addr(node->lhs);
    return;
  case ND_ASSIGN:
    gen_addr(node->lhs);
    push();
    gen_expr(node->rhs);

    if (node->lhs->kind == ND_MEMBER && node->lhs->member->is_bitfield) {
          println("  mov %%eax, %%esi");  // r8 fuck you

          Member *mem = node->lhs->member;
          println("  mov %%eax, %%ecx");  // rdi fuck you
          println("  and $%d, %%ecx", (1 << mem->bit_width) - 1);
          println("  shl $%d, %%ecx", mem->bit_offset);

          println("  mov (%%esp), %%eax");
          load(mem->ty);

          int mask = ((1 << mem->bit_width) - 1) << mem->bit_offset;
          println("  and $%d, %%eax", ~mask);
          println("  or %%ecx, %%eax");
          store(node->ty);
          println("  mov %%esi, %%eax");
          return;
        }

    store(node->ty);
    return;
  case ND_STMT_EXPR:
    for (Node *n = node->body; n; n = n->next)
      gen_stmt(n);
    return;
  case ND_COMMA:
    gen_expr(node->lhs);
    gen_expr(node->rhs);
    return;
  case ND_CAST:
    gen_expr(node->lhs);
    cast(node->lhs->ty, node->ty);
    return;
    case ND_MEMZERO:
        println("  mov $%d, %%ecx", node->var->ty->size);
        println("  lea %d(%%ebp), %%edi", node->var->offset);
        println("  mov $0, %%al");
        println("  rep stosb");
        return;

  case ND_COND: {
    int c = count();
    gen_expr(node->cond);
    cmp_zero(node->cond->ty);
    println("  je .L.else.%d", c);
    gen_expr(node->then);
    println("  jmp .L.end.%d", c);
    println(".L.else.%d:", c);
    gen_expr(node->els);
    println(".L.end.%d:", c);
    return;
  }
  case ND_NOT:
    gen_expr(node->lhs);
    cmp_zero(node->lhs->ty);
    println("  sete %%al");
    println("  movzx %%al, %%eax");
        return;

  case ND_BITNOT:
    gen_expr(node->lhs);
    println("  not %%eax");
    return;
  case ND_LOGAND: {
    int c = count();
    gen_expr(node->lhs);
    cmp_zero(node->lhs->ty);
    println("  je .L.false.%d", c);
    gen_expr(node->rhs);
    cmp_zero(node->rhs->ty);
    println("  je .L.false.%d", c);
    println("  mov $1, %%eax");
    println("  jmp .L.end.%d", c);
    println(".L.false.%d:", c);
    println("  mov $0, %%eax");
    println(".L.end.%d:", c);
    return;
  }
  case ND_LOGOR: {
    int c = count();
    gen_expr(node->lhs);
    cmp_zero(node->lhs->ty);
    println("  jne .L.true.%d", c);
    gen_expr(node->rhs);
    cmp_zero(node->rhs->ty);
    println("  jne .L.true.%d", c);
    println("  mov $0, %%eax");
    println("  jmp .L.end.%d", c);
    println(".L.true.%d:", c);
    println("  mov $1, %%eax");
    println(".L.end.%d:", c);
    return;
  }
  case ND_FUNCALL: {
    if (node->lhs->kind == ND_VAR && !strcmp(node->lhs->var->name, "alloca")) {
      gen_expr(node->args);
      builtin_alloca();
      return;
    }

    int stack_args = push_args(node);
    gen_expr(node->lhs);

    println("  call *%%eax");
    println("  add $%d, %%esp", stack_args * 4);  // clean up bitch

    depth -= stack_args;

    // as it ok i'm chibicc
    switch (node->ty->kind) {
    case TY_BOOL:
      println("  movzx %%al, %%eax");
      return;
    case TY_CHAR:
      if (node->ty->is_unsigned)
        println("  movzbl %%al, %%eax");
      else
        println("  movsbl %%al, %%eax");
      return;
    case TY_SHORT:
      if (node->ty->is_unsigned)
        println("  movzwl %%ax, %%eax");
      else
        println("  movswl %%ax, %%eax");
      return;
    }

    return;
  }

  case ND_LABEL_VAL:
    println("  lea %s(%%rip), %%eax", node->unique_label);
    return;
  case ND_CAS: {
    gen_expr(node->cas_addr);
    push();
    gen_expr(node->cas_new);
    push();
    gen_expr(node->cas_old);
    println("  mov %%eax, %%r8");
    load(node->cas_old->ty->base);
    pop("%edx");
    pop("%edi");

    int sz = node->cas_addr->ty->base->size;
    println("  lock cmpxchg %s, (%%edi)", reg_dx(sz));
    println("  sete %%cl");
    println("  je 1f");
    println("  mov %s, (%%r8)", reg_ax(sz));
    println("1:");
    println("  movzbl %%cl, %%eax");
    return;
  }
  case ND_EXCH: {
	  gen_expr(node->rhs);
	  push();
	  gen_expr(node->lhs);
	  pop("%edi");

    int sz = node->lhs->ty->base->size;
    println("  xchg %s, (%%edi)", reg_ax(sz));
    return;
  }
  }

  switch (node->lhs->ty->kind) {
  case TY_FLOAT:
  case TY_DOUBLE: {
    gen_expr(node->rhs);
    pushf();
    gen_expr(node->lhs);
    popf(1);

    char *sz = (node->lhs->ty->kind == TY_FLOAT) ? "ss" : "sd";

    switch (node->kind) {
    case ND_ADD:
      println("  add%s %%xmm1, %%xmm0", sz);
      return;
    case ND_SUB:
      println("  sub%s %%xmm1, %%xmm0", sz);
      return;
    case ND_MUL:
      println("  mul%s %%xmm1, %%xmm0", sz);
      return;
    case ND_DIV:
      println("  div%s %%xmm1, %%xmm0", sz);
      return;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
      println("  ucomi%s %%xmm0, %%xmm1", sz);

      if (node->kind == ND_EQ) {
        println("  sete %%al");
        println("  setnp %%dl");
        println("  and %%dl, %%al");
      } else if (node->kind == ND_NE) {
        println("  setne %%al");
        println("  setp %%dl");
        println("  or %%dl, %%al");
      } else if (node->kind == ND_LT) {
        println("  seta %%al");
      } else {
        println("  setae %%al");
      }

      println("  and $1, %%al");
      println("  movzb %%al, %%eax");
      return;
    }

    error_tok(node->tok, "invalid expression");
  }
  case TY_LDOUBLE: {
    gen_expr(node->lhs);
    gen_expr(node->rhs);

    switch (node->kind) {
    case ND_ADD:
      println("  faddp");
      return;
    case ND_SUB:
      println("  fsubrp");
      return;
    case ND_MUL:
      println("  fmulp");
      return;
    case ND_DIV:
      println("  fdivrp");
      return;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
      println("  fcomip");
      println("  fstp %%st(0)");

      if (node->kind == ND_EQ)
        println("  sete %%al");
      else if (node->kind == ND_NE)
        println("  setne %%al");
      else if (node->kind == ND_LT)
        println("  seta %%al");
      else
        println("  setae %%al");

      println("  movzb %%al, %%eax");
      return;
    }

    error_tok(node->tok, "invalid expression");
  }
  }

  char *ax, *di, *dx;

  if (node->lhs->ty->kind == TY_LONG || node->lhs->ty->base) {
    ax = "%eax";
    di = "%edi";
    dx = "%edx";
  } else {
    ax = "%eax";
    di = "%edi";
    dx = "%edx";
  }

  gen_expr(node->rhs);
  push();
  gen_expr(node->lhs);
  pop("%edi");

  switch (node->kind) {
  case ND_ADD:
      println("  add %%edi, %%eax");
      return;
  case ND_SUB:
    println("  sub %%edi, %%eax");
    return;
  case ND_MUL:
    println("  imul %%edi, %%eax");
    return;
  case ND_DIV:
  case ND_MOD:
    if (node->ty->is_unsigned) {
      println("  mov $0, %%edx");
      println("  div %%edi");
    } else {
      println("  cdq");
      println("  idiv %%edi");
    }

    if (node->kind == ND_MOD)
      println("  mov %%edx, %%eax");
    return;
  case ND_BITAND:
    println("  and %s, %s", di, ax);
    return;
  case ND_BITOR:
    println("  or %s, %s", di, ax);
    return;
  case ND_BITXOR:
    println("  xor %s, %s", di, ax);
    return;
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
    println("  cmp %s, %s", di, ax);

    if (node->kind == ND_EQ) {
      println("  sete %%al");
    } else if (node->kind == ND_NE) {
      println("  setne %%al");
    } else if (node->kind == ND_LT) {
      if (node->lhs->ty->is_unsigned)
        println("  setb %%al");
      else
        println("  setl %%al");
    } else if (node->kind == ND_LE) {
      if (node->lhs->ty->is_unsigned)
        println("  setbe %%al");
      else
        println("  setle %%al");
    }

    println("  movzb %%al, %%eax");
    return;
  case ND_SHL:
    println("  mov %%edi, %%ecx");
    println("  shl %%cl, %s", ax);
    return;
  case ND_SHR:
    println("  mov %%edi, %%ecx");
    if (node->lhs->ty->is_unsigned)
      println("  shr %%cl, %s", ax);
    else
      println("  sar %%cl, %s", ax);
    return;
  }

  error_tok(node->tok, "invalid expression");
}

static void gen_stmt(Node *node) {
  println("  .loc %d %d", node->tok->file->file_no, node->tok->line_no);

  switch (node->kind) {
  case ND_IF: {
    int c = count();
    gen_expr(node->cond);
    cmp_zero(node->cond->ty);
    println("  je  .L.else.%d", c);
    gen_stmt(node->then);
    println("  jmp .L.end.%d", c);
    println(".L.else.%d:", c);
    if (node->els)
      gen_stmt(node->els);
    println(".L.end.%d:", c);
    return;
  }
  case ND_FOR: {
    int c = count();
    if (node->init)
      gen_stmt(node->init);
    println(".L.begin.%d:", c);
    if (node->cond) {
      gen_expr(node->cond);
      cmp_zero(node->cond->ty);
      println("  je %s", node->brk_label);
    }
    gen_stmt(node->then);
    println("%s:", node->cont_label);
    if (node->inc)
      gen_expr(node->inc);
    println("  jmp .L.begin.%d", c);
    println("%s:", node->brk_label);
    return;
  }
  case ND_DO: {
    int c = count();
    println(".L.begin.%d:", c);
    gen_stmt(node->then);
    println("%s:", node->cont_label);
    gen_expr(node->cond);
    cmp_zero(node->cond->ty);
    println("  jne .L.begin.%d", c);
    println("%s:", node->brk_label);
    return;
  }
  case ND_SWITCH:
    gen_expr(node->cond);

    for (Node *n = node->case_next; n; n = n->case_next) {
	  char *ax = "%eax";
	  char *di = "%edi";
	  char *dx = "%edx";

      if (n->begin == n->end) {
        println("  cmp $%ld, %s", n->begin, ax);
        println("  je %s", n->label);
        continue;
      }

      println("  mov %s, %s", ax, di);
      println("  sub $%ld, %s", n->begin, di);
      println("  cmp $%ld, %s", n->end - n->begin, di);
      println("  jbe %s", n->label);
    }

    if (node->default_case)
      println("  jmp %s", node->default_case->label);

    println("  jmp %s", node->brk_label);
    gen_stmt(node->then);
    println("%s:", node->brk_label);
    return;
  case ND_CASE:
    println("%s:", node->label);
    gen_stmt(node->lhs);
    return;
  case ND_BLOCK:
    for (Node *n = node->body; n; n = n->next)
      gen_stmt(n);
    return;
  case ND_GOTO:
    println("  jmp %s", node->unique_label);
    return;
  case ND_GOTO_EXPR:
    gen_expr(node->lhs);
    println("  jmp *%%eax");
    return;
  case ND_LABEL:
    println("%s:", node->unique_label);
    gen_stmt(node->lhs);
    return;
  case ND_RETURN:
    if (node->lhs) {
      gen_expr(node->lhs);
      Type *ty = node->lhs->ty;

      switch (ty->kind) {
      case TY_STRUCT:
      case TY_UNION:
        if (ty->size <= 16)
          copy_struct_reg();
        else
          copy_struct_mem();
        break;
      }
    }

    println("  jmp .L.return.%s", current_fn->name);
    return;
  case ND_EXPR_STMT:
    gen_expr(node->lhs);
    return;
  case ND_ASM:
    println("  %s", node->asm_str);
    return;
  }

  error_tok(node->tok, "invalid statement");
}

static void assign_lvar_offsets(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function)
      continue;

    // ngh
    int top = 8;
    int bottom = 0;

    for (Obj *var = fn->params; var; var = var->next) {
      top = align_to(top, MAX(4, var->ty->align));
      var->offset = top;
      top += align_to(var->ty->size, 4);
    }

    // erm actually, local variables get negative offsets
    for (Obj *var = fn->locals; var; var = var->next) {
      if (var->offset)
        continue;

      int align = (var->ty->kind == TY_ARRAY && var->ty->size >= 16)
        ? MAX(16, var->align) : var->align;

      bottom += var->ty->size;
      bottom = align_to(bottom, align);
      var->offset = -bottom;
    }

    fn->stack_size = align_to(bottom, 16);
  }
}

static void emit_data(Obj *prog) {
  for (Obj *var = prog; var; var = var->next) {
    if (var->is_function || !var->is_definition)
      continue;

    if (var->is_static)
      println("  .local %s", var->name);
    else
      println("  .globl %s", var->name);

    int align = (var->ty->kind == TY_ARRAY && var->ty->size >= 16)
      ? MAX(16, var->align) : var->align;

    // Common symbol
    if (opt_fcommon && var->is_tentative) {
      println("  .comm %s, %d, %d", var->name, var->ty->size, align);
      continue;
    }

    // use .data or .tdata
    if (var->init_data) {
      if (var->is_tls)
        println("  .section .tdata,\"awT\",@progbits");
      else
        println("  .data");

      println("  .type %s, @object", var->name);
      println("  .size %s, %d", var->name, var->ty->size);
      println("  .align %d", align);
      println("%s:", var->name);

      Relocation *rel = var->rel;
      int pos = 0;
      while (pos < var->ty->size) {
        if (rel && rel->offset == pos) {
          println("  .long %s%+ld", *rel->label, rel->addend);
          rel = rel->next;
          pos += 8;
        } else {
          println("  .byte %d", var->init_data[pos++]);
        }
      }
      continue;
    }

    if (var->is_tls)
      println("  .section .tbss,\"awT\",@nobits");
    else
      println("  .bss");

    println("  .align %d", align);
    println("%s:", var->name);
    println("  .zero %d", var->ty->size);
  }
}

static void store_fp(int r, int offset, int sz) {
  switch (sz) {
  case 4:
    println("  movss %%xmm%d, %d(%%ebp)", r, offset);
    return;
  case 8:
    println("  movsd %%xmm%d, %d(%%ebp)", r, offset);
    return;
  }
  unreachable();
}

static void store_gp(int r, int offset, int sz) {
  switch (sz) {
  case 1:
    println("  mov %s, %d(%%ebp)", argreg8[r], offset);
    return;
  case 2:
    println("  mov %s, %d(%%ebp)", argreg16[r], offset);
    return;
  case 4:
    println("  mov %s, %d(%%ebp)", argreg32[r], offset);
    return;
  default:
    unreachable();
  }
}

static void emit_text(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition)
      continue;

    if (!fn->is_live)
      continue;

    println("  .globl %s", fn->name);
    println("  .text");
    println("%s:", fn->name);
    current_fn = fn;

    println("  push %%ebp");
    println("  mov %%esp, %%ebp");
    println("  sub $%d, %%esp", fn->stack_size);

    gen_stmt(fn->body);
    assert(depth == 0);

    if (strcmp(fn->name, "main") == 0)
      println("  mov $0, %%eax");

    println(".L.return.%s:", fn->name);
    println("  mov %%ebp, %%esp");
    println("  pop %%ebp");
    println("  ret");
  }
}

static void gen_start(void) {
    println("  .globl _start");
    println("  .text");
    println("_start:");

    // _start expects the stack prepared by run_bin()
    // argc at [esp], argv at [esp+4]
    println("  movl (%%esp), %%eax");         // argc
    println("  leal 4(%%esp), %%ebx");        // argv
    println("  push %%eax");                  // push argc
    println("  push %%ebx");                  // push argv
    println("  call main");                   // call main(int argc, char **argv)
    println("  add $8, %%esp");               // clean up stack

    /*
    println("hang:");
    println("  cli");
    println("  hlt");
    println("  jmp hang");
    */
}

void codegen(Obj *prog, FILE *out, bool library) {
  output_file = out;

  if (!library) {
 	gen_start();
  }

  File **files = get_input_files();
  for (int i = 0; files[i]; i++)
    println("  .file %d \"%s\"", files[i]->file_no, files[i]->name);

  assign_lvar_offsets(prog);
  emit_data(prog);
  emit_text(prog);
}
