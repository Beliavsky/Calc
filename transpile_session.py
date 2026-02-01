#!/usr/bin/env python3
import argparse
import re
from pathlib import Path

ARRAY_FUNCS = {
    "arange",
    "grid",
    "runif",
    "rnorm",
    "random_normal",
    "zeros",
    "ones",
    "cumsum",
    "cumprod",
    "cummean",
    "cummin",
    "cummax",
    "diff",
    "head",
    "tail",
    "sort",
    "sorted",
    "indexx",
    "rank",
    "unique",
    "stdz",
    "standardize",
    "reverse",
    "read",
}
SCALAR_FUNCS = {
    "sum",
    "mean",
    "sd",
    "median",
    "minval",
    "maxval",
    "minloc",
    "maxloc",
    "count",
    "size",
    "norm1",
    "norm2",
    "skew",
    "kurt",
    "kurtosis",
    "geomean",
    "harmean",
    "cor",
    "cov",
    "dot",
}
CALL_ONLY = {"plot", "plot_to_label", "print_stats"}
REWRITE_FUNCS = {
    "rnorm": "random_normal",
    "sort": "sorted",
    "stdz": "standardize",
    "read": "read_vec",
    "dot": "dot_product",
}
INT_VARS = set()
CONST_PARAMS = {}

MODULE_EXPORTS = {
    "util_mod": {
        "arange",
        "grid",
        "zeros",
        "ones",
        "rep",
        "read_vec",
        "reverse",
        "head",
        "tail",
    },
    "stats_mod": {
        "mean",
        "sd",
        "cor",
        "cov",
        "cumsum",
        "cumprod",
        "diff",
        "standardize",
        "print_stats",
        "skew",
        "kurtosis",
        "cummin",
        "cummax",
        "cummean",
        "geomean",
        "harmean",
        "acf",
        "arsim",
    },
    "random_mod": {
        "random_normal",
        "runif",
    },
    "qsort_mod": {
        "sorted",
        "indexx",
        "rank",
        "median",
        "unique",
    },
    "plot_mod": {
        "plot",
        "plot_to_label",
    },
}


def split_top_level(text, sep=";"):
    parts = []
    buf = []
    depth_par = 0
    depth_br = 0
    in_str = False
    i = 0
    while i < len(text):
        ch = text[i]
        if ch == '"':
            in_str = not in_str
            buf.append(ch)
            i += 1
            continue
        if not in_str:
            if ch == "(":
                depth_par += 1
            elif ch == ")":
                depth_par = max(0, depth_par - 1)
            elif ch == "[":
                depth_br += 1
            elif ch == "]":
                depth_br = max(0, depth_br - 1)
            elif ch == sep and depth_par == 0 and depth_br == 0:
                parts.append("".join(buf).strip())
                buf = []
                i += 1
                continue
        buf.append(ch)
        i += 1
    last = "".join(buf).strip()
    if last or text.rstrip().endswith(sep):
        parts.append(last)
    return parts


def split_comment(line):
    in_str = False
    for i, ch in enumerate(line):
        if ch == '"':
            in_str = not in_str
            continue
        if not in_str and ch == "!":
            return line[:i].rstrip(), line[i + 1 :].rstrip()
    return line.rstrip(), ""


def find_top_level_assign(s):
    depth_par = 0
    depth_br = 0
    in_str = False
    for i, ch in enumerate(s):
        if ch == '"':
            in_str = not in_str
            continue
        if in_str:
            continue
        if ch == "(":
            depth_par += 1
        elif ch == ")":
            depth_par = max(0, depth_par - 1)
        elif ch == "[":
            depth_br += 1
        elif ch == "]":
            depth_br = max(0, depth_br - 1)
        elif ch == "=" and depth_par == 0 and depth_br == 0:
            if i > 0 and s[i - 1] in "><!/=":
                continue
            if i + 1 < len(s) and s[i + 1] == "=":
                continue
            return i
    return -1


def normalize_array_content(content):
    items = []
    buf = []
    depth_par = 0
    depth_br = 0
    in_str = False
    i = 0
    while i < len(content):
        ch = content[i]
        if ch == '"':
            in_str = not in_str
            buf.append(ch)
            i += 1
            continue
        if not in_str:
            if ch == "(":
                depth_par += 1
            elif ch == ")":
                depth_par = max(0, depth_par - 1)
            elif ch == "[":
                depth_br += 1
            elif ch == "]":
                depth_br = max(0, depth_br - 1)
            if depth_par == 0 and depth_br == 0 and (ch == "," or ch.isspace()):
                item = "".join(buf).strip()
                if item:
                    items.append(item)
                buf = []
                i += 1
                while i < len(content) and (content[i].isspace() or content[i] == ","):
                    i += 1
                continue
        buf.append(ch)
        i += 1
    last = "".join(buf).strip()
    if last:
        items.append(last)
    return ", ".join(items)


def convert_brackets(expr):
    out = []
    i = 0
    in_str = False
    while i < len(expr):
        ch = expr[i]
        if ch == '"':
            in_str = not in_str
            out.append(ch)
            i += 1
            continue
        if not in_str and ch == "[":
            depth = 1
            j = i + 1
            while j < len(expr) and depth > 0:
                if expr[j] == '"':
                    in_str = not in_str
                if not in_str:
                    if expr[j] == "[":
                        depth += 1
                    elif expr[j] == "]":
                        depth -= 1
                        if depth == 0:
                            break
                j += 1
            content = expr[i + 1 : j] if j < len(expr) else expr[i + 1 :]
            norm = normalize_array_content(content)
            out.append("[" + norm + "]")
            i = j + 1
            continue
        out.append(ch)
        i += 1
    return "".join(out)


def replace_ops(expr):
    out = []
    in_str = False
    i = 0
    while i < len(expr):
        ch = expr[i]
        if ch == '"':
            in_str = not in_str
            out.append(ch)
            i += 1
            continue
        if not in_str and ch == "^":
            out.append("**")
            i += 1
            continue
        out.append(ch)
        i += 1
    return "".join(out)


def add_dp_suffix(expr):
    out = []
    in_str = False
    i = 0
    while i < len(expr):
        ch = expr[i]
        if ch == '"':
            in_str = not in_str
            out.append(ch)
            i += 1
            continue
        if in_str:
            out.append(ch)
            i += 1
            continue
        if ch.isdigit() or ch == ".":
            j = i
            has_dot = False
            has_exp = False
            if ch == ".":
                has_dot = True
            j += 1
            while j < len(expr):
                cj = expr[j]
                if cj.isdigit():
                    j += 1
                    continue
                if cj == "." and not has_dot and not has_exp:
                    has_dot = True
                    j += 1
                    continue
                if (cj == "e" or cj == "E") and not has_exp:
                    has_exp = True
                    j += 1
                    if j < len(expr) and expr[j] in "+-":
                        j += 1
                    continue
                break
            token = expr[i:j]
            if (has_dot or has_exp) and token != ".":
                if not token.endswith(("_dp", "_DP")):
                    out.append(token + "_dp")
                else:
                    out.append(token)
            else:
                out.append(token)
            i = j
            continue
        out.append(ch)
        i += 1
    return "".join(out)


def rewrite_functions(expr):
    def repl(match):
        name = match.group(1)
        repl_name = REWRITE_FUNCS.get(name, name)
        return repl_name + "("

    return re.sub(r"\b([A-Za-z_][A-Za-z0-9_]*)\s*\(", repl, expr)


def rewrite_int_args(expr):
    def wrap_int_arg(match):
        name = match.group(1)
        arg = match.group(2).strip()
        if "," in arg:
            return f"{name}({arg})"
        if re.fullmatch(r"[0-9]+", arg):
            return f"{name}({arg})"
        if arg in INT_VARS:
            return f"{name}({arg})"
        if arg.startswith(("nint(", "int(", "size(")):
            return f"{name}({arg})"
        return f"{name}(nint({arg}))"

    expr = re.sub(r"\b(random_normal|runif|arange)\s*\(\s*([^)]+?)\s*\)", wrap_int_arg, expr)
    return expr


def transpile_expr(expr):
    expr = rewrite_functions(expr)
    expr = rewrite_int_args(expr)
    expr = convert_brackets(expr)
    expr = replace_ops(expr)
    expr = add_dp_suffix(expr)
    return expr


def is_string_only(stmt):
    s = stmt.strip()
    return len(s) >= 2 and s[0] == '"' and s[-1] == '"'


def extract_identifiers(expr):
    return re.findall(r"\b[A-Za-z_][A-Za-z0-9_]*\b", expr)


def infer_rank(rhs, known_arrays):
    if "[" in rhs and "]" in rhs:
        return "array"
    if re.search(r"\b\w+\s*\([^)]*:\s*[^)]*\)", rhs):
        return "array"
    for fn in ARRAY_FUNCS:
        if re.search(rf"\b{fn}\s*\(", rhs):
            if fn in {"runif", "rnorm", "random_normal"}:
                if re.search(rf"\b{fn}\s*\(\s*\)", rhs):
                    return "scalar"
            return "array"
    for name in extract_identifiers(rhs):
        if name in known_arrays:
            return "array"
    return "scalar"


def is_int_literal(expr):
    s = expr.strip()
    return re.fullmatch(r"[0-9]+", s) is not None


def is_real_literal(expr):
    s = expr.strip()
    return re.fullmatch(r"([0-9]*\.[0-9]+|[0-9]+\.[0-9]*)([eE][+-]?[0-9]+)?", s) is not None or (
        re.fullmatch(r"[0-9]+([eE][+-]?[0-9]+)", s) is not None
    )


def is_int_expr(expr):
    s = re.sub(r"\s+", "", expr)
    if not s:
        return False
    return re.fullmatch(r"[0-9()+\-*^]+", s) is not None


def infer_from_lines(lines):
    ranks = {}
    loop_vars = set()
    int_vars = set()
    const_params = {}
    for raw in lines:
        raw_line = raw.rstrip("\n")
        if not raw_line.strip():
            continue
        if raw_line.lstrip().startswith("!"):
            continue
        line, _comment = split_comment(raw_line)
        line = line.strip()
        if not line or line.startswith("!"):
            continue
        if line.lower().startswith("do "):
            m = re.match(r"do\s+([A-Za-z_][A-Za-z0-9_]*)\s*=", line, re.IGNORECASE)
            if m:
                loop_vars.add(m.group(1))
        if line.startswith("*"):
            m = re.match(r"\*\s*(\d+)\s+(.+)$", line)
            if m:
                line = m.group(2).strip()
        for stmt in split_top_level(line, ";"):
            if not stmt:
                continue
            if stmt.lower().startswith("const "):
                const_stmt = stmt.strip()[6:].strip()
                eqpos = find_top_level_assign(const_stmt)
                if eqpos != -1:
                    lhs = const_stmt[:eqpos].strip()
                    rhs = const_stmt[eqpos + 1 :].strip()
                if is_int_expr(rhs):
                    const_params[lhs] = ("integer", rhs)
                    int_vars.add(lhs)
                else:
                    const_params[lhs] = ("real", rhs)
                ranks[lhs] = "scalar"
                continue
            if is_string_only(stmt):
                continue
            if stmt.lower().startswith("read "):
                continue
            if stmt.strip().startswith("?"):
                continue
            if stmt.strip().lower() in {"clear", "exit"}:
                continue
            if stmt.strip().lower().startswith("del "):
                continue
            if stmt.lower().startswith("do ") or stmt.lower().startswith("end do") or stmt.lower().startswith("if "):
                continue
            eqpos = find_top_level_assign(stmt)
            if eqpos != -1:
                lhs = stmt[:eqpos].strip()
                rhs = stmt[eqpos + 1 :].strip()
                if "(" in lhs and ")" in lhs:
                    continue
                name = lhs
                if is_int_expr(rhs):
                    int_vars.add(name)
                rank = infer_rank(rhs, {k for k, v in ranks.items() if v == "array"})
                prev = ranks.get(name)
                if prev == "array" or rank == "array":
                    ranks[name] = "array"
                else:
                    ranks[name] = "scalar"
            else:
                for name in extract_identifiers(stmt):
                    if name in ranks:
                        continue
    return ranks, loop_vars, int_vars, const_params


def transpile_lines(lines):
    out = []
    rep_idx = 0
    rep_vars = []
    for raw in lines:
        line = raw.rstrip("\n")
        if not line.strip():
            out.append("")
            continue
        if line.lstrip().startswith("!"):
            out.append(line.strip())
            continue
        code, comment = split_comment(line)
        stripped = code.strip()
        if not stripped:
            if comment:
                out.append("! " + comment)
            else:
                out.append("")
            continue
        if stripped.startswith("*"):
            m = re.match(r"\*\s*(\d+)\s+(.+)$", stripped)
            if not m:
                out.append("! " + stripped + (" ! " + comment if comment else ""))
                continue
            rep_idx += 1
            rep_var = f"rep{rep_idx}"
            rep_vars.append(rep_var)
            count = m.group(1)
            body = m.group(2).strip()
            out.append(f"do {rep_var} = 1, {count}")
            for stmt in split_top_level(body, ";"):
                if stmt:
                    out.extend(transpile_statement(stmt))
            out.append("end do")
            if comment:
                out.append("! " + comment)
            continue
        stmt_lines = []
        for stmt in split_top_level(stripped, ";"):
            if stmt:
                stmt_lines.extend(transpile_statement(stmt))
        if comment:
            if stmt_lines:
                stmt_lines[-1] = stmt_lines[-1] + " ! " + comment
            else:
                stmt_lines.append("! " + comment)
        out.extend(stmt_lines)
    return out, rep_vars


def transpile_statement(stmt):
    s = stmt.strip()
    low = s.lower()
    if not s:
        return []
    if s.startswith("!"):
        return [s]
    if is_string_only(s):
        return [f"print *, {s}"]
    if low.startswith("const "):
        const_stmt = s[6:].strip()
        eqpos = find_top_level_assign(const_stmt)
        if eqpos == -1:
            return [f"! {s}"]
        return []
    if low == "cor":
        return [f"! {s}"]
    if '"' in s:
        items = []
        buf = ""
        in_str = False
        for ch in s:
            if ch == '"':
                if in_str:
                    items.append('"' + buf + '"')
                    buf = ""
                    in_str = False
                else:
                    seg = buf.strip()
                    if seg:
                        seg = re.sub(r"\s*,\s*", " ", seg)
                        for tok in seg.split():
                            items.append(transpile_expr(tok))
                    buf = ""
                    in_str = True
            else:
                buf += ch
        seg = buf.strip()
        if seg:
            seg = re.sub(r"\s*,\s*", " ", seg)
            for tok in seg.split():
                items.append(transpile_expr(tok))
        if items:
            return ["print *, " + ", ".join(items)]
    tokens = s.split()
    if len(tokens) > 1 and all(re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", t) for t in tokens):
        return ["print *, " + ", ".join(tokens)]
    if low.startswith("?"):
        return [f"! {s}"]
    if low in {"clear", "exit"}:
        return [f"! {s}"]
    if low.startswith("del "):
        return [f"! {s}"]
    if low.startswith("read "):
        return [f"! {s}"]
    if low.startswith("do ") or low.startswith("end do") or low.startswith("if "):
        return [transpile_expr(s)]
    if low.startswith("else") or low.startswith("cycle") or low.startswith("exit"):
        return [s]
    if any(low.startswith(fn + "(") for fn in CALL_ONLY):
        return ["call " + transpile_expr(s)]
    eqpos = find_top_level_assign(s)
    if eqpos != -1:
        lhs = s[:eqpos].strip()
        rhs = s[eqpos + 1 :].strip()
        return [f"{lhs} = {transpile_expr(rhs)}"]
    return [f"print *, {transpile_expr(s)}"]


def render_fortran(lines, ranks, loop_vars, rep_vars, int_vars):
    arrays = sorted([k for k, v in ranks.items() if v == "array"])
    scalars = sorted([k for k, v in ranks.items() if v == "scalar" and k not in loop_vars])
    loop_vars_all = sorted(set(loop_vars) | set(rep_vars))
    int_vars = sorted([v for v in int_vars if v not in loop_vars_all])
    const_names = set(CONST_PARAMS.keys())
    arrays = [v for v in arrays if v not in const_names]
    scalars = [v for v in scalars if v not in const_names]
    int_vars = [v for v in int_vars if v not in const_names]
    scalars = [v for v in scalars if v not in int_vars]

    used_names = set()
    for line in lines:
        for name in re.findall(r"\b([A-Za-z_][A-Za-z0-9_]*)\s*\(", line):
            used_names.add(name)
        m = re.match(r"\s*call\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(", line)
        if m:
            used_names.add(m.group(1))

    module_uses = []
    for mod, exports in MODULE_EXPORTS.items():
        needed = sorted(exports & used_names)
        if needed:
            module_uses.append((mod, needed))

    out = []
    out.append("program session")
    needs_dp = bool(scalars or arrays)
    for _name, (typ, _rhs) in CONST_PARAMS.items():
        if typ == "real":
            needs_dp = True
            break
    if needs_dp:
        out.append("  use kind_mod, only: dp")
    for mod, needed in module_uses:
        out.append("  use " + mod + ", only: " + ", ".join(needed))
    out.append("  implicit none")
    if loop_vars_all:
        out.append("  integer :: " + ", ".join(loop_vars_all))
    const_ints = sorted([k for k, v in CONST_PARAMS.items() if v[0] == "integer"])
    const_reals = sorted([k for k, v in CONST_PARAMS.items() if v[0] == "real"])
    if const_ints:
        decls = [f"{k} = {replace_ops(CONST_PARAMS[k][1])}" for k in const_ints]
        out.append("  integer, parameter :: " + ", ".join(decls))
    if const_reals:
        decls = [f"{k} = {transpile_expr(CONST_PARAMS[k][1])}" for k in const_reals]
        out.append("  real(kind=dp), parameter :: " + ", ".join(decls))
    if int_vars:
        out.append("  integer :: " + ", ".join(int_vars))
    if scalars:
        out.append("  real(kind=dp) :: " + ", ".join(scalars))
    if arrays:
        out.append("  real(kind=dp), allocatable :: " + ", ".join(f"{a}(:)" for a in arrays))
    out.append("")
    base_indent = 3
    indent = 0
    for line in lines:
        if line == "":
            out.append("")
            continue
        low = line.strip().lower()
        if low.startswith("end do") or low.startswith("enddo") or low.startswith("end if") or low.startswith("endif"):
            indent = max(0, indent - 1)
        out.append((" " * (base_indent + 3 * indent)) + line)
        if low.startswith("do ") or low.startswith("if "):
            indent += 1
    out.append("end program session")
    return "\n".join(out)


def main():
    ap = argparse.ArgumentParser(description="Transpile interpreter .fi commands to Fortran")
    ap.add_argument("input", help="Input .fi file")
    ap.add_argument("-o", "--output", help="Output .f90 file (default: stdout)")
    args = ap.parse_args()

    path = Path(args.input)
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()

    ranks, loop_vars, int_vars, const_params = infer_from_lines(lines)
    global INT_VARS
    INT_VARS = set(int_vars)
    global CONST_PARAMS
    CONST_PARAMS = const_params
    transpiled, rep_vars = transpile_lines(lines)
    if not CONST_PARAMS and const_params:
        CONST_PARAMS = const_params
    rendered = render_fortran(transpiled, ranks, loop_vars, rep_vars, int_vars)

    if args.output:
        Path(args.output).write_text(rendered + "\n", encoding="utf-8")
    else:
        print(rendered)


if __name__ == "__main__":
    main()
