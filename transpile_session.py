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
    "rexp",
    "rgamma",
    "rlnorm",
    "rt",
    "rnct",
    "rmixnorm",
    "rchisq",
    "rf",
    "rbeta",
    "rlogis",
    "rsech",
    "rlaplace",
    "rcauchy",
    "rged",
    "rhyperb",
    "kde",
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
    "acf",
    "pacf",
    "fiacf",
    "fracdiff",
    "aracf",
    "maacf",
    "arpacf",
    "mapacf",
    "armaacf",
    "arfimaacf",
    "armapacf",
    "arsim",
    "masim",
    "armasim",
    "arfimasim",
    "cpsim",
    "cpfit",
    "cpfitaic",
    "cpfit_aic",
    "resample",
    "quantile",
    "trimmean",
    "winsor_mean",
    "mad",
    "iqr_scale",
    "jb_test",
    "ttest1",
    "ttest2",
    "ks2_test",
    "kernelreg",
    "lowess",
    "lowesscv",
    "knnreg",
    "knnregcv",
    "splinereg",
    "naturalspline",
    "mssk",
    "mssk_exp",
    "mssk_gamma",
    "mssk_lnorm",
    "mssk_t",
    "mssk_nct",
    "mssk_mixnorm",
    "mssk_chisq",
    "mssk_f",
    "mssk_beta",
    "mssk_logis",
    "mssk_sech",
    "mssk_laplace",
    "fit_norm",
    "fit_exp",
    "fit_gamma",
    "fit_lnorm",
    "fit_t",
    "fit_nct",
    "fit_mixnorm",
    "fit_mixnorm_aic",
    "fix_mixnorm_aic",
    "fit_chisq",
    "fit_f",
    "fit_beta",
    "fit_logis",
    "fit_sech",
    "fit_laplace",
    "fit_cauchy",
    "fit_ged",
    "fit_hyperb",
    "dunif",
    "dexp",
    "dgamma",
    "dlnorm",
    "dnorm",
    "dmixnorm",
    "dt",
    "dnct",
    "dchisq",
    "df",
    "dbeta",
    "dlogis",
    "dsech",
    "dlaplace",
    "dcauchy",
    "dged",
    "dhyperb",
    "punif",
    "pexp",
    "pgamma",
    "plnorm",
    "pnorm",
    "pmixnorm",
    "pt",
    "pnct",
    "pchisq",
    "pf",
    "pbeta",
    "plogis",
    "psech",
    "plaplace",
    "pcauchy",
    "pged",
    "phyperb",
    "qunif",
    "qexp",
    "qgamma",
    "qlnorm",
    "qnorm",
    "qmixnorm",
    "qt",
    "qnct",
    "qchisq",
    "qf",
    "qbeta",
    "qlogis",
    "qsech",
    "qlaplace",
    "qcauchy",
    "qged",
    "qhyperb",
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
CALL_ONLY = {
    "plot",
    "plot_to_label",
    "print_stats",
    "regress",
    "regress_multi",
    "arfit",
    "mafit",
    "armafit",
    "armafitgrid",
    "armafitaic",
    "arfimafit",
    "acfpacf",
    "acfpacfar",
    "poly1reg",
    "splinereg",
    "distaicscan",
}
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
        "pacf",
        "acfpacf",
        "acfpacfar",
        "fiacf",
        "fracdiff",
        "aracf",
        "maacf",
        "arpacf",
        "mapacf",
        "armaacf",
        "arfimaacf",
        "armapacf",
        "arsim",
        "masim",
        "armasim",
        "arfimasim",
        "cpsim",
        "cpfit",
        "cpfitaic",
        "cpfit_aic",
        "resample",
        "trimmean",
        "winsor_mean",
        "mad",
        "iqr_scale",
        "jb_test",
        "ttest1",
        "ttest2",
        "ks2_test",
        "kernelreg",
        "lowess",
        "lowesscv",
        "knnreg",
        "knnregcv",
        "splinereg",
        "naturalspline",
        "regress",
        "regress_multi",
        "poly1reg",
        "distaicscan",
        "arfit",
        "mafit",
        "armafit",
        "armafitgrid",
        "armafitaic",
        "arfimafit",
        "mssk",
        "mssk_exp",
        "mssk_gamma",
        "mssk_lnorm",
        "mssk_t",
        "mssk_nct",
        "mssk_mixnorm",
        "mssk_chisq",
        "mssk_f",
        "mssk_beta",
        "mssk_logis",
        "mssk_sech",
        "mssk_laplace",
        "fit_norm",
        "fit_exp",
        "fit_gamma",
        "fit_lnorm",
        "fit_t",
        "fit_nct",
        "fit_mixnorm",
        "fit_mixnorm_aic",
        "fix_mixnorm_aic",
        "fit_chisq",
        "fit_f",
        "fit_beta",
        "fit_logis",
        "fit_sech",
        "fit_laplace",
        "fit_cauchy",
        "fit_ged",
        "fit_hyperb",
        "dunif",
        "dexp",
        "dgamma",
        "dlnorm",
        "dnorm",
        "dmixnorm",
        "dt",
        "dnct",
        "dchisq",
        "df",
        "dbeta",
        "dlogis",
        "dsech",
        "dlaplace",
        "dcauchy",
        "dged",
        "dhyperb",
        "punif",
        "pexp",
        "pgamma",
        "plnorm",
        "pnorm",
        "pmixnorm",
        "pt",
        "pnct",
        "pchisq",
        "pf",
        "pbeta",
        "plogis",
        "psech",
        "plaplace",
        "pcauchy",
        "pged",
        "phyperb",
        "qunif",
        "qexp",
        "qgamma",
        "qlnorm",
        "qnorm",
        "qmixnorm",
        "qt",
        "qnct",
        "qchisq",
        "qf",
        "qbeta",
        "qlogis",
        "qsech",
        "qlaplace",
        "qcauchy",
        "qged",
        "qhyperb",
        "rhyperb",
        "kde",
    },
    "random_mod": {
        "random_normal",
        "runif",
        "rexp",
        "rgamma",
        "rlnorm",
        "rt",
        "rnct",
        "rmixnorm",
        "rchisq",
        "rf",
        "rbeta",
        "rlogis",
        "rsech",
        "rlaplace",
        "rcauchy",
        "rged",
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


def strip_prompt(line):
    return re.sub(r"^\s*>\s*", "", line)


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


def has_top_level_relational(expr):
    depth_par = 0
    depth_br = 0
    in_str = False
    i = 0
    while i < len(expr):
        ch = expr[i]
        if ch == '"':
            in_str = not in_str
            i += 1
            continue
        if in_str:
            i += 1
            continue
        if ch == "(":
            depth_par += 1
            i += 1
            continue
        if ch == ")":
            depth_par = max(0, depth_par - 1)
            i += 1
            continue
        if ch == "[":
            depth_br += 1
            i += 1
            continue
        if ch == "]":
            depth_br = max(0, depth_br - 1)
            i += 1
            continue
        if depth_par == 0 and depth_br == 0:
            if expr.startswith(("<=", ">=", "==", "/="), i):
                return True
            if ch in "<>":
                return True
        i += 1
    return False


def strip_outer_parens(expr):
    s = expr.strip()
    while s.startswith("(") and s.endswith(")"):
        depth = 0
        in_str = False
        ok = True
        for i, ch in enumerate(s):
            if ch == '"':
                in_str = not in_str
                continue
            if in_str:
                continue
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0 and i != len(s) - 1:
                    ok = False
                    break
        if ok and depth == 0:
            s = s[1:-1].strip()
        else:
            break
    return s


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


def rewrite_arfimasim_calls(expr):
    out = []
    i = 0
    while i < len(expr):
        m = re.search(r"\barfimasim\s*\(", expr[i:], re.IGNORECASE)
        if not m:
            out.append(expr[i:])
            break
        start = i + m.start()
        out.append(expr[i:start])
        lpar = start + m.group(0).rfind("(")
        depth = 1
        j = lpar + 1
        in_str = False
        while j < len(expr) and depth > 0:
            ch = expr[j]
            if ch == '"':
                in_str = not in_str
            elif not in_str:
                if ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                    if depth == 0:
                        break
            j += 1
        if j >= len(expr):
            out.append(expr[start:])
            break
        args_raw = expr[lpar + 1 : j]
        args = split_top_level(args_raw, ",")
        # Rewrite legacy positional form: arfimasim(n, phi, theta, d, ...)
        if (
            len(args) >= 4
            and all("=" not in a for a in args[:4])
            and "[" in args[1]
            and "[" in args[2]
        ):
            new_args = [args[0], args[3], f"phi={args[1]}", f"theta={args[2]}"] + args[4:]
            out.append("arfimasim(" + ", ".join(new_args) + ")")
        else:
            out.append(expr[start : j + 1])
        i = j + 1
    return "".join(out)


def rewrite_acf_pacf_plot_args(expr):
    out = []
    i = 0
    while i < len(expr):
        m = re.search(r"\b(acf|pacf)\s*\(", expr[i:], re.IGNORECASE)
        if not m:
            out.append(expr[i:])
            break
        start = i + m.start()
        fname = m.group(1)
        out.append(expr[i:start])
        lpar = start + m.group(0).rfind("(")
        depth = 1
        j = lpar + 1
        in_str = False
        while j < len(expr) and depth > 0:
            ch = expr[j]
            if ch == '"':
                in_str = not in_str
            elif not in_str:
                if ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                    if depth == 0:
                        break
            j += 1
        if j >= len(expr):
            out.append(expr[start:])
            break
        args = split_top_level(expr[lpar + 1 : j], ",")
        kept = []
        for a in args:
            a_str = a.strip()
            if a_str.lower().startswith("plot="):
                continue
            kept.append(rewrite_acf_pacf_plot_args(a_str))
        out.append(f"{fname}(" + ", ".join(kept) + ")")
        i = j + 1
    return "".join(out)


def rewrite_reduction_calls(expr):
    reducers = {"sum", "product", "minval", "maxval"}
    out = []
    i = 0
    while i < len(expr):
        m = re.search(r"\b(sum|product|minval|maxval)\s*\(", expr[i:], re.IGNORECASE)
        if not m:
            out.append(expr[i:])
            break
        start = i + m.start()
        fname = m.group(1)
        out.append(expr[i:start])
        lpar = start + m.group(0).rfind("(")
        depth = 1
        j = lpar + 1
        in_str = False
        while j < len(expr) and depth > 0:
            ch = expr[j]
            if ch == '"':
                in_str = not in_str
            elif not in_str:
                if ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                    if depth == 0:
                        break
            j += 1
        if j >= len(expr):
            out.append(expr[start:])
            break
        args = split_top_level(expr[lpar + 1 : j], ",")
        new_args = []
        for idx, a in enumerate(args):
            a_str = a.strip()
            eq = find_top_level_assign(a_str)
            if eq != -1:
                key = a_str[:eq].strip().lower()
                rhs = a_str[eq + 1 :].strip()
                rhs_rw = rewrite_reduction_calls(rhs)
                if key == "mask":
                    if has_top_level_relational(rhs_rw):
                        new_args.append(f"{a_str[:eq].strip()}={rhs_rw}")
                    else:
                        new_args.append(f"{a_str[:eq].strip()}=({rhs_rw} /= 0)")
                elif key == "dim":
                    if re.fullmatch(r"[0-9]+", rhs_rw) and rhs_rw != "1":
                        new_args.append(f"{a_str[:eq].strip()}=1")
                    else:
                        new_args.append(f"{a_str[:eq].strip()}={rhs_rw}")
                else:
                    new_args.append(f"{a_str[:eq].strip()}={rhs_rw}")
            else:
                a_rw = rewrite_reduction_calls(a_str)
                if idx == 1 and fname.lower() in reducers:
                    if re.fullmatch(r"[0-9]+", a_rw) and a_rw != "1":
                        a_rw = "1"
                new_args.append(a_rw)
        out.append(f"{fname}(" + ", ".join(new_args) + ")")
        i = j + 1
    return "".join(out)


def rewrite_default_optional_calls(expr):
    defaults = {
        "rexp": {
            1: lambda a: [a[0], "1.0"],
        },
        "rgamma": {
            2: lambda a: [a[0], a[1], "1.0"],
        },
        "rlnorm": {
            1: lambda a: [a[0], "0.0", "1.0"],
            2: lambda a: [a[0], a[1], "1.0"],
        },
        "rlogis": {
            1: lambda a: [a[0], "0.0", "1.0"],
            2: lambda a: [a[0], a[1], "1.0"],
        },
        "mssk_exp": {
            0: lambda a: ["1.0"],
        },
        "mssk_gamma": {
            1: lambda a: [a[0], "1.0"],
        },
        "mssk_lnorm": {
            0: lambda a: ["0.0", "1.0"],
            1: lambda a: [a[0], "1.0"],
        },
        "mssk_logis": {
            0: lambda a: ["0.0", "1.0"],
            1: lambda a: [a[0], "1.0"],
        },
    }

    out = []
    i = 0
    while i < len(expr):
        m = re.search(r"\b([A-Za-z_][A-Za-z0-9_]*)\s*\(", expr[i:])
        if not m:
            out.append(expr[i:])
            break
        start = i + m.start()
        fname = m.group(1)
        out.append(expr[i:start])
        lpar = start + m.group(0).rfind("(")
        depth = 1
        j = lpar + 1
        in_str = False
        while j < len(expr) and depth > 0:
            ch = expr[j]
            if ch == '"':
                in_str = not in_str
            elif not in_str:
                if ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                    if depth == 0:
                        break
            j += 1
        if j >= len(expr):
            out.append(expr[start:])
            break

        inner = expr[lpar + 1 : j]
        args = split_top_level(inner, ",")
        args = [rewrite_default_optional_calls(a.strip()) for a in args if a.strip()]
        low = fname.lower()
        if low in defaults and all("=" not in a for a in args):
            rules = defaults[low]
            if len(args) in rules:
                args = rules[len(args)](args)
        out.append(f"{fname}(" + ", ".join(args) + ")")
        i = j + 1
    return "".join(out)


def parse_top_call_with_plot(stmt, fname):
    s = stmt.strip()
    low = s.lower()
    prefix = fname.lower() + "("
    if not low.startswith(prefix):
        return None
    depth = 0
    in_str = False
    end = -1
    for idx, ch in enumerate(s):
        if ch == '"':
            in_str = not in_str
            continue
        if in_str:
            continue
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
            if depth == 0:
                end = idx
                break
    if end == -1 or s[end + 1 :].strip():
        return None
    args = split_top_level(s[len(fname) + 1 : end], ",")
    kept = []
    plot_arg = None
    for a in args:
        a_str = a.strip()
        if a_str.lower().startswith("plot="):
            plot_arg = a_str.split("=", 1)[1].strip()
        else:
            kept.append(a_str)
    if plot_arg is None:
        return None
    return kept, plot_arg


def find_named_call_spans(expr, fname):
    spans = []
    i = 0
    n = len(expr)
    in_str = False
    target = fname.lower()
    while i < n:
        ch = expr[i]
        if ch == '"':
            in_str = not in_str
            i += 1
            continue
        if in_str:
            i += 1
            continue
        if re.match(r"[A-Za-z_]", ch):
            j = i + 1
            while j < n and re.match(r"[A-Za-z0-9_]", expr[j]):
                j += 1
            word = expr[i:j].lower()
            if word == target:
                k = j
                while k < n and expr[k].isspace():
                    k += 1
                if k < n and expr[k] == "(":
                    depth = 1
                    p = k + 1
                    in_sub_str = False
                    while p < n and depth > 0:
                        c = expr[p]
                        if c == '"':
                            in_sub_str = not in_sub_str
                        elif not in_sub_str:
                            if c == "(":
                                depth += 1
                            elif c == ")":
                                depth -= 1
                                if depth == 0:
                                    break
                        p += 1
                    if p < n and depth == 0:
                        spans.append((i, p, expr[k + 1 : p]))
                        i = p + 1
                        continue
            i = j
            continue
        i += 1
    return spans


def rewrite_int_args(expr):
    def wrap_int_arg(match):
        name = match.group(1)
        arg = match.group(2).strip()
        if "," in arg:
            return f"{name}({arg})"
        if re.fullmatch(r"[0-9]+", arg):
            return f"{name}({arg})"
        if is_int_expr(arg):
            return f"{name}({arg})"
        if arg in INT_VARS:
            return f"{name}({arg})"
        if arg.startswith(("nint(", "int(", "size(")):
            return f"{name}({arg})"
        return f"{name}(nint({arg}))"

    expr = re.sub(r"\b(random_normal|runif|arange)\s*\(\s*([^)]+?)\s*\)", wrap_int_arg, expr)
    return expr


def rewrite_kernelreg_order_args(expr):
    out = []
    i = 0
    while i < len(expr):
        m = re.search(r"\bkernelreg\s*\(", expr[i:], re.IGNORECASE)
        if not m:
            out.append(expr[i:])
            break
        start = i + m.start()
        out.append(expr[i:start])
        lpar = start + m.group(0).rfind("(")
        depth = 1
        j = lpar + 1
        in_str = False
        while j < len(expr) and depth > 0:
            ch = expr[j]
            if ch == '"':
                in_str = not in_str
            elif not in_str:
                if ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                    if depth == 0:
                        break
            j += 1
        if j >= len(expr):
            out.append(expr[start:])
            break
        args = split_top_level(expr[lpar + 1 : j], ",")
        args = [a.strip() for a in args]
        if len(args) >= 4 and "=" not in args[3]:
            a4 = args[3]
            low4 = a4.lower()
            if not (low4.startswith("nint(") or low4.startswith("int(")):
                args[3] = f"nint(1.0*({a4}))"
        out.append("kernelreg(" + ", ".join(args) + ")")
        i = j + 1
    return "".join(out)


def rewrite_lowess_args(expr):
    def to_int_arg(a):
        s = a.strip()
        low = s.lower()
        if low.startswith(("nint(", "int(")):
            return s
        return f"nint(1.0*({s}))"

    out = []
    i = 0
    while i < len(expr):
        m = re.search(r"\b(lowesscv|lowess)\s*\(", expr[i:], re.IGNORECASE)
        if not m:
            out.append(expr[i:])
            break
        start = i + m.start()
        fname = m.group(1)
        out.append(expr[i:start])
        lpar = start + m.group(0).rfind("(")
        depth = 1
        j = lpar + 1
        in_str = False
        while j < len(expr) and depth > 0:
            ch = expr[j]
            if ch == '"':
                in_str = not in_str
            elif not in_str:
                if ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                    if depth == 0:
                        break
            j += 1
        if j >= len(expr):
            out.append(expr[start:])
            break
        args = [a.strip() for a in split_top_level(expr[lpar + 1 : j], ",")]
        if len(args) >= 4 and "=" not in args[3]:
            args[3] = to_int_arg(args[3])
        for idx in range(2, len(args)):
            a = args[idx]
            eq = find_top_level_assign(a)
            if eq != -1:
                key = a[:eq].strip().lower()
                rhs = a[eq + 1 :].strip()
                if key == "it":
                    args[idx] = f"{a[:eq].strip()}={to_int_arg(rhs)}"
        out.append(fname + "(" + ", ".join(args) + ")")
        i = j + 1
    return "".join(out)


def rewrite_knnreg_args(expr):
    def to_int_arg(a):
        s = a.strip()
        low = s.lower()
        if low.startswith(("nint(", "int(")):
            return s
        return f"nint(1.0*({s}))"

    out = []
    i = 0
    while i < len(expr):
        m = re.search(r"\b(knnregcv|knnreg)\s*\(", expr[i:], re.IGNORECASE)
        if not m:
            out.append(expr[i:])
            break
        start = i + m.start()
        fname = m.group(1)
        out.append(expr[i:start])
        lpar = start + m.group(0).rfind("(")
        depth = 1
        j = lpar + 1
        in_str = False
        while j < len(expr) and depth > 0:
            ch = expr[j]
            if ch == '"':
                in_str = not in_str
            elif not in_str:
                if ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                    if depth == 0:
                        break
            j += 1
        if j >= len(expr):
            out.append(expr[start:])
            break
        args = [a.strip() for a in split_top_level(expr[lpar + 1 : j], ",")]
        if len(args) >= 3 and "=" not in args[2]:
            args[2] = to_int_arg(args[2])
        if len(args) >= 4 and "=" not in args[3]:
            args[3] = to_int_arg(args[3])
        for idx in range(2, len(args)):
            a = args[idx]
            eq = find_top_level_assign(a)
            if eq != -1:
                key = a[:eq].strip().lower()
                rhs = a[eq + 1 :].strip()
                if key in {"k", "order"}:
                    args[idx] = f"{a[:eq].strip()}={to_int_arg(rhs)}"
        out.append(fname + "(" + ", ".join(args) + ")")
        i = j + 1
    return "".join(out)


def rewrite_splinereg_args(expr):
    def to_int_arg(a):
        s = a.strip()
        low = s.lower()
        if low in {".true.", "true", "t"}:
            return "1"
        if low in {".false.", "false", "f"}:
            return "0"
        if low.startswith(("nint(", "int(")):
            return s
        return f"nint(1.0*({s}))"

    out = []
    i = 0
    while i < len(expr):
        m = re.search(r"\bsplinereg\s*\(", expr[i:], re.IGNORECASE)
        if not m:
            out.append(expr[i:])
            break
        start = i + m.start()
        out.append(expr[i:start])
        lpar = start + m.group(0).rfind("(")
        depth = 1
        j = lpar + 1
        in_str = False
        while j < len(expr) and depth > 0:
            ch = expr[j]
            if ch == '"':
                in_str = not in_str
            elif not in_str:
                if ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                    if depth == 0:
                        break
            j += 1
        if j >= len(expr):
            out.append(expr[start:])
            break
        args = split_top_level(expr[lpar + 1 : j], ",")
        args = [a.strip() for a in args]
        for idx in range(3, len(args)):
            a = args[idx]
            eq = find_top_level_assign(a)
            if eq != -1:
                key = a[:eq].strip().lower()
                rhs = a[eq + 1 :].strip()
                if key in {"degree", "intcp", "plot"}:
                    args[idx] = f"{a[:eq].strip()}={to_int_arg(rhs)}"
            else:
                args[idx] = to_int_arg(a)
        out.append("splinereg(" + ", ".join(args) + ")")
        i = j + 1
    return "".join(out)


def rewrite_naturalspline_args(expr):
    def to_int_arg(a):
        s = a.strip()
        low = s.lower()
        if low in {".true.", "true", "t"}:
            return "1"
        if low in {".false.", "false", "f"}:
            return "0"
        if low.startswith(("nint(", "int(")):
            return s
        return f"nint(1.0*({s}))"

    out = []
    i = 0
    while i < len(expr):
        m = re.search(r"\bnaturalspline\s*\(", expr[i:], re.IGNORECASE)
        if not m:
            out.append(expr[i:])
            break
        start = i + m.start()
        out.append(expr[i:start])
        lpar = start + m.group(0).rfind("(")
        depth = 1
        j = lpar + 1
        in_str = False
        while j < len(expr) and depth > 0:
            ch = expr[j]
            if ch == '"':
                in_str = not in_str
            elif not in_str:
                if ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                    if depth == 0:
                        break
            j += 1
        if j >= len(expr):
            out.append(expr[start:])
            break
        args = split_top_level(expr[lpar + 1 : j], ",")
        args = [a.strip() for a in args]
        for idx in range(2, len(args)):
            a = args[idx]
            eq = find_top_level_assign(a)
            if eq != -1:
                key = a[:eq].strip().lower()
                rhs = a[eq + 1 :].strip()
                if key in {"k", "intcp", "plot"}:
                    args[idx] = f"{a[:eq].strip()}={to_int_arg(rhs)}"
                elif key in {"points"}:
                    args[idx] = f"{a[:eq].strip()}={rhs}"
            else:
                if idx == 2:
                    args[idx] = to_int_arg(a)
                else:
                    args[idx] = to_int_arg(a)
        out.append("naturalspline(" + ", ".join(args) + ")")
        i = j + 1
    return "".join(out)


def rewrite_cpsim_args(expr):
    def force_real_bracket(s):
        t = s.strip()
        if not (t.startswith("[") and t.endswith("]")):
            return t
        inner = t[1:-1].strip()
        if not inner:
            return t
        parts = [p.strip() for p in split_top_level(inner, ",")]
        out = []
        for p in parts:
            if re.fullmatch(r"[+-]?[0-9]+", p):
                out.append(p + ".0")
            else:
                out.append(p)
        return "[" + ", ".join(out) + "]"

    def wrap_vec(a):
        s = a.strip()
        if s.startswith("["):
            s2 = force_real_bracket(s)
            return f"(1.0*({s2}))"
        return f"[1.0*({s})]"
    def to_int_arg(a):
        s = a.strip()
        low = s.lower()
        if low in {".true.", "true", "t"}:
            return "1"
        if low in {".false.", "false", "f"}:
            return "0"
        if low.startswith(("nint(", "int(")):
            return s
        return f"nint(1.0*({s}))"

    out = []
    i = 0
    while i < len(expr):
        m = re.search(r"\bcpsim\s*\(", expr[i:], re.IGNORECASE)
        if not m:
            out.append(expr[i:])
            break
        start = i + m.start()
        out.append(expr[i:start])
        lpar = start + m.group(0).rfind("(")
        depth = 1
        j = lpar + 1
        in_str = False
        while j < len(expr) and depth > 0:
            ch = expr[j]
            if ch == '"':
                in_str = not in_str
            elif not in_str:
                if ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                    if depth == 0:
                        break
            j += 1
        if j >= len(expr):
            out.append(expr[start:])
            break
        args = [a.strip() for a in split_top_level(expr[lpar + 1 : j], ",")]
        if len(args) >= 2 and "=" not in args[1]:
            args[1] = wrap_vec(args[1])
        if len(args) >= 3 and "=" not in args[2]:
            args[2] = wrap_vec(args[2])
        if len(args) >= 4 and "=" not in args[3]:
            args[3] = wrap_vec(args[3])
        if len(args) >= 5 and "=" not in args[4]:
            args[4] = to_int_arg(args[4])
        if len(args) >= 6 and "=" not in args[5]:
            args[5] = to_int_arg(args[5])
        if len(args) >= 7 and "=" not in args[6]:
            args[6] = to_int_arg(args[6])
        for idx in range(2, len(args)):
            a = args[idx]
            eq = find_top_level_assign(a)
            if eq != -1:
                key = a[:eq].strip().lower()
                rhs = a[eq + 1 :].strip()
                if key in {"cp", "mu", "sd"}:
                    args[idx] = f"{a[:eq].strip()}={wrap_vec(rhs)}"
                elif key == "seed":
                    args[idx] = f"{a[:eq].strip()}={to_int_arg(rhs)}"
                elif key == "plot":
                    args[idx] = f"{a[:eq].strip()}={to_int_arg(rhs)}"
                elif key == "verbose":
                    args[idx] = f"{a[:eq].strip()}={to_int_arg(rhs)}"
        out.append("cpsim(" + ", ".join(args) + ")")
        i = j + 1
    return "".join(out)


def rewrite_cpfit_args(expr):
    def to_int_arg(a):
        s = a.strip()
        low = s.lower()
        if low in {".true.", "true", "t"}:
            return "1"
        if low in {".false.", "false", "f"}:
            return "0"
        if low.startswith(("nint(", "int(")):
            return s
        return f"nint(1.0*({s}))"

    out = []
    i = 0
    while i < len(expr):
        m = re.search(r"\bcpfit\s*\(", expr[i:], re.IGNORECASE)
        if not m:
            out.append(expr[i:])
            break
        start = i + m.start()
        out.append(expr[i:start])
        lpar = start + m.group(0).rfind("(")
        depth = 1
        j = lpar + 1
        in_str = False
        while j < len(expr) and depth > 0:
            ch = expr[j]
            if ch == '"':
                in_str = not in_str
            elif not in_str:
                if ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                    if depth == 0:
                        break
            j += 1
        if j >= len(expr):
            out.append(expr[start:])
            break
        args = [a.strip() for a in split_top_level(expr[lpar + 1 : j], ",")]
        for idx in range(1, len(args)):
            a = args[idx]
            eq = find_top_level_assign(a)
            if eq != -1:
                key = a[:eq].strip().lower()
                rhs = a[eq + 1 :].strip()
                if key in {"max_cp", "minseg", "plot", "verbose"}:
                    args[idx] = f"{a[:eq].strip()}={to_int_arg(rhs)}"
                elif key == "mode":
                    r = rhs.strip()
                    if not (r.startswith('"') or r.startswith("'")):
                        args[idx] = f'{a[:eq].strip()}="{r}"'
            else:
                if idx >= 2:
                    args[idx] = to_int_arg(a)
        out.append("cpfit(" + ", ".join(args) + ")")
        i = j + 1
    return "".join(out)


def rewrite_cpfit_aic_args(expr):
    def to_int_arg(a):
        s = a.strip()
        low = s.lower()
        if low in {".true.", "true", "t"}:
            return "1"
        if low in {".false.", "false", "f"}:
            return "0"
        if low.startswith(("nint(", "int(")):
            return s
        return f"nint(1.0*({s}))"

    def maybe_quote_word(s):
        t = s.strip()
        if not t:
            return t
        if t.startswith(("'", '"')):
            return t
        if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", t):
            return f'"{t}"'
        return t

    out = []
    i = 0
    while i < len(expr):
        m = re.search(r"\b(?:cpfitaic|cpfit_aic)\s*\(", expr[i:], re.IGNORECASE)
        if not m:
            out.append(expr[i:])
            break
        start = i + m.start()
        out.append(expr[i:start])
        lpar = start + m.group(0).rfind("(")
        depth = 1
        j = lpar + 1
        in_str = False
        while j < len(expr) and depth > 0:
            ch = expr[j]
            if ch == '"':
                in_str = not in_str
            elif not in_str:
                if ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                    if depth == 0:
                        break
            j += 1
        if j >= len(expr):
            out.append(expr[start:])
            break
        args = [a.strip() for a in split_top_level(expr[lpar + 1 : j], ",")]
        for idx in range(1, len(args)):
            a = args[idx]
            eq = find_top_level_assign(a)
            if eq != -1:
                key = a[:eq].strip().lower()
                rhs = a[eq + 1 :].strip()
                if key in {"max_cp", "minseg", "plot", "plot_ic", "verbose"}:
                    args[idx] = f"{a[:eq].strip()}={to_int_arg(rhs)}"
                elif key in {"mode", "criterion"}:
                    args[idx] = f"{a[:eq].strip()}={maybe_quote_word(rhs)}"
            else:
                if idx == 1:
                    t = maybe_quote_word(a)
                    if t != a:
                        args[idx] = t
                    else:
                        args[idx] = to_int_arg(a)
                elif idx == 4:
                    t = maybe_quote_word(a)
                    if t != a:
                        args[idx] = t
                    else:
                        args[idx] = to_int_arg(a)
                else:
                    args[idx] = to_int_arg(a)
        out.append("cpfitaic(" + ", ".join(args) + ")")
        i = j + 1
    return "".join(out)


def transpile_expr(expr):
    expr = rewrite_arfimasim_calls(expr)
    expr = rewrite_acf_pacf_plot_args(expr)
    expr = rewrite_reduction_calls(expr)
    expr = rewrite_default_optional_calls(expr)
    expr = rewrite_functions(expr)
    expr = rewrite_kernelreg_order_args(expr)
    expr = rewrite_lowess_args(expr)
    expr = rewrite_knnreg_args(expr)
    expr = rewrite_splinereg_args(expr)
    expr = rewrite_naturalspline_args(expr)
    expr = rewrite_cpsim_args(expr)
    expr = rewrite_cpfit_args(expr)
    expr = rewrite_cpfit_aic_args(expr)
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
        raw_line = strip_prompt(raw.rstrip("\n"))
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
        if line.lower().startswith("for "):
            m = re.match(r"for\s+([A-Za-z_][A-Za-z0-9_]*)\s+in\s+.+$", line, re.IGNORECASE)
            if m:
                ranks[m.group(1)] = "scalar"
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
            if (
                stmt.lower().startswith("do ")
                or stmt.lower().startswith("end do")
                or stmt.lower().startswith("if ")
                or stmt.lower().startswith("end if")
                or stmt.lower().startswith("endif")
                or stmt.lower().startswith("for ")
                or stmt.lower().startswith("end for")
                or stmt.lower().startswith("endfor")
            ):
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


def split_for_expr_tail(rem):
    s = rem.strip()
    if not s:
        return "", ""
    dpar = 0
    dbr = 0
    in_str = False
    for i, ch in enumerate(s):
        if ch == '"':
            in_str = not in_str
            continue
        if in_str:
            continue
        if ch == "(":
            dpar += 1
            continue
        if ch == ")":
            dpar = max(0, dpar - 1)
            continue
        if ch == "[":
            dbr += 1
            continue
        if ch == "]":
            dbr = max(0, dbr - 1)
            continue
        if ch == " " and dpar == 0 and dbr == 0:
            left = s[:i].strip()
            right = s[i + 1 :].strip()
            if not left or not right:
                continue
            if left[-1] in "+-*/^<>=:&|" or right[0] in "+-*/^<>=:&|":
                continue
            return left, right
    return s, ""


def transpile_lines(lines):
    out = []
    rep_idx = 0
    rep_vars = []
    for_array_vars = []
    for raw in lines:
        line = strip_prompt(raw.rstrip("\n"))
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
        m_for = re.match(r"for\s+([A-Za-z_][A-Za-z0-9_]*)\s+in\s+(.+)$", stripped, re.IGNORECASE)
        if m_for:
            rep_idx += 1
            loop_var = m_for.group(1)
            src_expr, inline_body = split_for_expr_tail(m_for.group(2))
            if not src_expr:
                out.append("! " + stripped + (" ! " + comment if comment else ""))
                continue
            idx_var = f"for_idx{rep_idx}"
            arr_var = f"for_vals{rep_idx}"
            rep_vars.append(idx_var)
            for_array_vars.append(arr_var)
            out.append(f"!$forloop {loop_var}")
            out.append(f"{arr_var} = {transpile_expr(src_expr)}")
            out.append(f"do {idx_var} = 1, size({arr_var})")
            out.append(f"{loop_var} = {arr_var}({idx_var})")
            if inline_body:
                for stmt in split_top_level(inline_body, ";"):
                    stmt = stmt.strip()
                    if stmt:
                        out.extend(transpile_statement(stmt))
                out.append("end do")
                if comment:
                    out[-1] = out[-1] + " ! " + comment
            elif comment:
                out[-1] = out[-1] + " ! " + comment
            continue
        if re.match(r"end\s*for\s*;?$", stripped, re.IGNORECASE):
            out.append("end do")
            if comment:
                out[-1] = out[-1] + " ! " + comment
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
    return out, rep_vars, for_array_vars


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
    for fn in ("acf", "pacf"):
        parsed = parse_top_call_with_plot(s, fn)
        if parsed is not None:
            args_no_plot, plot_arg = parsed
            call_expr = fn + "(" + ", ".join(transpile_expr(a) for a in args_no_plot) + ")"
            plot_low = plot_arg.lower()
            if plot_low in {".true.", "true", "t"}:
                cond = ".true."
            elif plot_low in {".false.", "false", "f"}:
                cond = ".false."
            else:
                cond = "(" + transpile_expr(plot_arg) + " /= 0.0_dp)"
            return [
                "block",
                "real(kind=dp), allocatable :: plot_tmp(:)",
                f"plot_tmp = {call_expr}",
                f"if ({cond}) then",
                "call plot(plot_tmp)",
                "end if",
                "print *, plot_tmp",
                "end block",
            ]
    if '"' in s and find_top_level_assign(s) == -1:
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
    if low.startswith("?"):
        return [f"! {s}"]
    if low in {"clear", "exit"}:
        return [f"! {s}"]
    if low.startswith("del "):
        return [f"! {s}"]
    if low.startswith("read "):
        return [f"! {s}"]
    m_do = re.match(r"do\s+([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(.+)$", s, re.IGNORECASE)
    if m_do:
        loop_var = m_do.group(1)
        bounds_part, inline_body = split_for_expr_tail(m_do.group(2))
        if inline_body:
            bounds = [b.strip() for b in split_top_level(bounds_part, ",") if b.strip()]
            if len(bounds) in {2, 3}:
                hdr = f"do {loop_var} = {transpile_expr(bounds[0])}, {transpile_expr(bounds[1])}"
                if len(bounds) == 3:
                    hdr += f", {transpile_expr(bounds[2])}"
                out = [hdr]
                for stmt_i in split_top_level(inline_body, ";"):
                    stmt_i = stmt_i.strip()
                    if stmt_i:
                        out.extend(transpile_statement(stmt_i))
                out.append("end do")
                return out
    if (
        low.startswith("do ")
        or low.startswith("end do")
        or low.startswith("if ")
        or low.startswith("end if")
        or low.startswith("endif")
        or low.startswith("for ")
        or low.startswith("end for")
        or low.startswith("endfor")
    ):
        return [transpile_expr(s)]
    if low.startswith("else") or low.startswith("cycle") or low.startswith("exit"):
        return [s]
    tokens = s.split()
    if len(tokens) > 1 and all(re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", t) for t in tokens):
        return ["print *, " + ", ".join(tokens)]
    if any(low.startswith(fn + "(") for fn in CALL_ONLY):
        return ["call " + transpile_expr(s)]
    eqpos = find_top_level_assign(s)
    if eqpos != -1:
        lhs = s[:eqpos].strip()
        rhs = s[eqpos + 1 :].strip()
        read_calls = find_named_call_spans(rhs, "read")
        if len(read_calls) == 1:
            start, end, args_raw = read_calls[0]
            args = [a.strip() for a in split_top_level(args_raw, ",") if a.strip()]
            if args:
                call_args = [transpile_expr(args[0]), lhs]
                for extra in args[1:]:
                    call_args.append(transpile_expr(extra))
                rhs_repl = (rhs[:start] + lhs + rhs[end + 1 :]).strip()
                out_lines = [f"call read_vec({', '.join(call_args)})"]
                if rhs_repl == lhs:
                    return out_lines
                rhs_norm = strip_outer_parens(rhs_repl)
                if has_top_level_relational(rhs_norm):
                    out_lines.append(f"{lhs} = merge(1.0_dp, 0.0_dp, {transpile_expr(rhs_repl)})")
                else:
                    out_lines.append(f"{lhs} = {transpile_expr(rhs_repl)}")
                return out_lines
        rhs_norm = strip_outer_parens(rhs)
        if has_top_level_relational(rhs_norm):
            return [f"{lhs} = merge(1.0_dp, 0.0_dp, {transpile_expr(rhs)})"]
        return [f"{lhs} = {transpile_expr(rhs)}"]
    return [f"print *, {transpile_expr(s)}"]


def render_fortran(lines, ranks, loop_vars, rep_vars, int_vars, for_array_vars):
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
    if any("_dp" in line for line in lines):
        needs_dp = True
    if any("kind=dp" in line.lower() for line in lines):
        needs_dp = True
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
    for_array_vars = sorted(set(for_array_vars))
    arrays = sorted(set(arrays) | set(for_array_vars))
    if arrays:
        out.append("  real(kind=dp), allocatable :: " + ", ".join(f"{a}(:)" for a in arrays))
    out.append("")
    base_indent = 3
    indent = 0
    named_do_counter = 0
    loop_label_stack = []
    pending_for_alias = ""
    for line in lines:
        if line == "":
            out.append("")
            continue
        low = line.strip().lower()
        if low.startswith("!$forloop "):
            pending_for_alias = line.strip().split(maxsplit=1)[1].strip().lower()
            continue
        if low.startswith("end do") or low.startswith("enddo") or low.startswith("end if") or low.startswith("endif"):
            indent = max(0, indent - 1)
        emit_line = line
        if low.startswith("do "):
            m = re.match(r"\s*do\s+([A-Za-z_][A-Za-z0-9_]*)\s*=", line, re.IGNORECASE)
            loop_var = pending_for_alias if pending_for_alias else (m.group(1).lower() if m else "")
            pending_for_alias = ""
            named_do_counter += 1
            label = f"loop_{named_do_counter}"
            loop_label_stack.append((loop_var.lower(), label))
            emit_line = f"{label}: {line.strip()}"
        elif low.startswith("end do") or low.startswith("enddo"):
            if loop_label_stack:
                _, end_label = loop_label_stack.pop()
                emit_line = f"end do {end_label}"
        elif low.startswith("cycle") or low.startswith("exit"):
            m = re.match(r"\s*(cycle|exit)\s+([A-Za-z_][A-Za-z0-9_]*)\s*$", line, re.IGNORECASE)
            if m:
                stmt = m.group(1).lower()
                target_var = m.group(2).lower()
                target_label = ""
                for loop_var_name, loop_label in reversed(loop_label_stack):
                    if loop_var_name == target_var:
                        target_label = loop_label
                        break
                if target_label:
                    emit_line = f"{stmt} {target_label}"
        elif low.startswith("if"):
            m = re.match(
                r"(\s*if\s*\(.+\)\s*)(cycle|exit)\s+([A-Za-z_][A-Za-z0-9_]*)\s*$",
                line,
                re.IGNORECASE,
            )
            if m:
                head = m.group(1)
                stmt = m.group(2).lower()
                target_var = m.group(3).lower()
                target_label = ""
                for loop_var_name, loop_label in reversed(loop_label_stack):
                    if loop_var_name == target_var:
                        target_label = loop_label
                        break
                if target_label:
                    emit_line = f"{head}{stmt} {target_label}"
        out.append((" " * (base_indent + 3 * indent)) + emit_line)
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
    transpiled, rep_vars, for_array_vars = transpile_lines(lines)
    if not CONST_PARAMS and const_params:
        CONST_PARAMS = const_params
    rendered = render_fortran(transpiled, ranks, loop_vars, rep_vars, int_vars, for_array_vars)

    if args.output:
        Path(args.output).write_text(rendered + "\n", encoding="utf-8")
    else:
        print(rendered)


if __name__ == "__main__":
    main()
