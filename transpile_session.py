#!/usr/bin/env python3
import argparse
import re
from pathlib import Path

ARRAY_FUNCS = {
    "arange",
    "irange",
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
    "mixnoise",
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
    "arspec",
    "arspecaic",
    "armaspec",
    "armaspecaic",
    "arma_mt_spec",
    "armaaic_mt_spec",
    "welchspec",
    "pgramspec",
    "acfspec",
    "mtspec",
    "fiacf",
    "fracdiff",
    "aracf",
    "maacf",
    "arpacf",
    "mapacf",
    "armaacf",
    "arfimaacf",
    "armapacf",
    "armastab",
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
    "huber_mean",
    "bisquare_mean",
    "mad",
    "iqr",
    "iqr_scale",
    "jb_test",
    "ttest1",
    "ttest2",
    "ks2_test",
    "adf_stat",
    "kernelreg",
    "lowess",
    "lowesscv",
    "knnreg",
    "knnregcv",
    "splinereg",
    "naturalspline",
    "mssk",
    "mssk_normal",
    "mssk_gaussian",
    "mssk_uniform",
    "mssk_exponential",
    "mssk_lognormal",
    "mssk_logistic",
    "mssk_double_exponential",
    "mssk_chi2",
    "mssk_chisquare",
    "mssk_hyperbolic",
    "mssk_student_t",
    "mssk_noncentral_t",
    "mssk_unif",
    "mssk_norm",
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
    "mssk_cauchy",
    "mssk_ged",
    "mssk_hyperb",
    "skew_gamma",
    "skew_lnorm",
    "skew_nct",
    "skew_chisq",
    "skew_f",
    "skew_beta",
    "kurt_gamma",
    "kurt_lnorm",
    "kurt_t",
    "kurt_nct",
    "kurt_chisq",
    "kurt_f",
    "kurt_beta",
    "kurt_ged",
    "kurt_hyperb",
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
    "polyroots",
}
SCALAR_FUNCS = {
    "sum",
    "mean",
    "sd",
    "iqr",
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
    "skew_gamma",
    "skew_lnorm",
    "skew_nct",
    "skew_chisq",
    "skew_f",
    "skew_beta",
    "kurt_gamma",
    "kurt_lnorm",
    "kurt_t",
    "kurt_nct",
    "kurt_chisq",
    "kurt_f",
    "kurt_beta",
    "kurt_ged",
    "kurt_hyperb",
    "adf_stat",
    "phillips_perron_stat",
}
CALL_ONLY = {
    "plot",
    "plot_to_label",
    "print_stats",
    "regress",
    "adf",
    "phillips_perron",
    "huber_regress",
    "bisquare_regress",
    "dist_regress",
    "regress_multi",
    "arsimfit",
    "masimfit",
    "arfit",
    "mafit",
    "armafit",
    "armasimfit",
    "armafitgrid",
    "armafitaic",
    "araic",
    "maaic",
    "arfimafit",
    "acfpacf",
    "acfpacfar",
    "poly1reg",
    "splinereg",
    "distaicscan",
    "seed",
}
REWRITE_FUNCS = {
    "rnorm": "random_normal",
    "sort": "sorted",
    "stdz": "standardize",
    "read": "read_vec",
    "dot": "dot_product",
    "mssk_normal": "mssk_norm",
    "mssk_gaussian": "mssk_norm",
    "mssk_uniform": "mssk_unif",
    "mssk_exponential": "mssk_exp",
    "mssk_lognormal": "mssk_lnorm",
    "mssk_logistic": "mssk_logis",
    "mssk_double_exponential": "mssk_laplace",
    "mssk_chi2": "mssk_chisq",
    "mssk_chisquare": "mssk_chisq",
    "mssk_hyperbolic": "mssk_hyperb",
    "mssk_student_t": "mssk_t",
    "mssk_noncentral_t": "mssk_nct",
}
INT_VARS = set()
CONST_PARAMS = {}
USER_PROCS = {}
NO_PLOT = False

MODULE_EXPORTS = {
    "util_mod": {
        "arange",
        "irange",
        "grid",
        "zeros",
        "ones",
        "rep",
        "read_vec",
        "reverse",
        "head",
        "tail",
        "polyroots",
    },
    "stats_mod": {
        "mean",
        "sd",
        "cor",
        "cor_spearman",
        "cor_kendall",
        "cor_matrix_print",
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
        "arspec",
        "arspecaic",
        "armaspec",
        "armaspecaic",
        "arma_mt_spec",
        "armaaic_mt_spec",
        "welchspec",
        "pgramspec",
        "acfspec",
        "mtspec",
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
        "armastab",
        "arsim",
        "arsimfit",
        "masimfit",
        "masim",
        "armasim",
        "armasimfit",
        "arfimasim",
        "cpsim",
        "cpfit",
        "cpfitaic",
        "cpfit_aic",
        "resample",
        "trimmean",
        "winsor_mean",
        "huber_mean",
        "bisquare_mean",
        "mad",
        "iqr",
        "iqr_scale",
        "jb_test",
        "ttest1",
        "ttest2",
        "ks2_test",
        "adf_stat",
        "phillips_perron_stat",
        "adf",
        "phillips_perron",
        "kernelreg",
        "lowess",
        "lowesscv",
        "knnreg",
        "knnregcv",
        "splinereg",
        "naturalspline",
        "regress",
        "dist_regress",
        "huber_regress",
        "bisquare_regress",
        "dist_regress",
        "regress_multi",
        "poly1reg",
        "distaicscan",
        "arfit",
        "mafit",
        "armafit",
        "armafitgrid",
        "armafitaic",
        "araic",
        "maaic",
        "arfimafit",
        "mssk",
        "mssk_unif",
        "mssk_norm",
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
        "mssk_cauchy",
        "mssk_ged",
        "mssk_hyperb",
        "skew_gamma",
        "skew_lnorm",
        "skew_nct",
        "skew_chisq",
        "skew_f",
        "skew_beta",
        "kurt_gamma",
        "kurt_lnorm",
        "kurt_t",
        "kurt_nct",
        "kurt_chisq",
        "kurt_f",
        "kurt_beta",
        "kurt_ged",
        "kurt_hyperb",
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
        "mixnoise",
        "rchisq",
        "rf",
        "rbeta",
        "rlogis",
        "rsech",
        "rlaplace",
        "rcauchy",
        "rged",
        "random_seed_init",
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


def is_assignment_lhs(lhs):
    t = lhs.strip()
    m = re.match(r"^[A-Za-z_][A-Za-z0-9_]*", t)
    if not m:
        return False
    rest = t[m.end() :].strip()
    if not rest:
        return True
    if not (rest.startswith("(") and rest.endswith(")")):
        return False
    depth = 0
    for ch in rest:
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
            if depth < 0:
                return False
    return depth == 0


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
        "rcauchy": {
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
        "mssk_norm": {
            0: lambda a: ["0.0", "1.0"],
            1: lambda a: [a[0], "1.0"],
        },
        "mssk_unif": {
            0: lambda a: ["0.0", "1.0"],
        },
        "mssk_uniform": {
            0: lambda a: ["0.0", "1.0"],
        },
        "mssk_logis": {
            0: lambda a: ["0.0", "1.0"],
            1: lambda a: [a[0], "1.0"],
        },
        "mssk_laplace": {
            0: lambda a: ["0.0", "1.0"],
            1: lambda a: [a[0], "1.0"],
        },
        "mssk_cauchy": {
            0: lambda a: ["0.0", "1.0"],
            1: lambda a: [a[0], "1.0"],
        },
        "mssk_norm": {
            0: lambda a: ["0.0", "1.0"],
            1: lambda a: [a[0], "1.0"],
        },
        "mssk_normal": {
            0: lambda a: ["0.0", "1.0"],
            1: lambda a: [a[0], "1.0"],
        },
        "mssk_gaussian": {
            0: lambda a: ["0.0", "1.0"],
            1: lambda a: [a[0], "1.0"],
        },
        "mssk_exponential": {
            0: lambda a: ["1.0"],
        },
        "mssk_lognormal": {
            0: lambda a: ["0.0", "1.0"],
            1: lambda a: [a[0], "1.0"],
        },
        "mssk_logistic": {
            0: lambda a: ["0.0", "1.0"],
            1: lambda a: [a[0], "1.0"],
        },
        "mssk_double_exponential": {
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


def rewrite_arange_args(expr):
    def to_real_arg(a):
        t = a.strip()
        if not t:
            return t
        if re.search(r"_dp\b", t, re.IGNORECASE):
            return t
        if is_real_literal(t):
            return t
        if re.fullmatch(r"[+-]?[0-9]+", t):
            return f"{t}.0"
        if t.startswith("real("):
            return t
        if is_int_expr(t) or t in INT_VARS or t.startswith(("nint(", "int(", "size(")):
            return f"real({t}, kind=dp)"
        return t

    out = []
    i = 0
    pat = re.compile(r"\barange\s*\(", re.IGNORECASE)
    while i < len(expr):
        m = pat.search(expr, i)
        if not m:
            out.append(expr[i:])
            break
        out.append(expr[i:m.start()])
        lpar = m.end() - 1
        depth = 1
        j = lpar + 1
        in_str = False
        while j < len(expr) and depth > 0:
            c = expr[j]
            if c == '"':
                in_str = not in_str
            elif not in_str:
                if c == "(":
                    depth += 1
                elif c == ")":
                    depth -= 1
            j += 1
        if depth != 0:
            out.append(expr[m.start():])
            break
        inner = expr[lpar + 1 : j - 1]
        parts = split_top_level(inner, ",")
        if len(parts) >= 2:
            new_parts = [to_real_arg(p) for p in parts]
            out.append(expr[m.start():lpar + 1] + ", ".join(new_parts) + ")")
        else:
            out.append(expr[m.start():j])
        i = j
    return "".join(out)


def rewrite_int_args(expr):
    def is_int_expr_with_vars(s):
        t = re.sub(r"\s+", "", s)
        if not t:
            return False
        # Only allow identifiers that are known integer vars.
        ids = re.findall(r"[A-Za-z_][A-Za-z0-9_]*", t)
        if any(i not in INT_VARS for i in ids):
            return False
        # Remove identifiers, digits, and operators; if nothing left, ok.
        t2 = re.sub(r"[A-Za-z_][A-Za-z0-9_]*", "", t)
        t2 = re.sub(r"[0-9()+\-*^/]", "", t2)
        return t2 == ""

    def wrap_int_arg(match):
        name = match.group(1)
        arg = match.group(2).strip()
        if "," in arg:
            return f"{name}({arg})"
        if re.fullmatch(r"[0-9]+", arg):
            return f"{name}({arg})"
        if is_int_expr(arg) or is_int_expr_with_vars(arg):
            return f"{name}({arg})"
        if arg in INT_VARS:
            return f"{name}({arg})"
        if arg.startswith(("nint(", "int(", "size(")):
            return f"{name}({arg})"
        return f"{name}(nint({arg}))"

    expr = re.sub(r"\b(random_normal|runif)\s*\(\s*([^)]+?)\s*\)", wrap_int_arg, expr)
    return expr


def rewrite_rt_rnct_args(expr):
    def to_real_arg(a):
        t = a.strip()
        if t.startswith("[") and t.endswith("]"):
            inner = t[1:-1].strip()
            if not inner:
                return t
            parts = [p.strip() for p in split_top_level(inner, ",")]
            out = []
            for p in parts:
                if re.fullmatch(r"[+-]?[0-9]+", p):
                    out.append(f"{p}.0")
                else:
                    out.append(p)
            return "[" + ", ".join(out) + "]"
        if re.search(r"_dp\b", t, re.IGNORECASE):
            return t
        if re.fullmatch(r"[+-]?[0-9]+", t):
            return f"{t}.0"
        if is_real_literal(t):
            return t
        if t.startswith(("real(",)):
            return t
        if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", t):
            return f"real({t}, kind=dp)"
        if is_int_expr(t) or t in INT_VARS or t.startswith(("nint(", "int(", "size(")):
            return f"real({t}, kind=dp)"
        return t

    def rewrite_name(s, fname, idx_real):
        out = []
        i = 0
        pat = re.compile(rf"\b{fname}\s*\(", re.IGNORECASE)
        while i < len(s):
            m = pat.search(s, i)
            if not m:
                out.append(s[i:])
                break
            out.append(s[i : m.start()])
            lpar = m.end() - 1
            depth = 1
            j = lpar + 1
            in_str = False
            while j < len(s) and depth > 0:
                ch = s[j]
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
            if j >= len(s):
                out.append(s[m.start():])
                break
            args = [a.strip() for a in split_top_level(s[lpar + 1 : j], ",")]
            if len(args) > idx_real:
                eq = find_top_level_assign(args[idx_real])
                if eq != -1:
                    k = args[idx_real][:eq].strip()
                    v = args[idx_real][eq + 1 :].strip()
                    args[idx_real] = f"{k}={to_real_arg(v)}"
                else:
                    args[idx_real] = to_real_arg(args[idx_real])
            out.append(f"{fname}(" + ", ".join(args) + ")")
            i = j + 1
        return "".join(out)

    expr = rewrite_name(expr, "rt", 1)
    expr = rewrite_name(expr, "rnct", 1)
    return expr


def rewrite_fit_t_args(expr):
    def to_real_arg(a):
        t = a.strip()
        if t.startswith("[") and t.endswith("]"):
            inner = t[1:-1].strip()
            if not inner:
                return t
            parts = [p.strip() for p in split_top_level(inner, ",")]
            out = []
            for p in parts:
                if re.fullmatch(r"[+-]?[0-9]+", p):
                    out.append(f"{p}.0")
                else:
                    out.append(p)
            return "[" + ", ".join(out) + "]"
        if re.search(r"_dp\b", t, re.IGNORECASE):
            return t
        if re.fullmatch(r"[+-]?[0-9]+", t):
            return f"{t}.0"
        if is_real_literal(t):
            return t
        if t.startswith(("real(",)):
            return t
        if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", t):
            return f"real({t}, kind=dp)"
        if is_int_expr(t) or t in INT_VARS or t.startswith(("nint(", "int(", "size(")):
            return f"real({t}, kind=dp)"
        return t

    def to_df_vec_arg(a):
        t = a.strip()
        tr = to_real_arg(t)
        if tr.startswith("[") and tr.endswith("]"):
            return tr
        if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", t):
            return tr
        if ":" in t:
            return tr
        return "[" + tr + "]"

    out = []
    i = 0
    pat = re.compile(r"\bfit_t\s*\(", re.IGNORECASE)
    while i < len(expr):
        m = pat.search(expr, i)
        if not m:
            out.append(expr[i:])
            break
        out.append(expr[i : m.start()])
        lpar = m.end() - 1
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
            out.append(expr[m.start():])
            break
        args = [a.strip() for a in split_top_level(expr[lpar + 1 : j], ",")]
        if len(args) > 1:
            eq = find_top_level_assign(args[1])
            if eq != -1:
                k = args[1][:eq].strip()
                v = args[1][eq + 1 :].strip()
                if k.lower() == "df":
                    args[1] = f"{k}={to_df_vec_arg(v)}"
                else:
                    args[1] = f"{k}={v}"
            else:
                args[1] = to_df_vec_arg(args[1])
        out.append("fit_t(" + ", ".join(args) + ")")
        i = j + 1
    return "".join(out)


def rewrite_fit_nct_args(expr):
    def to_real_arg(a):
        t = a.strip()
        if t.startswith("[") and t.endswith("]"):
            inner = t[1:-1].strip()
            if not inner:
                return t
            parts = [p.strip() for p in split_top_level(inner, ",")]
            out = []
            for p in parts:
                if re.fullmatch(r"[+-]?[0-9]+", p):
                    out.append(f"{p}.0")
                else:
                    out.append(p)
            return "[" + ", ".join(out) + "]"
        if re.search(r"_dp\b", t, re.IGNORECASE):
            return t
        if re.fullmatch(r"[+-]?[0-9]+", t):
            return f"{t}.0"
        if is_real_literal(t):
            return t
        if t.startswith(("real(",)):
            return t
        if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", t):
            return f"real({t}, kind=dp)"
        if is_int_expr(t) or t in INT_VARS or t.startswith(("nint(", "int(", "size(")):
            return f"real({t}, kind=dp)"
        return t

    def to_df_vec_arg(a):
        t = a.strip()
        tr = to_real_arg(t)
        if tr.startswith("[") and tr.endswith("]"):
            return tr
        if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", t):
            return tr
        if ":" in t:
            return tr
        return "[" + tr + "]"

    def to_logical_arg(a):
        t = a.strip()
        tl = t.lower()
        if tl in {".true.", "true", "t"}:
            return ".true."
        if tl in {".false.", "false", "f"}:
            return ".false."
        if re.fullmatch(r"[+-]?[0-9]+", t):
            return ".false." if int(t) == 0 else ".true."
        return t

    out = []
    i = 0
    pat = re.compile(r"\bfit_nct\s*\(", re.IGNORECASE)
    while i < len(expr):
        m = pat.search(expr, i)
        if not m:
            out.append(expr[i:])
            break
        out.append(expr[i : m.start()])
        lpar = m.end() - 1
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
            out.append(expr[m.start():])
            break
        args = [a.strip() for a in split_top_level(expr[lpar + 1 : j], ",")]
        for idx in range(1, len(args)):
            eq = find_top_level_assign(args[idx])
            if eq != -1:
                k = args[idx][:eq].strip()
                v = args[idx][eq + 1 :].strip()
                kl = k.lower()
                if kl == "df":
                    args[idx] = f"{k}={to_df_vec_arg(v)}"
                elif kl in {"verbose", "full"}:
                    args[idx] = f"{k}={to_logical_arg(v)}"
                else:
                    args[idx] = f"{k}={v}"
            else:
                if idx == 1:
                    args[idx] = to_df_vec_arg(args[idx])
        out.append("fit_nct(" + ", ".join(args) + ")")
        i = j + 1
    return "".join(out)


def rewrite_ttest_args(expr):
    def to_logical_arg(a):
        t = a.strip()
        tl = t.lower()
        if tl in {".true.", "true", "t"}:
            return ".true."
        if tl in {".false.", "false", "f"}:
            return ".false."
        if re.fullmatch(r"[+-]?[0-9]+", t):
            return ".false." if int(t) == 0 else ".true."
        return t

    out = []
    i = 0
    pat = re.compile(r"\bttest2\s*\(", re.IGNORECASE)
    while i < len(expr):
        m = pat.search(expr, i)
        if not m:
            out.append(expr[i:])
            break
        out.append(expr[i : m.start()])
        lpar = m.end() - 1
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
            out.append(expr[m.start() :])
            break
        args = [a.strip() for a in split_top_level(expr[lpar + 1 : j], ",") if a.strip()]
        if len(args) >= 3:
            eq = find_top_level_assign(args[2])
            if eq != -1 and args[2][:eq].strip().lower() == "pooled":
                rhs = args[2][eq + 1 :].strip()
                args[2] = f"pooled={to_logical_arg(rhs)}"
            elif eq == -1:
                args[2] = f"pooled={to_logical_arg(args[2])}"
        out.append("ttest2(" + ", ".join(args) + ")")
        i = j + 1
    return "".join(out)


def rewrite_moment_real_args(expr):
    def to_real_arg(a):
        t = a.strip()
        if not t:
            return t
        if t.startswith("[") and t.endswith("]"):
            inner = t[1:-1].strip()
            if not inner:
                return t
            parts = [p.strip() for p in split_top_level(inner, ",")]
            out = []
            for p in parts:
                if re.fullmatch(r"[+-]?[0-9]+", p):
                    out.append(f"{p}.0")
                else:
                    out.append(p)
            return "[" + ", ".join(out) + "]"
        if re.search(r"_dp\b", t, re.IGNORECASE):
            return t
        if re.fullmatch(r"[+-]?[0-9]+", t):
            return f"{t}.0"
        if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", t):
            return f"real({t}, kind=dp)"
        if is_int_expr(t) or t in INT_VARS or t.startswith(("nint(", "int(", "size(")):
            return f"real({t}, kind=dp)"
        return t

    out = []
    i = 0
    pat = re.compile(r"\b((?:mssk|kurt|skew)_[A-Za-z0-9_]+)\s*\(", re.IGNORECASE)
    while i < len(expr):
        m = pat.search(expr, i)
        if not m:
            out.append(expr[i:])
            break
        fname = m.group(1)
        out.append(expr[i : m.start()])
        lpar = m.end() - 1
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
            out.append(expr[m.start() :])
            break
        args = [a.strip() for a in split_top_level(expr[lpar + 1 : j], ",")]
        for idx in range(len(args)):
            a = args[idx]
            eq = find_top_level_assign(a)
            if eq != -1:
                k = a[:eq].strip()
                v = a[eq + 1 :].strip()
                args[idx] = f"{k}={to_real_arg(v)}"
            else:
                args[idx] = to_real_arg(a)
        out.append(f"{fname}(" + ", ".join(args) + ")")
        i = j + 1
    return "".join(out)


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
        if len(args) >= 8 and "=" not in args[7]:
            args[7] = wrap_vec(args[7])
        for idx in range(2, len(args)):
            a = args[idx]
            eq = find_top_level_assign(a)
            if eq != -1:
                key = a[:eq].strip().lower()
                rhs = a[eq + 1 :].strip()
                if key in {"cp", "mu", "sd", "noise"}:
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
        has_named = any(find_top_level_assign(a) != -1 for a in args)
        # Support shorthand positional form:
        #   cpfitaic(x, max_cp, minseg, plot, plot_ic, verbose)
        # by inserting defaults for mode/criterion.
        if (not has_named) and len(args) >= 2:
            t1 = args[1].strip()
            t1_is_word = re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", t1) is not None
            if not t1_is_word:
                short = args[1:]
                args = [args[0], '"mean"']
                if len(short) >= 1:
                    args.append(short[0])  # max_cp
                if len(short) >= 2:
                    args.append(short[1])  # minseg
                args.append('"aic"')
                if len(short) >= 3:
                    args.append(short[2])  # plot
                if len(short) >= 4:
                    args.append(short[3])  # plot_ic
                if len(short) >= 5:
                    args.append(short[4])  # verbose
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
        has_named = any(find_top_level_assign(a) != -1 for a in args)
        # Short positional form:
        # cpfitaic(x, max_cp, minseg, plot, plot_ic, verbose)
        if (not has_named) and len(args) >= 2:
            t1 = args[1].strip()
            if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", t1) is None:
                short = args[1:]
                args = [args[0], "mean"]
                if len(short) >= 1:
                    args.append(short[0])  # max_cp
                if len(short) >= 2:
                    args.append(short[1])  # minseg
                args.append("aic")
                if len(short) >= 3:
                    args.append(short[2])  # plot
                if len(short) >= 4:
                    args.append(short[3])  # plot_ic
                if len(short) >= 5:
                    args.append(short[4])  # verbose
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
                    if t != a or a.strip().startswith(("'", '"')):
                        args[idx] = t
                    else:
                        args[idx] = to_int_arg(a)
                elif idx == 4:
                    t = maybe_quote_word(a)
                    if t != a or a.strip().startswith(("'", '"')):
                        args[idx] = t
                    else:
                        args[idx] = to_int_arg(a)
                else:
                    args[idx] = to_int_arg(a)
        out.append("cpfitaic(" + ", ".join(args) + ")")
        i = j + 1
    return "".join(out)


def rewrite_distaicscan_args(expr):
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

    def to_logical_arg(a):
        s = a.strip()
        low = s.lower()
        if low in {".true.", "true", "t"}:
            return ".true."
        if low in {".false.", "false", "f"}:
            return ".false."
        return s

    out = []
    i = 0
    while i < len(expr):
        m = re.search(r"\bdistaicscan\s*\(", expr[i:], re.IGNORECASE)
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
            args[1] = to_int_arg(args[1])
        if len(args) >= 3 and "=" not in args[2]:
            args[2] = to_logical_arg(args[2])
        for idx in range(1, len(args)):
            a = args[idx]
            eq = find_top_level_assign(a)
            if eq != -1:
                key = a[:eq].strip().lower()
                rhs = a[eq + 1 :].strip()
                if key == "verbose":
                    args[idx] = f"{a[:eq].strip()}={to_int_arg(rhs)}"
                elif key == "nct":
                    args[idx] = f"{a[:eq].strip()}={to_logical_arg(rhs)}"
        out.append("distaicscan(" + ", ".join(args) + ")")
        i = j + 1
    return "".join(out)


def rewrite_welchspec_args(expr):
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
        m = re.search(r"\bwelchspec\s*\(", expr[i:], re.IGNORECASE)
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
                if key in {"window", "detrend"}:
                    args[idx] = f"{a[:eq].strip()}={maybe_quote_word(rhs)}"
            else:
                # Positional optional args:
                # 1: seglen, 2: overlap, 3: window, 4: nfreq, 5: detrend, 6: plot
                if idx in {3, 5}:
                    args[idx] = maybe_quote_word(a)
        out.append("welchspec(" + ", ".join(args) + ")")
        i = j + 1
    return "".join(out)


def rewrite_ar_method_args(expr):
    def maybe_quote_word(s):
        t = s.strip()
        if not t:
            return t
        if t.startswith(("'", '"')):
            return t
        if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", t):
            return f'"{t}"'
        return t

    for fname in ("arspec", "arspecaic", "arfit", "arsimfit"):
        out = []
        i = 0
        pat = re.compile(rf"\b{fname}\s*\(", re.IGNORECASE)
        while i < len(expr):
            m = pat.search(expr, i)
            if not m:
                out.append(expr[i:])
                break
            out.append(expr[i : m.start()])
            lpar = m.end() - 1
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
                out.append(expr[m.start() :])
                break
            args = [a.strip() for a in split_top_level(expr[lpar + 1 : j], ",")]
            for idx in range(1, len(args)):
                a = args[idx]
                eq = find_top_level_assign(a)
                if eq != -1:
                    key = a[:eq].strip().lower()
                    rhs = a[eq + 1 :].strip()
                    if key == "method":
                        args[idx] = f"{a[:eq].strip()}={maybe_quote_word(rhs)}"
            out.append(f"{fname}(" + ", ".join(args) + ")")
            i = j + 1
        expr = "".join(out)
    return expr


def rewrite_mtspec_args(expr):
    def to_logical(s):
        t = s.strip().lower()
        if t in {".true.", "true", "t", "1", "1.0", "1.0_dp", "1.0d0"}:
            return ".true."
        if t in {".false.", "false", "f", "0", "0.0", "0.0_dp", "0.0d0"}:
            return ".false."
        return s

    out = []
    i = 0
    while i < len(expr):
        m = re.search(r"\bmtspec\s*\(", expr[i:], re.IGNORECASE)
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
        for idx in range(len(args)):
            a = args[idx]
            eq = find_top_level_assign(a)
            if eq != -1:
                key = a[:eq].strip().lower()
                rhs = a[eq + 1 :].strip()
                if key in {"demean", "plot"}:
                    args[idx] = f"{a[:eq].strip()}={to_logical(rhs)}"
            else:
                # positional: x, nfreq, nw, k, demean, plot
                if idx in {4, 5}:
                    args[idx] = to_logical(a)
        out.append("mtspec(" + ", ".join(args) + ")")
        i = j + 1
    return "".join(out)


def rewrite_pgramspec_args(expr):
    def is_int_list(s):
        t = s.strip()
        if not (t.startswith("[") and t.endswith("]")):
            return False
        inner = t[1:-1].strip()
        if not inner:
            return False
        parts = [p.strip() for p in split_top_level(inner, ",")]
        return all(re.fullmatch(r"[+-]?[0-9]+", p) for p in parts)

    def wrap_smooth(s):
        t = s.strip()
        if not t:
            return t
        if t.startswith(("nint(", "int(")):
            return t
        if is_int_list(t) or re.fullmatch(r"[+-]?[0-9]+", t):
            return t
        return f"nint(1.0*({t}))"

    out = []
    i = 0
    while i < len(expr):
        m = re.search(r"\bpgramspec\s*\(", expr[i:], re.IGNORECASE)
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
        for idx in range(len(args)):
            a = args[idx]
            eq = find_top_level_assign(a)
            if eq != -1:
                key = a[:eq].strip().lower()
                rhs = a[eq + 1 :].strip()
                if key == "smooth":
                    args[idx] = f"{a[:eq].strip()}={wrap_smooth(rhs)}"
            else:
                # positional smooth is 5th arg (index 4)
                if idx == 4:
                    args[idx] = wrap_smooth(a)
        out.append("pgramspec(" + ", ".join(args) + ")")
        i = j + 1
    return "".join(out)


def rewrite_acfspec_args(expr):
    def is_int_list(s):
        t = s.strip()
        if not (t.startswith("[") and t.endswith("]")):
            return False
        inner = t[1:-1].strip()
        if not inner:
            return False
        parts = [p.strip() for p in split_top_level(inner, ",")]
        return all(re.fullmatch(r"[+-]?[0-9]+", p) for p in parts)

    def wrap_m(s):
        t = s.strip()
        if not t:
            return t
        if t.startswith(("nint(", "int(")):
            return t
        if is_int_list(t) or re.fullmatch(r"[+-]?[0-9]+", t):
            return t
        return f"nint(1.0*({t}))"

    def maybe_quote_word(s):
        t = s.strip()
        if not t:
            return t
        if t.startswith(("'", '"')):
            return t
        if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", t):
            return f'"{t}"'
        return t

    def to_logical(s):
        t = s.strip().lower()
        if t in {".true.", "true", "t", "1", "1.0", "1.0_dp", "1.0d0"}:
            return ".true."
        if t in {".false.", "false", "f", "0", "0.0", "0.0_dp", "0.0d0"}:
            return ".false."
        return s

    out = []
    i = 0
    while i < len(expr):
        m = re.search(r"\bacfspec\s*\(", expr[i:], re.IGNORECASE)
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
        for idx in range(len(args)):
            a = args[idx]
            eq = find_top_level_assign(a)
            if eq != -1:
                key = a[:eq].strip().lower()
                rhs = a[eq + 1 :].strip()
                if key == "m":
                    args[idx] = f"{a[:eq].strip()}={wrap_m(rhs)}"
                elif key == "window":
                    args[idx] = f"{a[:eq].strip()}={maybe_quote_word(rhs)}"
                elif key == "plot":
                    args[idx] = f"{a[:eq].strip()}={to_logical(rhs)}"
            else:
                if idx == 1:
                    args[idx] = wrap_m(a)
                elif idx == 3:
                    args[idx] = maybe_quote_word(a)
                elif idx == 4:
                    args[idx] = to_logical(a)
        out.append("acfspec(" + ", ".join(args) + ")")
        i = j + 1
    return "".join(out)


def rewrite_iter_arg(expr):
    def rewrite_one(fname):
        out = []
        i = 0
        pat = re.compile(rf"\b{fname}\s*\(", re.IGNORECASE)
        while i < len(expr):
            m = pat.search(expr, i)
            if not m:
                out.append(expr[i:])
                break
            out.append(expr[i : m.start()])
            lpar = m.end() - 1
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
                out.append(expr[m.start() :])
                break
            args = [a.strip() for a in split_top_level(expr[lpar + 1 : j], ",")]
            for idx in range(len(args)):
                a = args[idx]
                eq = find_top_level_assign(a)
                if eq != -1:
                    key = a[:eq].strip().lower()
                    if key == "iter":
                        args[idx] = "niter" + a[eq:]
            out.append(f"{fname}(" + ", ".join(args) + ")")
            i = j + 1
        return "".join(out)

    for fname in ("arspecaic", "arspec", "armaspec", "welchspec", "pgramspec", "mtspec", "masimfit", "armasimfit"):
        expr = rewrite_one(fname)
    return expr


def rewrite_ar_order_args(expr):
    def is_vector_expr(t):
        return ("[" in t and "]" in t) or re.search(r"\b(arange|irange)\s*\(", t, re.IGNORECASE)

    def int_vector_to_real_vector(t):
        tt = t.strip()
        if tt.startswith("[") and tt.endswith("]"):
            inner = tt[1:-1].strip()
            if not inner:
                return tt
            parts = [p.strip() for p in split_top_level(inner, ",")]
            if all(re.fullmatch(r"[+-]?[0-9]+", p) for p in parts):
                return "[" + ", ".join(f"{p}.0" for p in parts) + "]"
        return tt

    def to_real_order_vec(t):
        tt = t.strip()
        if not tt:
            return tt
        if re.fullmatch(r"[+-]?[0-9]+", tt):
            return f"[{tt}.0]"
        if is_int_expr(tt) or tt in INT_VARS:
            return f"[1.0*({tt})]"
        if re.search(r"\birange\s*\(", tt, re.IGNORECASE):
            return f"1.0*({tt})"
        return int_vector_to_real_vector(tt)

    def wrap_if_needed(s):
        t = s.strip()
        if not t:
            return t
        if t.startswith(("nint(", "int(", "size(")):
            return t
        if re.fullmatch(r"[0-9]+", t):
            return t
        if is_int_expr(t):
            return t
        if t in INT_VARS:
            return t
        return f"nint(1.0*({t}))"

    def maxval_int(t):
        tt = t.strip()
        if "arange(" in tt.lower() or tt.startswith(("nint(", "int(")) or re.search(r"\bnint\s*\(", tt):
            return f"maxval({tt})"
        return f"nint(maxval({tt}))"

    def rewrite_one(fname, pos_keys):
        out = []
        i = 0
        pat = re.compile(rf"\b{fname}\s*\(", re.IGNORECASE)
        while i < len(expr):
            m = pat.search(expr, i)
            if not m:
                out.append(expr[i:])
                break
            out.append(expr[i : m.start()])
            lpar = m.end() - 1
            depth = 1
            j = lpar + 1
            in_str = False
            while j < len(expr) and depth > 0:
                ch = expr[j]
                if ch == '\"':
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
                out.append(expr[m.start() :])
                break
            args = [a.strip() for a in split_top_level(expr[lpar + 1 : j], ",")]
            for idx in range(len(args)):
                a = args[idx]
                eq = find_top_level_assign(a)
                if eq != -1:
                    key = a[:eq].strip().lower()
                    rhs = a[eq + 1 :].strip()
                    if key in pos_keys:
                        args[idx] = f"{a[:eq].strip()}={wrap_if_needed(rhs)}"
                    elif fname == "armasimfit" and key in {"pvec", "qvec"}:
                        rr = rhs
                        if re.match(r"\s*arange\s*\(", rr, re.IGNORECASE):
                            rr = "irange(" + rr[rr.find("(") + 1 : rr.rfind(")")] + ")"
                        args[idx] = f"{a[:eq].strip()}={to_real_order_vec(rr)}"
                else:
                    if idx == 1:
                        if fname in {"arsimfit", "masimfit", "armasimfit"}:
                            args[idx] = a
                        else:
                            args[idx] = wrap_if_needed(a)
                    if fname == "armasimfit" and idx in {3, 4}:
                        rr = a
                        if re.match(r"\s*arange\s*\(", rr, re.IGNORECASE):
                            rr = "irange(" + rr[rr.find("(") + 1 : rr.rfind(")")] + ")"
                        args[idx] = to_real_order_vec(rr)

            # Special handling: allow vector order for arfit/arspec by
            # reducing to a scalar or a range.
            if len(args) >= 2:
                arg1 = args[1]
                if fname == "arfit" and is_vector_expr(arg1):
                    # arfit(x, arange(n)) -> arfit(x, 1, maxval(arange(n)))
                    if len(args) == 2:
                        args = [args[0], "1", maxval_int(arg1)]
                if fname == "arspec" and is_vector_expr(arg1):
                    args[1] = maxval_int(arg1)
            if fname == "arsimfit" and len(args) >= 3:
                arg2 = args[2]
                m2 = re.match(r"\s*arange\s*\((.*)\)\s*$", arg2, re.IGNORECASE)
                if m2:
                    inner = m2.group(1)
                    parts = [p.strip() for p in split_top_level(inner, ",") if p.strip()]
                    out_parts = []
                    for p in parts:
                        if re.fullmatch(r"[+-]?[0-9]+", p) or is_int_expr(p) or p in INT_VARS:
                            out_parts.append(p)
                        else:
                            out_parts.append(f"nint({p})")
                    args[2] = "irange(" + ", ".join(out_parts) + ")"
            if fname == "masimfit" and len(args) >= 3:
                arg2 = args[2]
                m2 = re.match(r"\s*arange\s*\((.*)\)\s*$", arg2, re.IGNORECASE)
                if m2:
                    inner = m2.group(1)
                    parts = [p.strip() for p in split_top_level(inner, ",") if p.strip()]
                    out_parts = []
                    for p in parts:
                        if re.fullmatch(r"[+-]?[0-9]+", p) or is_int_expr(p) or p in INT_VARS:
                            out_parts.append(p)
                        else:
                            out_parts.append(f"nint({p})")
                    args[2] = "irange(" + ", ".join(out_parts) + ")"
            out.append(f"{fname}(" + ", ".join(args) + ")")
            i = j + 1
        return "".join(out)

    expr = rewrite_one("arfit", {"p", "order", "k1"})
    expr = rewrite_one("arspec", {"p", "order"})
    expr = rewrite_one("arspecaic", {"pmax", "p"})
    expr = rewrite_one("arsimfit", {"k1", "order"})
    expr = rewrite_one("masimfit", {"k1", "order", "kvec"})
    expr = rewrite_one("armasimfit", set())
    return expr


def transpile_expr(expr):
    expr = normalize_proc_calls_in_expr(expr)
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
    expr = rewrite_distaicscan_args(expr)
    expr = rewrite_welchspec_args(expr)
    expr = rewrite_ar_method_args(expr)
    expr = rewrite_mtspec_args(expr)
    expr = rewrite_pgramspec_args(expr)
    expr = rewrite_acfspec_args(expr)
    expr = rewrite_iter_arg(expr)
    expr = rewrite_ar_order_args(expr)
    expr = rewrite_rt_rnct_args(expr)
    expr = rewrite_fit_t_args(expr)
    expr = rewrite_fit_nct_args(expr)
    expr = rewrite_ttest_args(expr)
    expr = rewrite_moment_real_args(expr)
    expr = rewrite_arange_args(expr)
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
    # huber_mean is scalar for huber_mean(x) or scalar c, but array for vector c.
    m_huber = re.match(r"^\s*huber_mean\s*\((.*)\)\s*$", rhs)
    if m_huber:
        args = split_top_level(m_huber.group(1), ",")
        if len(args) <= 1:
            return "scalar"
        c_arg = args[1].strip()
        if "[" in c_arg and "]" in c_arg:
            return "array"
        if re.search(r"\b\w+\s*\([^)]*:\s*[^)]*\)", c_arg):
            return "array"
        for name in extract_identifiers(c_arg):
            if name in known_arrays and re.search(rf"\b{name}\b(?!\s*\()", c_arg):
                return "array"
        return "scalar"
    m_bisquare = re.match(r"^\s*bisquare_mean\s*\((.*)\)\s*$", rhs)
    if m_bisquare:
        args = split_top_level(m_bisquare.group(1), ",")
        if len(args) <= 1:
            return "scalar"
        c_arg = args[1].strip()
        if "[" in c_arg and "]" in c_arg:
            return "array"
        if re.search(r"\b\w+\s*\([^)]*:\s*[^)]*\)", c_arg):
            return "array"
        for name in extract_identifiers(c_arg):
            if name in known_arrays and re.search(rf"\b{name}\b(?!\s*\()", c_arg):
                return "array"
        return "scalar"
    # Common scalar reducers/tests should stay scalar even when their arguments
    # contain array variables.
    for fn in SCALAR_FUNCS:
        if re.search(rf"^\s*{fn}\s*\(", rhs):
            return "scalar"
    for fn in ARRAY_FUNCS:
        if re.search(rf"\b{fn}\s*\(", rhs):
            if fn in {"runif", "rnorm", "random_normal"}:
                if re.search(rf"\b{fn}\s*\(\s*\)", rhs):
                    return "scalar"
            return "array"
    for name in extract_identifiers(rhs):
        if name in known_arrays:
            # If the name appears without indexing, treat as array.
            if re.search(rf"\b{name}\b(?!\s*\()", rhs):
                return "array"
            # If any slice is used, treat as array.
            if re.search(rf"\b{name}\s*\([^)]*:\s*[^)]*\)", rhs):
                return "array"
            # Otherwise, only scalar element indexing is present; ignore.
    return "scalar"


def is_int_literal(expr):
    s = expr.strip()
    return re.fullmatch(r"[0-9]+", s) is not None


def is_real_literal(expr):
    s = expr.strip()
    return re.fullmatch(r"([0-9]*\.[0-9]+|[0-9]+\.[0-9]*)([eE][+-]?[0-9]+)?", s) is not None or (
        re.fullmatch(r"[0-9]+([eE][+-]?[0-9]+)", s) is not None
    )


def to_dp_if_int_literal(expr):
    s = expr.strip()
    if re.fullmatch(r"[+-]?[0-9]+", s):
        return f"{s}.0"
    if s.startswith("[") and s.endswith("]"):
        inner = s[1:-1].strip()
        if not inner:
            return expr
        parts = [p.strip() for p in split_top_level(inner, ",")]
        conv = []
        all_simple = True
        for p in parts:
            if re.fullmatch(r"[+-]?[0-9]+", p):
                conv.append(f"{p}.0")
            else:
                all_simple = False
                break
        if all_simple:
            return "[" + ", ".join(conv) + "]"
    return expr


def is_int_expr(expr):
    s = re.sub(r"\s+", "", expr)
    if not s:
        return False
    return re.fullmatch(r"[0-9()+\-*^]+", s) is not None


def infer_from_lines(lines, seed_arrays=None):
    ranks = {}
    loop_vars = set()
    int_vars = set()
    const_params = {}
    seed_arrays = set(seed_arrays or [])
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
            m_call = re.match(r"call\s+([A-Za-z_][A-Za-z0-9_]*)\s*\((.*)\)\s*$", stmt, re.IGNORECASE)
            if m_call:
                for a in split_top_level(m_call.group(2), ","):
                    nm, rhs = parse_call_actual(a.strip())
                    expr = rhs if nm is not None else a.strip()
                    if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", expr):
                        ranks.setdefault(expr, "scalar")
                continue
            eqpos = find_top_level_assign(stmt)
            if eqpos != -1 and is_assignment_lhs(stmt[:eqpos]):
                lhs = stmt[:eqpos].strip()
                rhs = stmt[eqpos + 1 :].strip()
                if "(" in lhs and ")" in lhs:
                    continue
                name = lhs
                if is_int_expr(rhs):
                    int_vars.add(name)
                rank = infer_rank(rhs, set(seed_arrays) | {k for k, v in ranks.items() if v == "array"})
                prev = ranks.get(name)
                if prev == "array" or rank == "array":
                    ranks[name] = "array"
                else:
                    ranks[name] = "scalar"
            else:
                m_fn = re.match(r"\s*([A-Za-z_][A-Za-z0-9_]*)\s*\((.*)\)\s*$", stmt)
                if m_fn:
                    fname = m_fn.group(1)
                    args_raw = m_fn.group(2)
                    args_list = [a.strip() for a in split_top_level(args_raw, ",")]
                    if fname.lower() == "armaspec" and args_list:
                        nm0, rhs0 = parse_call_actual(args_list[0])
                        if nm0 is None and re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", rhs0):
                            ranks[rhs0] = "array"
                    for idx_arg, a in enumerate(args_list):
                        nm, rhs = parse_call_actual(a.strip())
                        if nm is not None:
                            key = nm.strip().lower()
                            if key in {"method", "mode", "window", "detrend", "criterion", "plot"}:
                                if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", rhs):
                                    continue
                            expr = rhs
                        else:
                            expr = a.strip()
                            if fname.lower() == "dist_regress" and idx_arg == 0:
                                # distribution tag (e.g. t/normal) is not a variable
                                continue
                            if fname.lower() == "welchspec" and idx_arg in {3, 5}:
                                if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", expr):
                                    continue
                        if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", expr):
                            ranks.setdefault(expr, "scalar")
                        elif re.search(r"\b([A-Za-z_][A-Za-z0-9_]*)\s*\([^)]*:\s*[^)]*\)", expr):
                            name = re.findall(r"\b([A-Za-z_][A-Za-z0-9_]*)\s*\(", expr)[0]
                            ranks[name] = "array"
                    continue
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
        if comment and "should error" in comment.lower():
            out.append("! " + stripped + " ! " + comment)
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


def transpile_procedure(proc, main_ranks):
    hdr = proc["header"]
    kind = hdr["kind"]
    name = hdr["name"]
    args = hdr["args"]
    # collect intents from body declarations
    intents = {a.lower(): ("in" if kind == "function" else "inout") for a in args}
    body_src = []
    for raw in proc["body_lines"]:
        line = strip_prompt(raw.rstrip("\n"))
        code, comment = split_comment(line)
        s = code.strip()
        if not s:
            continue
        m_int = re.match(r"intent\s*\(([^)]*)\)\s*::\s*(.+)$", s, re.IGNORECASE)
        if m_int:
            mode = re.sub(r"\s+", "", m_int.group(1).strip().lower())
            if mode == "inout":
                mode = "inout"
            elif mode == "in":
                mode = "in"
            elif mode == "out":
                mode = "out"
            else:
                continue
            for a in split_top_level(m_int.group(2), ","):
                an = a.strip().lower()
                if an in intents:
                    intents[an] = mode
            continue
        line_out = []
        for stmt in split_top_level(s, ";"):
            stmt = stmt.strip()
            if stmt:
                line_out.extend(transpile_statement(stmt))
        if comment and line_out:
            line_out[-1] = line_out[-1] + " ! " + comment
        body_src.extend(line_out)

    def infer_arg_num_type(actual_expr, int_names):
        s = actual_expr.strip()
        if re.fullmatch(r"[+-]?[0-9]+", s):
            return "integer"
        if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", s):
            return "integer" if s in int_names else "real"
        return "real"

    # infer arg ranks/types from call sites in transpiled main lines
    arg_ranks = {a.lower(): "scalar" for a in args}
    arg_types = {a.lower(): "integer" for a in args}
    int_names = set(main_ranks.get("_int_vars", set()))
    call_pat = re.compile(rf"\b{name}\s*\((.*)\)", re.IGNORECASE)
    for ml in main_ranks.get("_transpiled_main_lines", []):
        m = call_pat.search(ml)
        if not m:
            continue
        raw_args = m.group(1)
        actuals = [a.strip() for a in split_top_level(raw_args, ",") if a.strip()]
        amap = {}
        pos = 0
        saw_named = False
        for t in actuals:
            nm, rhs = parse_call_actual(t)
            if nm is not None:
                saw_named = True
                amap[nm.lower()] = rhs
            else:
                if saw_named:
                    continue
                if pos < len(args):
                    amap[args[pos].lower()] = t
                    pos += 1
        for a in args:
            if a.lower() not in amap:
                continue
            rk = infer_rank(amap[a.lower()], {k for k, v in main_ranks.items() if v == "array"})
            if rk == "array":
                arg_ranks[a.lower()] = "array"
                arg_types[a.lower()] = "real"
            else:
                if infer_arg_num_type(amap[a.lower()], int_names) != "integer":
                    arg_types[a.lower()] = "real"

    # infer local vars in body
    seed_arrays = {a.lower() for a in args if arg_ranks[a.lower()] == "array"}
    body_ranks, body_loop_vars, body_int_vars, _ = infer_from_lines(body_src, seed_arrays=seed_arrays)
    locals_all = sorted(
        [
            v
            for v in body_ranks.keys()
            if v.lower() not in {a.lower() for a in args}
            and not (kind == "function" and v.lower() == name.lower())
        ]
    )
    local_arrays = [v for v in locals_all if body_ranks.get(v) == "array"]
    local_scalars = [v for v in locals_all if body_ranks.get(v) == "scalar" and v not in body_int_vars]
    local_ints = sorted([v for v in body_int_vars if v.lower() not in {a.lower() for a in args} and v not in body_loop_vars])

    out = []
    out.append(f"{kind} {name}(" + ", ".join(args) + ")")
    for a in args:
        rk = arg_ranks[a.lower()]
        mode = intents.get(a.lower(), "inout")
        attr = f"intent({mode})"
        if rk == "array":
            out.append(f"real(kind=dp), {attr} :: {a}(:)")
        else:
            if arg_types[a.lower()] == "integer":
                out.append(f"integer, {attr} :: {a}")
            else:
                out.append(f"real(kind=dp), {attr} :: {a}")
    if kind == "function" and name.lower() not in {a.lower() for a in args}:
        if body_ranks.get(name, "scalar") == "array":
            out.append(f"real(kind=dp), allocatable :: {name}(:)")
        else:
            out.append(f"real(kind=dp) :: {name}")
    if body_loop_vars:
        out.append("integer :: " + ", ".join(sorted(body_loop_vars)))
    if local_ints:
        out.append("integer :: " + ", ".join(local_ints))
    if local_scalars:
        out.append("real(kind=dp) :: " + ", ".join(local_scalars))
    if local_arrays:
        out.append("real(kind=dp), allocatable :: " + ", ".join(f"{a}(:)" for a in local_arrays))
    out.append("")
    out.extend(body_src)
    out.append(f"end {kind} {name}")
    return out


def comment_out_plot_calls(lines):
    out = []
    for line in lines:
        if re.match(r"^\s*call\s+plot\s*\(", line, re.IGNORECASE):
            out.append("! " + line.strip())
        else:
            out.append(line)
    return out


def transpile_statement(stmt):
    s = stmt.strip()
    low = s.lower()
    def parse_cor_methods(method_expr):
        t = method_expr.strip()
        if not t:
            return None
        if t.startswith("[") and t.endswith("]"):
            inner = t[1:-1].strip()
            if not inner:
                return []
            raw = [a.strip() for a in split_top_level(inner, ",") if a.strip()]
        else:
            raw = [t]
        out = []
        for tok in raw:
            u = tok.strip()
            if (u.startswith('"') and u.endswith('"')) or (u.startswith("'") and u.endswith("'")):
                u = u[1:-1].strip()
            u = u.lower()
            if u in {"pearson", "spearman", "kendall"}:
                out.append(u)
            else:
                return None
        return out
    def intcp_cond(expr):
        e = expr.strip()
        low_e = e.lower()
        if low_e in {".true.", "true", "t"}:
            return ".true."
        if low_e in {".false.", "false", "f"}:
            return ".false."
        return f"({e} /= 0.0_dp)"
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
    m_cor = re.match(r"^\s*cor\s*\((.*)\)\s*$", s, re.IGNORECASE)
    if m_cor:
        args = [a.strip() for a in split_top_level(m_cor.group(1), ",") if a.strip()]
        if len(args) < 2:
            return [f"! {s}"]
        vec_args = []
        method_expr = None
        for a in args:
            eq = find_top_level_assign(a)
            if eq != -1 and a[:eq].strip().lower() == "method":
                method_expr = a[eq + 1 :].strip()
            else:
                vec_args.append(transpile_expr(a))
        methods = ["pearson"] if method_expr is None else parse_cor_methods(method_expr)
        if methods is None or len(methods) < 1:
            return [f"! {s}"]

        if len(vec_args) == 2:
            fx = vec_args[0]
            fy = vec_args[1]
            exprs = []
            for mname in methods:
                if mname == "pearson":
                    exprs.append(f"cor({fx}, {fy})")
                elif mname == "spearman":
                    exprs.append(f"cor_spearman({fx}, {fy})")
                else:
                    exprs.append(f"cor_kendall({fx}, {fy})")
            if len(exprs) == 1:
                return [f"print *, {exprs[0]}"]
            return ["print *, [" + ", ".join(exprs) + "]"]

        ncol = len(vec_args)
        out = [
            "block",
            "real(kind=dp), allocatable :: xcor_tmp(:,:)",
            f"character(len=16) :: xcor_lbl({ncol})",
            f"allocate(xcor_tmp(size({vec_args[0]}), {ncol}))",
        ]
        for j, xj in enumerate(vec_args, start=1):
            out.append(f"xcor_tmp(:, {j}) = {xj}")

        lbls = []
        for j, src in enumerate(vec_args, start=1):
            if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", src):
                lbls.append(f'"{src}"')
            else:
                lbls.append(f'"x{j}"')
        out.append("xcor_lbl = [character(len=16) :: " + ", ".join(lbls) + "]")

        if len(methods) == 1 and methods[0] == "pearson":
            out.append("call cor_matrix_print(xcor_tmp, xcor_lbl)")
        else:
            mitems = ", ".join([f'"{m}"' for m in methods])
            out.append(
                "call cor_matrix_print(xcor_tmp, xcor_lbl, "
                + f'methods=[character(len=8) :: {mitems}])'
            )
        out.append("end block")
        return out
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
    looks_like_single_call = re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*\s*\(.*\)\s*", s) is not None
    if '"' in s and find_top_level_assign(s) == -1 and not looks_like_single_call:
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
    if low.startswith("set plotout"):
        return [f"! {s}"]
    if low.startswith("seed("):
        inner = s[s.find("(") + 1 : s.rfind(")")]
        args = [a.strip() for a in split_top_level(inner, ",") if a.strip()]
        args = [transpile_expr(a) for a in args]
        return [f"call random_seed_init(" + ", ".join(args) + ")"]
    if re.fullmatch(r"run\s*\(.*\)\s*", s, re.IGNORECASE):
        return [f"! {s}"]
    if low.startswith("call "):
        return [transpile_expr(s)]
    m_reg = re.match(r"regress\s*\((.*)\)\s*$", s, re.IGNORECASE)
    if m_reg:
        args = [a.strip() for a in split_top_level(m_reg.group(1), ",") if a.strip()]
        if len(args) < 2:
            return [f"! {s}"]
        y_expr = transpile_expr(args[0])
        x_exprs = []
        intcp_expr = None
        for a in args[1:]:
            eq = find_top_level_assign(a)
            if eq != -1 and a[:eq].strip().lower() == "intcp":
                intcp_expr = transpile_expr(a[eq + 1 :].strip())
            else:
                x_exprs.append(transpile_expr(a))
        if len(x_exprs) == 1:
            if intcp_expr is None:
                return [f"call regress({y_expr}, {x_exprs[0]})"]
            return [f"call regress({y_expr}, {x_exprs[0]}, intcp={intcp_cond(intcp_expr)})"]
        ncol = len(x_exprs)
        out = [
            "block",
            f"real(kind=dp), allocatable :: xreg_tmp(:,:)",
            f"character(len=16) :: xreg_lbl({ncol})",
            f"allocate(xreg_tmp(size({y_expr}), {ncol}))",
        ]
        for j, xj in enumerate(x_exprs, start=1):
            out.append(f"xreg_tmp(:, {j}) = {xj}")
        lbls = ", ".join([f'\"x{j}\"' for j in range(1, ncol + 1)])
        out.append(f"xreg_lbl = [character(len=16) :: {lbls}]")
        if intcp_expr is None:
            out.append(f"call regress_multi({y_expr}, xreg_tmp, xreg_lbl)")
        else:
            out.append(f"call regress_multi({y_expr}, xreg_tmp, xreg_lbl, intcp={intcp_cond(intcp_expr)})")
        out.append("end block")
        return out
    m_rreg = re.match(r"(huber_regress|bisquare_regress)\s*\((.*)\)\s*$", s, re.IGNORECASE)
    if m_rreg:
        reg_name = m_rreg.group(1).lower()
        args = [a.strip() for a in split_top_level(m_rreg.group(2), ",") if a.strip()]
        if len(args) < 2:
            return [f"! {s}"]
        y_expr = transpile_expr(args[0])
        x_exprs = []
        intcp_expr = None
        c_expr = None
        for a in args[1:]:
            eq = find_top_level_assign(a)
            if eq != -1 and a[:eq].strip().lower() == "intcp":
                intcp_expr = transpile_expr(a[eq + 1 :].strip())
            elif eq != -1 and a[:eq].strip().lower() == "c":
                c_expr = transpile_expr(a[eq + 1 :].strip())
            else:
                x_exprs.append(transpile_expr(a))
        if len(x_exprs) != 1:
            return [f"! {s}"]
        call = f"call {reg_name}({y_expr}, {x_exprs[0]}"
        if c_expr is not None:
            call += f", c={c_expr}"
        if intcp_expr is not None:
            call += f", intcp={intcp_cond(intcp_expr)}"
        call += ")"
        return [call]
    m_dreg = re.match(r"dist_regress\s*\((.*)\)\s*$", s, re.IGNORECASE)
    if m_dreg:
        def to_real_vec_expr(expr):
            t = expr.strip()
            if not t:
                return t
            if t.startswith("[") and t.endswith("]"):
                inner = t[1:-1].strip()
                if not inner:
                    return t
                parts = [p.strip() for p in split_top_level(inner, ",")]
                out = []
                for p in parts:
                    if re.fullmatch(r"[+-]?[0-9]+", p):
                        out.append(f"{p}.0_dp")
                    else:
                        out.append(transpile_expr(p))
                return "[" + ", ".join(out) + "]"
            t0 = transpile_expr(t)
            if re.fullmatch(r"[+-]?[0-9]+", t0):
                return f"[{t0}.0_dp]"
            return "[" + t0 + "]"

        args = [a.strip() for a in split_top_level(m_dreg.group(1), ",") if a.strip()]
        if len(args) < 2:
            return [f"! {s}"]

        dist_raw = args[0].strip()
        if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", dist_raw):
            dist_expr = f'"{dist_raw}"'
        else:
            dist_expr = transpile_expr(dist_raw)
        y_expr = transpile_expr(args[1])

        x_exprs = []
        intcp_expr = None
        df_expr = None
        beta_expr = None
        for a in args[2:]:
            eq = find_top_level_assign(a)
            if eq != -1 and a[:eq].strip().lower() == "intcp":
                intcp_expr = transpile_expr(a[eq + 1 :].strip())
            elif eq != -1 and a[:eq].strip().lower() == "df":
                df_expr = to_real_vec_expr(a[eq + 1 :].strip())
            elif eq != -1 and a[:eq].strip().lower() == "beta":
                beta_expr = to_real_vec_expr(a[eq + 1 :].strip())
            else:
                x_exprs.append(transpile_expr(a))

        call_tail = ""
        if intcp_expr is not None:
            call_tail += f", intcp={intcp_cond(intcp_expr)}"
        if df_expr is not None:
            call_tail += f", df={df_expr}"
        if beta_expr is not None:
            call_tail += f", beta={beta_expr}"

        if len(x_exprs) == 0:
            return [f"call dist_regress({dist_expr}, {y_expr}{call_tail})"]
        if len(x_exprs) == 1:
            return [f"call dist_regress({dist_expr}, {y_expr}, {x_exprs[0]}{call_tail})"]

        ncol = len(x_exprs)
        out = [
            "block",
            "real(kind=dp), allocatable :: xreg_tmp(:,:)",
            f"allocate(xreg_tmp(size({y_expr}), {ncol}))",
        ]
        for j, xj in enumerate(x_exprs, start=1):
            out.append(f"xreg_tmp(:, {j}) = {xj}")
        out.append(f"call dist_regress({dist_expr}, {y_expr}, xreg_tmp{call_tail})")
        out.append("end block")
        return out
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
        low == "do"
        or low.startswith("do ")
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
    if eqpos != -1 and is_assignment_lhs(s[:eqpos]):
        lhs = s[:eqpos].strip()
        # In assignment subscripts, prefer integer ranges.
        lhs_out = re.sub(r"\barange\s*\(", "irange(", lhs, flags=re.IGNORECASE)
        rhs = s[eqpos + 1 :].strip()
        read_calls = find_named_call_spans(rhs, "read")
        if len(read_calls) == 1:
            start, end, args_raw = read_calls[0]
            args = [a.strip() for a in split_top_level(args_raw, ",") if a.strip()]
            if args:
                call_args = [transpile_expr(args[0]), lhs]
                for extra in args[1:]:
                    eq = find_top_level_assign(extra)
                    if eq != -1 and extra[:eq].strip().lower() == "col":
                        call_args.append("icol=" + transpile_expr(extra[eq + 1 :].strip()))
                    else:
                        call_args.append("icol=" + transpile_expr(extra))
                rhs_repl = (rhs[:start] + lhs + rhs[end + 1 :]).strip()
                out_lines = [f"call read_vec({', '.join(call_args)})"]
                if rhs_repl == lhs:
                    return out_lines
                rhs_norm = strip_outer_parens(rhs_repl)
                if has_top_level_relational(rhs_norm):
                    out_lines.append(f"{lhs_out} = merge(1.0_dp, 0.0_dp, {transpile_expr(rhs_repl)})")
                else:
                    out_lines.append(f"{lhs_out} = {transpile_expr(rhs_repl)}")
                return out_lines
        rhs_norm = strip_outer_parens(rhs)
        if has_top_level_relational(rhs_norm):
            return [f"{lhs_out} = merge(1.0_dp, 0.0_dp, {transpile_expr(rhs)})"]
        return [f"{lhs_out} = {transpile_expr(rhs)}"]
    read_calls_stmt = find_named_call_spans(s, "read")
    if len(read_calls_stmt) == 1:
        start, end, args_raw = read_calls_stmt[0]
        args = [a.strip() for a in split_top_level(args_raw, ",") if a.strip()]
        if args:
            call_args = [transpile_expr(args[0]), "read_tmp"]
            for extra in args[1:]:
                eq = find_top_level_assign(extra)
                if eq != -1 and extra[:eq].strip().lower() == "col":
                    call_args.append("icol=" + transpile_expr(extra[eq + 1 :].strip()))
                else:
                    call_args.append("icol=" + transpile_expr(extra))
            expr_repl = (s[:start] + "read_tmp" + s[end + 1 :]).strip()
            out_lines = [
                "block",
                "real(kind=dp), allocatable :: read_tmp(:)",
                f"call read_vec({', '.join(call_args)})",
            ]
            if expr_repl == "read_tmp":
                out_lines.append("print *, read_tmp")
            else:
                out_lines.append(f"print *, {transpile_expr(expr_repl)}")
            out_lines.append("end block")
            return out_lines
    return [f"print *, {transpile_expr(s)}"]


def parse_proc_header(line):
    s = line.strip()
    m = re.match(r"(function|subroutine)\s+([A-Za-z_][A-Za-z0-9_]*)\s*\((.*)\)\s*$", s, re.IGNORECASE)
    if not m:
        return None
    kind = m.group(1).lower()
    name = m.group(2)
    args_raw = m.group(3).strip()
    args = []
    defaults = {}
    if args_raw:
        for tok in split_top_level(args_raw, ","):
            t = tok.strip()
            if not t:
                continue
            eq = find_top_level_assign(t)
            if eq != -1:
                a = t[:eq].strip()
                d = t[eq + 1 :].strip()
                args.append(a)
                defaults[a.lower()] = d
            else:
                args.append(t)
    return {"kind": kind, "name": name, "args": args, "defaults": defaults}


def collect_user_procedures(lines):
    main_lines = []
    procs = []
    i = 0
    n = len(lines)
    while i < n:
        raw = strip_prompt(lines[i].rstrip("\n"))
        code, _comment = split_comment(raw)
        s = code.strip()
        hdr = parse_proc_header(s)
        if hdr is None:
            main_lines.append(lines[i])
            i += 1
            continue
        body = []
        i += 1
        while i < n:
            raw_i = strip_prompt(lines[i].rstrip("\n"))
            code_i, _ = split_comment(raw_i)
            si = code_i.strip().lower()
            is_end = False
            if hdr["kind"] == "function":
                is_end = si == "end function" or si == "endfunction" or si.startswith("end function ")
            else:
                is_end = si == "end subroutine" or si == "endsubroutine" or si.startswith("end subroutine ")
            if is_end:
                break
            body.append(lines[i])
            i += 1
        # skip matching END line if present
        if i < n:
            i += 1
        procs.append({"header": hdr, "body_lines": body})
    return main_lines, procs


def parse_call_actual(tok):
    t = tok.strip()
    eq = find_top_level_assign(t)
    if eq == -1:
        return None, t
    lhs = t[:eq].strip()
    rhs = t[eq + 1 :].strip()
    if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", lhs):
        return lhs, rhs
    return None, t


def normalize_proc_calls_in_expr(expr):
    if not USER_PROCS:
        return expr
    out = []
    i = 0
    while i < len(expr):
        m = re.search(r"\b([A-Za-z_][A-Za-z0-9_]*)\s*\(", expr[i:])
        if not m:
            out.append(expr[i:])
            break
        start = i + m.start()
        name = m.group(1)
        key = name.lower()
        lpar = i + m.end() - 1
        out.append(expr[i:start])
        if key not in USER_PROCS:
            out.append(expr[start : lpar + 1])
            i = lpar + 1
            continue
        # match right paren
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
        raw_args = expr[lpar + 1 : j]
        meta = USER_PROCS[key]
        formal = meta["args"]
        defaults = meta["defaults"]
        provided = {}
        next_pos = 0
        saw_named = False
        bad = False
        for tok in [a.strip() for a in split_top_level(raw_args, ",") if a.strip()]:
            nm, rhs = parse_call_actual(tok)
            if nm is not None:
                saw_named = True
                k = nm.lower()
                if k not in [a.lower() for a in formal] or k in provided:
                    bad = True
                    break
                provided[k] = rhs
            else:
                if saw_named:
                    bad = True
                    break
                while next_pos < len(formal) and formal[next_pos].lower() in provided:
                    next_pos += 1
                if next_pos >= len(formal):
                    bad = True
                    break
                provided[formal[next_pos].lower()] = tok
                next_pos += 1
        if bad:
            out.append(expr[start : j + 1])
            i = j + 1
            continue
        full = []
        missing = False
        for a in formal:
            k = a.lower()
            if k in provided:
                full.append(to_dp_if_int_literal(provided[k]))
            elif k in defaults:
                full.append(to_dp_if_int_literal(defaults[k]))
            else:
                missing = True
                break
        if missing:
            out.append(expr[start : j + 1])
            i = j + 1
            continue
        out.append(f"{name}(" + ", ".join(full) + ")")
        i = j + 1
    return "".join(out)


def render_fortran(lines, ranks, loop_vars, rep_vars, int_vars, for_array_vars, proc_lines=None):
    proc_lines = proc_lines or []
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
    for line in lines + proc_lines:
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
    if any("kind=dp" in line.lower() for line in proc_lines):
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
        if low == "do" or low.startswith("do ") or low.startswith("if "):
            indent += 1
    if proc_lines:
        out.append("contains")
        for pl in proc_lines:
            if pl == "":
                out.append("")
            else:
                out.append("  " + pl)
    out.append("end program session")
    return "\n".join(out)


def main():
    ap = argparse.ArgumentParser(description="Transpile interpreter .fi commands to Fortran")
    ap.add_argument("input", help="Input .fi file")
    ap.add_argument("-o", "--output", help="Output .f90 file (default: stdout)")
    ap.add_argument("--noplot", action="store_true", help="Comment out plot calls in generated Fortran")
    args = ap.parse_args()

    path = Path(args.input)
    if not path.exists():
        print(f"Error: file not found: {args.input}")
        return 1
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()

    main_lines, proc_defs = collect_user_procedures(lines)
    global USER_PROCS
    USER_PROCS = {}
    for p in proc_defs:
        h = p["header"]
        USER_PROCS[h["name"].lower()] = {
            "kind": h["kind"],
            "name": h["name"],
            "args": h["args"],
            "defaults": {k.lower(): v for k, v in h["defaults"].items()},
        }

    ranks, loop_vars, int_vars, const_params = infer_from_lines(main_lines)
    global INT_VARS
    INT_VARS = set(int_vars)
    global CONST_PARAMS
    CONST_PARAMS = const_params
    global NO_PLOT
    NO_PLOT = bool(args.noplot)
    transpiled, rep_vars, for_array_vars = transpile_lines(main_lines)
    if not CONST_PARAMS and const_params:
        CONST_PARAMS = const_params
    ranks_for_proc = dict(ranks)
    ranks_for_proc["_transpiled_main_lines"] = transpiled
    ranks_for_proc["_int_vars"] = set(int_vars)
    proc_lines = []
    for p in proc_defs:
        if proc_lines:
            proc_lines.append("")
        proc_lines.extend(transpile_procedure(p, ranks_for_proc))
    if NO_PLOT:
        transpiled = comment_out_plot_calls(transpiled)
        proc_lines = comment_out_plot_calls(proc_lines)
    rendered = render_fortran(transpiled, ranks, loop_vars, rep_vars, int_vars, for_array_vars, proc_lines=proc_lines)

    if args.output:
        Path(args.output).write_text(rendered + "\n", encoding="utf-8")
    else:
        print(rendered)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
