#!/usr/bin/env python3
"""Batch runner for xp2f.py over explicit files and glob patterns."""

from __future__ import annotations

import argparse
import glob
import re
import shlex
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import List


@dataclass
class CaseResult:
    source: str
    ok: bool
    rc: int
    status: str
    stage_times: dict[str, float] | None = None
    fail_reason: str | None = None
    outcome: str | None = None
    python_loc: int = 0
    fortran_loc: int = 0
    fortran_source: str = ""


def _has_glob_meta(s: str) -> bool:
    return any(ch in s for ch in "*?[]")


def _is_generated_typed_file(p: Path) -> bool:
    stem = p.stem.lower()
    if stem.endswith("_typed"):
        return True
    if "_typed_typed" in stem:
        return True
    return False


_STAGE_RE = re.compile(
    r"^\s*(python run|transpile|compile|fortran run|total)\s+([0-9]+(?:\.[0-9]+)?)\b",
    flags=re.IGNORECASE,
)
_TRANSPILE_FAIL_RE = re.compile(r"^\s*Transpile:\s*FAIL\s*\((.*)\)\s*$", flags=re.IGNORECASE)
_UNSUPPORTED_CALL_RE = re.compile(r"^unsupported call:\s*([A-Za-z_][\w\.]*)\s*\(")
_WROTE_F90_RE = re.compile(r"^\s*wrote\s+(.+?\.f90)\s*$", flags=re.IGNORECASE | re.MULTILINE)


def _parse_stage_times(text: str) -> dict[str, float]:
    out: dict[str, float] = {}
    for ln in text.splitlines():
        m = _STAGE_RE.match(ln)
        if not m:
            continue
        stage = m.group(1).strip().lower()
        try:
            out[stage] = float(m.group(2))
        except ValueError:
            continue
    return out


def _extract_transpile_fail_reason(stdout: str, stderr: str) -> str | None:
    for txt in (stdout or "", stderr or ""):
        for ln in txt.splitlines():
            m = _TRANSPILE_FAIL_RE.match(ln)
            if m:
                return m.group(1).strip()
    return None


def _classify_outcome(ok: bool, stdout: str, stderr: str) -> str:
    txt = f"{stdout or ''}\n{stderr or ''}"
    if ok:
        return "full_pass"
    if re.search(r"^\s*Transpile:\s*FAIL\b", txt, flags=re.IGNORECASE | re.MULTILINE):
        return "transpile_fail"
    if re.search(r"^\s*Build:\s*FAIL\b", txt, flags=re.IGNORECASE | re.MULTILINE):
        return "compile_fail"
    if re.search(r"^\s*Run:\s*FAIL\b", txt, flags=re.IGNORECASE | re.MULTILINE):
        return "run_fail"
    return "other_fail"


def _extract_written_fortran(stdout: str) -> str:
    m = _WROTE_F90_RE.search(stdout or "")
    if not m:
        return ""
    return m.group(1).strip()


def _count_file_lines(path: str) -> int:
    if not path:
        return 0
    p = Path(path)
    if not p.exists() or not p.is_file():
        return 0
    try:
        with p.open("r", encoding="utf-8", errors="ignore") as f:
            return sum(1 for _ in f)
    except Exception:
        return 0


def _normalize_blocker(reason: str) -> str:
    r = reason.strip()
    if r.startswith("SciPy is currently unsupported"):
        return "scipy.unsupported"
    if r.startswith("unsupported expression call:"):
        return "unsupported.expression.call"
    if r.startswith("unsupported attribute expr:"):
        return "unsupported.attribute.expr"
    if r.startswith("unsupported assign:"):
        return "unsupported.assign"
    if r.startswith("unsupported call:"):
        m = _UNSUPPORTED_CALL_RE.match(r)
        if m:
            return f"unsupported.call:{m.group(1)}"
        return "unsupported.call:other"
    if "only for .. in range" in r:
        return "loop.only_range"
    if "changes type within an if/elif/else block" in r:
        return "type.merge.after.branch"
    if "tuple assignment targets must be names or starred ignores" in r:
        return "tuple.assign.targets"
    if "ListComp currently supports only single-generator form" in r:
        return "listcomp.single.generator.only"
    return f"other:{r}"


def _print_blocker_report(results: List[CaseResult], top_n: int = 20) -> None:
    blocker_to_files: dict[str, set[str]] = {}
    call_to_files: dict[str, set[str]] = {}
    for r in results:
        if r.ok or not r.fail_reason:
            continue
        key = _normalize_blocker(r.fail_reason)
        blocker_to_files.setdefault(key, set()).add(r.source)
        if key.startswith("unsupported.call:"):
            cn = key.split(":", 1)[1]
            call_to_files.setdefault(cn, set()).add(r.source)

    if not blocker_to_files:
        print("")
        print("Blockers:")
        print("(no transpile blocker details captured)")
        return

    total_files = len(set().union(*blocker_to_files.values()))
    print("")
    print("Blockers:")
    print(f"Files with transpile blockers: {total_files}")

    calls_sorted = sorted(call_to_files.items(), key=lambda kv: (-len(kv[1]), kv[0]))[:top_n]
    print(f"Top {top_n} unsupported calls by unique-file impact:")
    if not calls_sorted:
        print("(none)")
    else:
        for i, (call_name, files) in enumerate(calls_sorted, 1):
            print(f"{i:2d}. {call_name:<30} files={len(files)}")

    # Greedy order by unique-file gain.
    unresolved = set().union(*blocker_to_files.values())
    items = sorted(blocker_to_files.items(), key=lambda kv: (-len(kv[1]), kv[0]))
    print(f"Greedy implementation order (top {top_n}):")
    covered = 0
    step = 0
    while unresolved and items and step < top_n:
        best_key = None
        best_gain = -1
        best_cover: set[str] = set()
        for key, files in items:
            gain_set = files & unresolved
            gain = len(gain_set)
            if gain > best_gain:
                best_gain = gain
                best_key = key
                best_cover = gain_set
        if best_key is None or best_gain <= 0:
            break
        step += 1
        covered += best_gain
        print(f"{step:2d}. {best_key:<34} gain={best_gain:4d} covered_so_far={covered:4d}")
        unresolved -= best_cover
        items = [(k, v) for (k, v) in items if k != best_key]
    print(f"Remaining unresolved files after greedy top-{top_n}: {len(unresolved)}")


def _expand_inputs(items: List[str], *, exclude_generated_typed: bool = False) -> List[Path]:
    out: List[Path] = []
    seen = set()
    for it in items:
        matches: List[str]
        if _has_glob_meta(it):
            matches = glob.glob(it, recursive=True)
        else:
            matches = [it]
        for m in matches:
            p = Path(m)
            if p.is_dir():
                for q in sorted(p.rglob("*.py")):
                    k = str(q.resolve()).lower()
                    if k not in seen:
                        seen.add(k)
                        out.append(q)
                continue
            if p.suffix.lower() != ".py":
                continue
            if p.exists():
                if exclude_generated_typed and _is_generated_typed_file(p):
                    continue
                k = str(p.resolve()).lower()
                if k not in seen:
                    seen.add(k)
                    out.append(p)
    return sorted(out, key=lambda p: str(p).lower())


def _path_key(p: Path) -> str:
    try:
        return str(p.resolve()).lower()
    except Exception:
        return str(p).lower()


_PROCESSED_LINE_RE = re.compile(r"^\s*\[\d+/\d+\]\s+(.+?)\s*$")


def _processed_from_log(log_path: Path) -> set[str]:
    out: set[str] = set()
    try:
        text = log_path.read_text(encoding="utf-8", errors="ignore")
    except Exception:
        return out
    for ln in text.splitlines():
        m = _PROCESSED_LINE_RE.match(ln)
        if not m:
            continue
        raw = m.group(1).strip()
        if not raw:
            continue
        out.add(_path_key(Path(raw)))
    return out


def _resolve_resume_anchor(py_files: List[Path], anchor_raw: str) -> tuple[Path | None, str | None]:
    """Resolve resume anchor against matched files.

    Resolution order:
    1) exact path-key match;
    2) unique basename match when only filename is supplied.
    """
    anchor_path = Path(anchor_raw)
    keys = {_path_key(p): p for p in py_files}

    exact = keys.get(_path_key(anchor_path))
    if exact is not None:
        return exact, None

    # If user supplied only a filename, allow implicit directory resolution.
    if (not anchor_path.is_absolute()) and anchor_path.parent in {Path("."), Path("")}:
        name = anchor_path.name.lower()
        cands = [p for p in py_files if p.name.lower() == name]
        if len(cands) == 1:
            return cands[0], None
        if len(cands) > 1:
            shown = ", ".join(str(p) for p in cands[:5])
            more = "" if len(cands) <= 5 else f", ... (+{len(cands) - 5} more)"
            return None, f"ambiguous resume anchor '{anchor_raw}' matches {len(cands)} files: {shown}{more}"

    return None, f"resume anchor not found in matched files: {anchor_raw}"


def main() -> int:
    t0 = time.perf_counter()
    ap = argparse.ArgumentParser(
        description="Run xp2f.py on multiple Python files/globs."
    )
    ap.add_argument("inputs", nargs="+", help="Python files, directories, and/or glob patterns.")
    ap.add_argument(
        "--helpers",
        nargs="*",
        default=[],
        help="Zero or more helper .f90 files passed to xp2f.py.",
    )
    ap.add_argument(
        "--compiler",
        default="gfortran -O3 -march=native -flto -Wfatal-errors",
        help='Compiler command forwarded to xp2f.py --compiler.',
    )
    ap.add_argument("--flat", action="store_true", help="Forward --flat to xp2f.py.")
    ap.add_argument("--type", action="store_true", help="Forward --type to xp2f.py.")
    ap.add_argument("--comment", action="store_true", help="Forward --comment to xp2f.py.")
    ap.add_argument("--run-diff", action="store_true", help="Forward --run-diff to xp2f.py.")
    ap.add_argument("--run-both", action="store_true", help="Forward --run-both to xp2f.py.")
    ap.add_argument("--autofix", action="store_true", help="Forward --autofix to xp2f.py.")
    ap.add_argument("--no-run", action="store_true", help="Transpile (and compile unless --no-compile), but do not run Fortran executables.")
    ap.add_argument("--no-compile", action="store_true", help="Transpile only (implies --no-run).")
    ap.add_argument("--time-both", action="store_true", help="Forward --time-both to xp2f.py.")
    ap.add_argument(
        "--timing-breakdown",
        action="store_true",
        help="Request xp2f stage timings and report aggregated breakdown in summary.",
    )
    ap.add_argument("--pretty", action="store_true", help="Forward --pretty to xp2f.py.")
    ap.add_argument("--tee-orig", action="store_true", help="Forward --tee-orig to xp2f.py.")
    ap.add_argument("--strict", action="store_true", help="Run xp2f.py --strict for each file.")
    ap.add_argument("--strict-fix", action="store_true", help="Run xp2f.py --strict-fix for each file.")
    ap.add_argument(
        "--strict-fix-overloads",
        action="store_true",
        help="Forward --strict-fix-overloads (requires --strict-fix).",
    )
    ap.add_argument(
        "--strict-fix-inplace",
        action="store_true",
        help="Forward --strict-fix-inplace (requires --strict-fix).",
    )
    ap.add_argument(
        "--out-python-dir",
        help="Output directory root for strict-fixed Python files (requires --strict-fix, non-inplace only).",
    )
    ap.add_argument("--maxfail", type=int, default=0, help="Stop after this many failures (0 = no limit).")
    ap.add_argument("--limit", type=int, default=0, help="Process at most this many matched files (0 = no limit).")
    ap.add_argument("--resume", help="Resume from prior xp2f_batch log by skipping already-processed files.")
    ap.add_argument("--resume-after", help="Start after this Python source path in matched ordering.")
    ap.add_argument("--resume-with", help="Start with this Python source path in matched ordering.")
    ap.add_argument("--verbose", action="store_true", help="Print full xp2f output for PASS cases too.")
    ap.add_argument(
        "--terse",
        action="store_true",
        help="Show only failing cases (plus final totals).",
    )
    ap.add_argument(
        "--blockers",
        action="store_true",
        help="Print blocker summary (top unsupported calls + greedy implementation order) at end.",
    )
    args = ap.parse_args()

    if args.strict and args.strict_fix:
        print("Invalid options: --strict and --strict-fix are mutually exclusive.")
        return 1
    if args.no_compile:
        args.no_run = True
    if args.run_both and args.no_run:
        print("Invalid options: --run-both cannot be combined with --no-run.")
        return 1
    if args.run_diff and args.no_run:
        print("Invalid options: --run-diff cannot be combined with --no-run.")
        return 1
    if args.time_both and args.no_run:
        print("Invalid options: --time-both cannot be combined with --no-run.")
        return 1
    if args.strict_fix_inplace and not args.strict_fix:
        print("Invalid options: --strict-fix-inplace requires --strict-fix.")
        return 1
    if args.strict_fix_overloads and not args.strict_fix:
        print("Invalid options: --strict-fix-overloads requires --strict-fix.")
        return 1
    if args.out_python_dir and not args.strict_fix:
        print("Invalid options: --out-python-dir requires --strict-fix.")
        return 1
    if args.out_python_dir and args.strict_fix_inplace:
        print("Invalid options: --out-python-dir cannot be used with --strict-fix-inplace.")
        return 1
    if args.resume_after and args.resume_with:
        print("Invalid options: --resume-after and --resume-with are mutually exclusive.")
        return 1

    py_files = _expand_inputs(args.inputs, exclude_generated_typed=args.type)
    if not py_files:
        print("No Python files matched the provided inputs.")
        return 1

    if args.resume:
        resume_path = Path(args.resume)
        if not resume_path.exists() or not resume_path.is_file():
            print(f"Invalid options: --resume file not found: {resume_path}")
            return 1
        done_keys = _processed_from_log(resume_path)
        before = len(py_files)
        py_files = [p for p in py_files if _path_key(p) not in done_keys]
        skipped = before - len(py_files)
        print(f"Resume: skipped {skipped} already-processed file(s) from {resume_path}.")
        if not py_files:
            print("Resume: no remaining files to process.")
            return 0

    if args.resume_after or args.resume_with:
        anchor_raw = args.resume_with if args.resume_with else args.resume_after
        anchor_resolved, err = _resolve_resume_anchor(py_files, anchor_raw)
        if anchor_resolved is None:
            print(f"Invalid options: {err}")
            return 1
        keys = [_path_key(p) for p in py_files]
        anchor_key = _path_key(anchor_resolved)
        idx = keys.index(anchor_key)
        if args.resume_after:
            py_files = py_files[idx + 1 :]
            print(f"Resume-after: starting after {anchor_resolved}. Remaining: {len(py_files)} file(s).")
        else:
            py_files = py_files[idx:]
            print(f"Resume-with: starting with {anchor_resolved}. Remaining: {len(py_files)} file(s).")
        if not py_files:
            print("Resume: no remaining files to process.")
            return 0

    if args.limit < 0:
        print("Invalid options: --limit must be >= 0.")
        return 1
    if args.limit > 0:
        py_files = py_files[: args.limit]

    xp2f_path = Path(__file__).with_name("xp2f.py")
    if not xp2f_path.exists():
        print(f"Missing script: {xp2f_path}")
        return 1

    results: List[CaseResult] = []
    failures = 0
    total = len(py_files)
    strict_files_created = 0
    timing_sum = {"python run": 0.0, "transpile": 0.0, "compile": 0.0, "fortran run": 0.0, "total": 0.0}
    timing_count = {"python run": 0, "transpile": 0, "compile": 0, "fortran run": 0, "total": 0}

    out_python_root = Path(args.out_python_dir) if args.out_python_dir else None
    if out_python_root is not None:
        out_python_root.mkdir(parents=True, exist_ok=True)
    cwd_resolved = Path.cwd().resolve()

    for i, pyf in enumerate(py_files, start=1):
        rel = str(pyf)
        python_loc = _count_file_lines(rel)
        cmd = [sys.executable, str(xp2f_path), rel, *args.helpers]
        if args.strict:
            cmd.append("--strict")
        elif args.strict_fix:
            cmd.append("--strict-fix")
            if args.strict_fix_overloads:
                cmd.append("--strict-fix-overloads")
            if args.strict_fix_inplace:
                cmd.append("--strict-fix-inplace")
            elif out_python_root is not None:
                try:
                    rel_path = pyf.resolve().relative_to(cwd_resolved)
                except Exception:
                    rel_path = Path(pyf.name)
                out_py = out_python_root / rel_path.parent / f"{pyf.stem}_strict{pyf.suffix or '.py'}"
                out_py.parent.mkdir(parents=True, exist_ok=True)
                cmd.extend(["--out-python", str(out_py)])
        else:
            if args.no_compile:
                # Transpile only.
                pass
            elif args.no_run:
                # Compile but do not execute transpiled Fortran.
                cmd.extend(["--compile", "--compiler", args.compiler])
            elif args.run_both:
                cmd.extend(["--run-both", "--compiler", args.compiler])
            else:
                cmd.extend(["--run", "--compiler", args.compiler])
        if args.flat:
            cmd.append("--flat")
        if args.type:
            cmd.append("--type")
        if args.comment:
            cmd.append("--comment")
        if args.run_diff and (not args.strict) and (not args.strict_fix):
            cmd.append("--run-diff")
        if args.autofix and (not args.strict) and (not args.strict_fix):
            cmd.append("--autofix")
        if (not args.strict) and (not args.strict_fix):
            want_timing = args.time_both or args.timing_breakdown
            if want_timing:
                if args.run_both or args.run_diff:
                    cmd.append("--time-both")
                else:
                    cmd.append("--time")
        if args.pretty:
            cmd.append("--pretty")
        if args.tee_orig:
            cmd.append("--tee-orig")

        if not args.terse:
            print(f"[{i}/{total}] {rel}")
        cp = subprocess.run(cmd, text=True, capture_output=True, encoding="utf-8", errors="ignore")
        ok = cp.returncode == 0
        outcome = _classify_outcome(ok, cp.stdout or "", cp.stderr or "")
        fortran_source = _extract_written_fortran(cp.stdout or "")
        fortran_loc = _count_file_lines(fortran_source)

        stage_times = _parse_stage_times(cp.stdout or "")
        for k, v in stage_times.items():
            if k in timing_sum:
                timing_sum[k] += v
                timing_count[k] += 1

        if ok:
            if args.strict:
                status = "STRICT_PASS"
            elif args.strict_fix:
                created = ("Strict-fix: wrote " in (cp.stdout or "")) and ("no output file created" not in (cp.stdout or ""))
                if created:
                    strict_files_created += 1
                    status = "STRICT_FIXED"
                else:
                    status = "STRICT_PASS"
            else:
                status = "PASS"
            show_pass_output = args.verbose or (args.time_both and not args.strict and not args.strict_fix) or (args.run_diff and not args.strict and not args.strict_fix)
            if (not args.terse) and show_pass_output and cp.stdout.strip():
                print(cp.stdout.rstrip())
            if (not args.terse) and show_pass_output and cp.stderr.strip():
                print(cp.stderr.rstrip())
        else:
            if args.strict:
                status = "STRICT_FAIL"
            elif args.strict_fix:
                status = "STRICT_FIX_FAIL"
            else:
                status = "FAIL"
            failures += 1
            if args.terse:
                print(f"[{i}/{total}] {rel}")
            print(f"  FAIL (exit {cp.returncode})")
            if cp.stdout.strip():
                print(cp.stdout.rstrip())
            if cp.stderr.strip():
                print(cp.stderr.rstrip())
            if args.maxfail > 0 and failures >= args.maxfail:
                results.append(
                    CaseResult(
                        source=rel,
                        ok=ok,
                        rc=cp.returncode,
                        status=status,
                        stage_times=stage_times or None,
                        fail_reason=_extract_transpile_fail_reason(cp.stdout or "", cp.stderr or ""),
                        outcome=outcome,
                        python_loc=python_loc,
                        fortran_loc=fortran_loc,
                        fortran_source=fortran_source,
                    )
                )
                print(f"Stopped at maxfail={args.maxfail}.")
                break

        results.append(
            CaseResult(
                source=rel,
                ok=ok,
                rc=cp.returncode,
                status=status,
                stage_times=stage_times or None,
                fail_reason=_extract_transpile_fail_reason(cp.stdout or "", cp.stderr or ""),
                outcome=outcome,
                python_loc=python_loc,
                fortran_loc=fortran_loc,
                fortran_source=fortran_source,
            )
        )
        if (not args.terse) and i < total:
            print("")

    print("")
    print("Summary:")
    summary_rows = [r for r in results if (not args.terse or not r.ok)]
    if args.terse and not summary_rows:
        print("(no failures)")
    src_w = max(len("source"), *(len(r.source) for r in summary_rows)) if summary_rows else len("source")
    st_w = max(len("status"), *(len(r.status) for r in summary_rows)) if summary_rows else len("status")
    out_w = max(len("outcome"), *(len(r.outcome or "") for r in summary_rows)) if summary_rows else len("outcome")
    py_w = max(len("Python_loc"), *(len(str(r.python_loc)) for r in summary_rows)) if summary_rows else len("Python_loc")
    f90loc_w = max(len("Fortran_loc"), *(len(str(r.fortran_loc)) for r in summary_rows)) if summary_rows else len("Fortran_loc")
    f90src_w = max(len("Fortran_src"), *(len(r.fortran_source or "") for r in summary_rows)) if summary_rows else len("Fortran_src")
    show_rc = bool(args.verbose)
    rc_w = max(len("rc"), *(len(str(r.rc)) for r in summary_rows)) if (summary_rows and show_rc) else len("rc")
    if show_rc:
        header = (
            f"{'source':<{src_w}}  {'status':<{st_w}}  {'outcome':<{out_w}}  {'rc':>{rc_w}}  "
            f"{'Python_loc':>{py_w}}  {'Fortran_loc':>{f90loc_w}}  {'Fortran_src':<{f90src_w}}"
        )
    else:
        header = (
            f"{'source':<{src_w}}  {'status':<{st_w}}  {'outcome':<{out_w}}  "
            f"{'Python_loc':>{py_w}}  {'Fortran_loc':>{f90loc_w}}  {'Fortran_src':<{f90src_w}}"
        )
    if summary_rows:
        print(header)
        for r in summary_rows:
            if show_rc:
                print(
                    f"{r.source:<{src_w}}  {r.status:<{st_w}}  {(r.outcome or ''):<{out_w}}  {r.rc:>{rc_w}}  "
                    f"{r.python_loc:>{py_w}}  {r.fortran_loc:>{f90loc_w}}  {(r.fortran_source or ''):<{f90src_w}}"
                )
            else:
                print(
                    f"{r.source:<{src_w}}  {r.status:<{st_w}}  {(r.outcome or ''):<{out_w}}  "
                    f"{r.python_loc:>{py_w}}  {r.fortran_loc:>{f90loc_w}}  {(r.fortran_source or ''):<{f90src_w}}"
                )
    n_pass = sum(1 for r in results if r.ok)
    n_fail = len(results) - n_pass
    print(f"Totals: {len(results)} files, {n_pass} pass, {n_fail} fail")
    out_counts = {
        "full_pass": sum(1 for r in results if r.outcome == "full_pass"),
        "transpile_fail": sum(1 for r in results if r.outcome == "transpile_fail"),
        "compile_fail": sum(1 for r in results if r.outcome == "compile_fail"),
        "run_fail": sum(1 for r in results if r.outcome == "run_fail"),
        "other_fail": sum(1 for r in results if r.outcome == "other_fail"),
    }
    print(
        "Outcomes: "
        f"full_pass={out_counts['full_pass']}  "
        f"transpile_fail={out_counts['transpile_fail']}  "
        f"compile_fail={out_counts['compile_fail']}  "
        f"run_fail={out_counts['run_fail']}  "
        f"other_fail={out_counts['other_fail']}"
    )
    if any(timing_count[k] > 0 for k in timing_count):
        print("")
        print("Timing breakdown:")
        rows = []
        for k in ("python run", "transpile", "compile", "fortran run", "total"):
            if k == "python run" and timing_count[k] == 0:
                continue
            cnt = timing_count[k]
            s = timing_sum[k]
            avg = (s / cnt) if cnt > 0 else 0.0
            rows.append((k, cnt, s, avg))
        stage_w = max(len("stage"), *(len(r[0]) for r in rows))
        files_w = max(len("files"), *(len(str(r[1])) for r in rows))
        sum_w = max(len("sum_s"), *(len(f"{r[2]:.3f}") for r in rows))
        avg_w = max(len("per_file_s"), *(len(f"{r[3]:.3f}") for r in rows))
        print(f"  {'stage':<{stage_w}}  {'files':>{files_w}}  {'sum_s':>{sum_w}}  {'per_file_s':>{avg_w}}")
        for stage, cnt, s, avg in rows:
            print(f"  {stage:<{stage_w}}  {cnt:>{files_w}}  {s:>{sum_w}.3f}  {avg:>{avg_w}.3f}")
    if args.strict_fix and not args.strict_fix_inplace:
        print(f"Strict files created: {strict_files_created}")
    if args.blockers:
        _print_blocker_report(results, top_n=20)
    elapsed = time.perf_counter() - t0
    print(f"Elapsed: {elapsed:.3f} s")
    return 0 if n_fail == 0 else 1


if __name__ == "__main__":
    raise SystemExit(main())
