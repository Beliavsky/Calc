#!/usr/bin/env python3
import argparse
import fnmatch
import json
import re
import subprocess
import sys
import time
from pathlib import Path


def run(cmd, capture=False):
    if capture:
        return subprocess.run(cmd, shell=False, capture_output=True, text=True)
    return subprocess.run(cmd, shell=False)


def save_state(path, state):
    path.write_text(json.dumps(state, indent=2), encoding="utf-8")


def build_error_entry(fi, stage, script_text="", fortran_text="", output_text=""):
    return "\n".join(
        [
            f"=== {fi.name} ({stage}) ===",
            "--- Script ---",
            script_text.rstrip(),
            "--- Fortran ---",
            fortran_text.rstrip(),
            "--- Output ---",
            output_text.rstrip(),
            "",
        ]
    )


def build_name_matcher(pattern_text, option_name):
    try:
        rx = re.compile(pattern_text)
        return lambda s: bool(rx.search(s))
    except re.error:
        # Accept glob-like patterns (e.g. *simfit*.fi) for convenience.
        if any(ch in pattern_text for ch in "*?[]"):
            return lambda s: fnmatch.fnmatch(s, pattern_text)
        raise ValueError(
            f"Invalid regex for {option_name}: {pattern_text!r}. "
            "Use a valid regex, or a glob like '*simfit*.fi'."
        )


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--limit",
        type=int,
        default=0,
        help="Limit number of .fi files to process (0 means no limit).",
    )
    parser.add_argument(
        "--include-re",
        type=str,
        default="",
        help="Include only files whose name matches this regex.",
    )
    parser.add_argument(
        "--exclude-re",
        type=str,
        default="",
        help="Exclude files whose name matches this regex.",
    )
    parser.add_argument(
        "--files",
        type=str,
        default="",
        help="Comma-separated list of .fi files to process (overrides glob).",
    )
    parser.add_argument(
        "--hide-output",
        action="store_true",
        help="Hide output of compiled Fortran programs (current behavior).",
    )
    parser.add_argument(
        "--run-script",
        action="store_true",
        help="Run each interpreter script before compiling.",
    )
    parser.add_argument(
        "--noplot",
        action="store_true",
        help="Pass --noplot to fcalc when running scripts.",
    )
    parser.add_argument(
        "--run-exe",
        action="store_true",
        help="Run the generated Fortran program.",
    )
    parser.add_argument(
        "--time",
        action="store_true",
        help="Print per-script and per-program timings at the end.",
    )
    parser.add_argument(
        "--fail-fast",
        action="store_true",
        help="Stop immediately on the first failure.",
    )
    parser.add_argument(
        "--resume",
        action="store_true",
        help="Resume from prior checkpoint state.",
    )
    parser.add_argument(
        "--restart",
        action="store_true",
        help="Reset checkpoint state before running.",
    )
    parser.add_argument(
        "--state-file",
        type=str,
        default="xpytr_all_state.json",
        help="Checkpoint file path used by --resume/--restart.",
    )
    args = parser.parse_args()

    if args.files:
        fi_files = []
        for name in args.files.replace(",", " ").split():
            fi_files.append(Path(name))
        fi_files = [p for p in fi_files if p.exists()]
    else:
        fi_files = sorted(Path(".").glob("*.fi"))
    if args.include_re:
        try:
            matcher = build_name_matcher(args.include_re, "--include-re")
        except ValueError as e:
            print(f"Error: {e}")
            return 2
        fi_files = [p for p in fi_files if matcher(p.name)]
    if args.exclude_re:
        try:
            matcher = build_name_matcher(args.exclude_re, "--exclude-re")
        except ValueError as e:
            print(f"Error: {e}")
            return 2
        fi_files = [p for p in fi_files if not matcher(p.name)]
    if args.limit and args.limit > 0:
        fi_files = fi_files[: args.limit]

    state_path = Path(args.state_file)
    if args.restart and state_path.exists():
        state_path.unlink()

    run_signature = {
        "include_re": args.include_re,
        "exclude_re": args.exclude_re,
        "files": args.files,
        "limit": args.limit,
        "hide_output": args.hide_output,
        "run_script": args.run_script,
        "run_exe": args.run_exe,
        "noplot": args.noplot,
    }
    state = {
        "signature": run_signature,
        "next_index": 0,
        "updated_at": time.time(),
    }
    start_index = 0
    if args.resume and state_path.exists():
        try:
            loaded = json.loads(state_path.read_text(encoding="utf-8"))
            if loaded.get("signature") == run_signature:
                start_index = int(loaded.get("next_index", 0))
                state = loaded
                print(f"[RESUME] Starting from index {start_index}")
            else:
                print("[RESUME] State signature mismatch; starting from beginning.")
        except Exception:
            print("[RESUME] Could not read state file; starting from beginning.")

    if start_index < 0:
        start_index = 0
    if start_index > len(fi_files):
        start_index = len(fi_files)
    fi_files = fi_files[start_index:]

    error_entries = []
    error_file = Path("xpytr_all_errors.txt")
    if error_file.exists():
        error_file.unlink()

    total = 0
    passed = 0
    failed = 0
    failed_list = []

    timings = {}

    for offset, fi in enumerate(fi_files):
        abs_index = start_index + offset
        total += 1
        print("\n=== Processing {} ===".format(fi.name))
        file_failed = False
        fi_src = fi.read_text(encoding="utf-8", errors="replace") if fi.exists() else ""
        t_transpile0 = time.perf_counter()

        tests_f90 = Path('tests.f90')
        tests_o = Path('tests.o')
        if tests_f90.exists():
            tests_f90.unlink()

        proc = run([sys.executable, 'transpile_session.py', str(fi), '--noplot', '-o', 'tests.f90'])
        t_transpile1 = time.perf_counter()
        if proc.returncode != 0:
            print("[FAIL] Transpile failed for {}".format(fi.name))
            failed += 1
            failed_list.append(f"{fi.name}(transpile)")
            file_failed = True
            out = ""
            if getattr(proc, "stdout", None):
                out += proc.stdout
            if getattr(proc, "stderr", None):
                out += proc.stderr
            error_entries.append(
                build_error_entry(fi, "transpile", script_text=fi_src, output_text=out)
            )
            state["next_index"] = abs_index
            state["updated_at"] = time.time()
            save_state(state_path, state)
            if args.fail_fast:
                print("[STOP] --fail-fast triggered at {} (transpile)".format(fi.name))
                break

        if not file_failed and not tests_f90.exists():
            print("[FAIL] Transpiler did not create tests.f90 for {}".format(fi.name))
            failed += 1
            failed_list.append(f"{fi.name}(missing_output)")
            file_failed = True
            error_entries.append(
                build_error_entry(fi, "missing_output", script_text=fi_src)
            )
            state["next_index"] = abs_index
            state["updated_at"] = time.time()
            save_state(state_path, state)
            if args.fail_fast:
                print("[STOP] --fail-fast triggered at {} (missing_output)".format(fi.name))
                break

        script_time = None
        if not file_failed and args.run_script:
            t0 = time.perf_counter()
            fcalc_cmd = ["fcalc"]
            if args.noplot:
                fcalc_cmd.append("--noplot")
            fcalc_cmd.append(str(fi))
            proc = run(fcalc_cmd, capture=args.hide_output)
            if proc.returncode != 0:
                print("[FAIL] Interpreter run failed for {}".format(fi.name))
                if proc.stdout:
                    print(proc.stdout, end="")
                if proc.stderr:
                    print(proc.stderr, end="")
                failed += 1
                failed_list.append(f"{fi.name}(interp)")
                file_failed = True
                out = (proc.stdout or "") + (proc.stderr or "")
                src = tests_f90.read_text(encoding="utf-8", errors="replace") if tests_f90.exists() else ""
                error_entries.append(
                    build_error_entry(fi, "interp", script_text=fi_src, fortran_text=src, output_text=out)
                )
                state["next_index"] = abs_index
                state["updated_at"] = time.time()
                save_state(state_path, state)
                if args.fail_fast:
                    print("[STOP] --fail-fast triggered at {} (interp)".format(fi.name))
                    break
            t1 = time.perf_counter()
            script_time = t1 - t0

        compile_time = None
        run_time = None
        if not file_failed:
            if tests_o.exists():
                tests_o.unlink()
            t2 = time.perf_counter()
            proc = run(['make', '-f', 'Makefile_tests', 'all'], capture=True)
            t3 = time.perf_counter()
            compile_time = t3 - t2
            if proc.returncode != 0:
                print("[FAIL] Compile failed for {}".format(fi.name))
                if proc.stdout:
                    print(proc.stdout, end="")
                if proc.stderr:
                    print(proc.stderr, end="")
                failed += 1
                failed_list.append(f"{fi.name}(compile)")
                src = tests_f90.read_text(encoding="utf-8", errors="replace") if tests_f90.exists() else ""
                out = (proc.stdout or "") + (proc.stderr or "")
                error_entries.append(
                    build_error_entry(fi, "compile", script_text=fi_src, fortran_text=src, output_text=out)
                )
                state["next_index"] = abs_index
                state["updated_at"] = time.time()
                save_state(state_path, state)
                if args.fail_fast:
                    print("[STOP] --fail-fast triggered at {} (compile)".format(fi.name))
                    break
            else:
                if args.run_exe:
                    exe = Path("tests.exe")
                    exe_cmd = [f".\\{exe}"] if sys.platform.startswith("win") else [str(exe)]
                    t4 = time.perf_counter()
                    run_proc = run(exe_cmd, capture=args.hide_output)
                    t5 = time.perf_counter()
                    run_time = t5 - t4
                    if run_proc.returncode != 0:
                        print("[FAIL] Run failed for {}".format(fi.name))
                        if run_proc.stdout:
                            print(run_proc.stdout, end="")
                        if run_proc.stderr:
                            print(run_proc.stderr, end="")
                        failed += 1
                        failed_list.append(f"{fi.name}(run)")
                        out = (run_proc.stdout or "") + (run_proc.stderr or "")
                        src = tests_f90.read_text(encoding="utf-8", errors="replace") if tests_f90.exists() else ""
                        error_entries.append(
                            build_error_entry(fi, "run", script_text=fi_src, fortran_text=src, output_text=out)
                        )
                        state["next_index"] = abs_index
                        state["updated_at"] = time.time()
                        save_state(state_path, state)
                        if args.fail_fast:
                            print("[STOP] --fail-fast triggered at {} (run)".format(fi.name))
                            break
                    else:
                        print("[PASS] {}".format(fi.name))
                        passed += 1
                else:
                    print("[PASS] {}".format(fi.name))
                    passed += 1
        if not file_failed:
            state["next_index"] = abs_index + 1
            state["updated_at"] = time.time()
            save_state(state_path, state)
        if args.time:
            timings[fi.name] = {
                "transpile_s": t_transpile1 - t_transpile0,
                "compile_s": compile_time,
                "script_s": script_time,
                "run_s": run_time,
            }

    print("\n=== Summary ===")
    print("Total .fi files: {}".format(total))
    print("Passed: {}".format(passed))
    print("Failed: {}".format(failed))
    if failed_list:
        print("Failed files: {}".format(" ".join(failed_list)))

    if args.time and timings:
        import pandas as pd

        cols = ["transpile", "compile"]
        if args.run_script:
            cols.append("script")
        if args.run_exe:
            cols.append("run")
        df = pd.DataFrame.from_dict(timings, orient="index")
        df = df.rename(
            columns={
                "transpile_s": "transpile",
                "compile_s": "compile",
                "script_s": "script",
                "run_s": "run",
            }
        )
        if args.run_script and args.run_exe:
            df["ratio"] = df["run"] / df["script"]
            cols.append("ratio")
        df["total"] = df[["transpile", "compile"] + (["script"] if args.run_script else []) + (["run"] if args.run_exe else [])].sum(axis=1)
        cols.append("total")
        df = df[cols]
        print("\n=== Timing ===")
        print(df.to_string())
        stats = df.agg(["median", "mean", "min", "max", "sum"])
        print("\n=== Timing Summary ===")
        print(stats.to_string())

    if error_entries:
        error_file.write_text("\n".join(error_entries).rstrip() + "\n", encoding="utf-8")
        print(f"Error report written to {error_file}")

    if total == 0:
        if args.resume and start_index > 0:
            print("No remaining .fi files to process for current checkpoint.")
        else:
            print("No .fi files found in current directory.")
        return 0
    if failed > 0:
        return 1
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
