"""Summarise paired benchmark runs; uses only Python's standard library."""

import csv
import math
from pathlib import Path
from statistics import mean

root = Path(__file__).resolve().parent


def read(name):
    with (root / name).open() as stream:
        return list(csv.DictReader(stream))


def write(name, rows):
    with (root / name).open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)


runs = read("runs.csv")
comparison = []
for scenario in ("sparse", "dense"):
    for centred in ("FALSE", "TRUE"):
        row = {"scenario": scenario, "centred": centred}
        groups = {}
        for variant in ("main", "affine"):
            group = [
                run for run in runs
                if (run["scenario"], run["centred"], run["variant"])
                == (scenario, centred, variant)
            ]
            assert len(group) == 2
            groups[variant] = group
            for metric in (
                "wall_seconds", "sampling_seconds", "median_ess_per_second",
                "hyper_ess_per_second", "median_ess_bulk", "median_hyper_ess_bulk",
            ):
                row[f"{variant}_{metric}"] = mean(float(x[metric]) for x in group)
            row[f"{variant}_max_rhat"] = max(float(x["max_rhat"]) for x in group)
            row[f"{variant}_min_ebfmi"] = min(float(x["min_ebfmi"]) for x in group)
            for metric in ("divergences", "treedepth_hits"):
                row[f"{variant}_{metric}"] = sum(int(x[metric]) for x in group)
        for metric in (
            "wall_seconds", "sampling_seconds", "median_ess_per_second",
            "hyper_ess_per_second",
        ):
            row[f"affine_over_main_{metric}"] = row[f"affine_{metric}"] / row[
                f"main_{metric}"
            ]
        comparison.append(row)
write("comparison.csv", comparison)

parameters = read("parameters.csv")
posterior_comparison = []
for scenario in ("sparse", "dense"):
    for centred in ("FALSE", "TRUE"):
        variables = sorted({p["variable"] for p in parameters})
        for variable in variables:
            row = {"scenario": scenario, "centred": centred, "variable": variable}
            combined_variance = 0
            for variant in ("main", "affine"):
                group = [p for p in parameters if (
                    p["scenario"], p["centred"], p["variable"], p["variant"]
                ) == (scenario, centred, variable, variant)]
                assert len(group) == 2
                row[f"{variant}_mean"] = mean(float(p["mean"]) for p in group)
                combined_variance += sum(float(p["mcse_mean"]) ** 2 for p in group) / 4
            row["difference"] = row["affine_mean"] - row["main_mean"]
            row["combined_mcse"] = math.sqrt(combined_variance)
            row["difference_in_mcse"] = row["difference"] / row["combined_mcse"]
            posterior_comparison.append(row)
write("posterior-comparison.csv", posterior_comparison)

for row in comparison:
    print(row)
print("Maximum absolute posterior mean difference / combined MCSE:", max(
    abs(row["difference_in_mcse"]) for row in posterior_comparison
))

lines = [
    "# Stein–Fojo affine parameterisation benchmark",
    "",
    "Run on 2026-11-06 against main commit "
    "`118884ebec28e4b0e9a84e4eecd2ea3179f3b907`. The affine version is the "
    "uncommitted working tree on `440-affine-trafo-syntax`.",
    "",
    "The focused Stein–Fojo and Parameter tests passed: 165 checks, no failures "
    "or warnings. Three existing full recovery tests were skipped.",
    "",
    "## Design",
    "",
    "One simulated longitudinal-only dataset with 60 subjects, one study and "
    "one arm, observed at either five (sparse) or eight (dense) time points. "
    "The two implementations receive identical Stan data and matching "
    "natural-scale initial values. Each fit uses four parallel chains, "
    "1,000 warmup and 1,000 retained draws per chain, adapt_delta=0.9 and "
    "max_treedepth=12. Two seeds are run, reversing implementation order for "
    "the second seed. Fits run sequentially; compilation finishes before timing.",
    "",
    "Wall time includes warmup, sampling and sampler overhead, excluding "
    "compilation and R-side model/data construction. Sampling ESS/s divides "
    "the bulk ESS of the combined four chains by the slowest chain's sampling "
    "time. Only shared natural-scale quantities are compared: seven population "
    "parameters and 180 subject effects. Hyper ESS/s is the median over the "
    "seven population parameters. Values below are averages over two seeds; "
    "ratios are ratios of those averages.",
    "",
    "## Results",
    "",
    "| Data | Parameterisation | Main wall (s) | Affine wall (s) | Wall change "
    "| Median ESS/s change | Hyper ESS/s change |",
    "| --- | --- | ---: | ---: | ---: | ---: | ---: |",
]
for row in comparison:
    label = "centred" if row["centred"] == "TRUE" else "non-centred"
    lines.append(
        f"| {row['scenario']} | {label} | {row['main_wall_seconds']:.2f} "
        f"| {row['affine_wall_seconds']:.2f} "
        f"| {100 * (row['affine_over_main_wall_seconds'] - 1):+.1f}% "
        f"| {100 * (row['affine_over_main_median_ess_per_second'] - 1):+.1f}% "
        f"| {100 * (row['affine_over_main_hyper_ess_per_second'] - 1):+.1f}% |"
    )
lines += [
    "",
    "Positive wall changes mean slower execution; positive ESS/s changes mean "
    "higher sampling efficiency. ESS/s excludes warmup, so it can move in a "
    "different direction from total wall time.",
    "",
    "| Data | Parameterisation | Main max R-hat | Affine max R-hat "
    "| Main min E-BFMI | Affine min E-BFMI |",
    "| --- | --- | ---: | ---: | ---: | ---: |",
]
for row in comparison:
    label = "centred" if row["centred"] == "TRUE" else "non-centred"
    lines.append(
        f"| {row['scenario']} | {label} | {row['main_max_rhat']:.4f} "
        f"| {row['affine_max_rhat']:.4f} | {row['main_min_ebfmi']:.3f} "
        f"| {row['affine_min_ebfmi']:.3f} |"
    )
divergences = sum(int(row["divergences"]) for row in runs)
depth_hits = sum(int(row["treedepth_hits"]) for row in runs)
max_difference = max(abs(row["difference_in_mcse"]) for row in posterior_comparison)
outliers = sum(abs(row["difference_in_mcse"]) > 3 for row in posterior_comparison)
lines += [
    "",
    f"Across all 16 fits: {divergences} post-warmup divergences and "
    f"{depth_hits} maximum-treedepth hits. R-hat and E-BFMI extrema above span "
    "both seeds and all shared parameters/chains.",
    "",
    "Posterior means are averaged over the two independent runs per version. "
    "Their differences are divided by the combined Monte Carlo standard "
    "error, propagating each run's MCSE. The maximum absolute difference is "
    f"{max_difference:.2f} combined MCSE, with {outliers} of "
    f"{len(posterior_comparison)} comparisons exceeding 3 MCSE. "
    "This is a diagnostic comparison, not a formal equivalence test.",
    "",
    "## Limits",
    "",
    "Two repetitions on one machine support an indicative comparison, not a "
    "precise speedup claim. R-hat above 1.01 limits confidence in efficiency "
    "and posterior comparisons for the affected runs. This benchmark does "
    "not cover survival associations, covariates, multiple arms/studies or "
    "other longitudinal models. An exploratory three-observation case had "
    "poor energy diagnostics on both versions; it was excluded before the "
    "final five/eight-observation benchmark.",
    "",
    "Compilation timings in compilation.csv are single measurements, "
    "reported separately from sampling. session-info.txt and provenance.txt "
    "record the software environment. No executables or posterior draws are "
    "stored in the repository.",
    "",
    "## Reproduction",
    "",
    "Run from the repository root with the affine changes present:",
    "",
    "```sh",
    "mkdir -p /tmp/jmpost-affine-main",
    "git archive 118884ebec28e4b0e9a84e4eecd2ea3179f3b907 | tar -x -C /tmp/jmpost-affine-main",
    "Rscript design/benchmarks/affine-stein-fojo/benchmark.R prepare /tmp/jmpost-affine-main main /tmp/jmpost-affine-benchmark",
    'Rscript design/benchmarks/affine-stein-fojo/benchmark.R prepare "$PWD" affine /tmp/jmpost-affine-benchmark',
    'Rscript design/benchmarks/affine-stein-fojo/benchmark.R run /tmp/jmpost-affine-benchmark "$PWD/design/benchmarks/affine-stein-fojo"',
    "python3 design/benchmarks/affine-stein-fojo/summarise.py",
    "```",
    "",
]
(root / "README.md").write_text("\n".join(lines))
