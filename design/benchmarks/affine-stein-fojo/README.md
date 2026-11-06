# Stein–Fojo affine parameterisation benchmark

Run on 2026-11-06 against main commit `118884ebec28e4b0e9a84e4eecd2ea3179f3b907`. The affine version is the uncommitted working tree on `440-affine-trafo-syntax`.

The focused Stein–Fojo and Parameter tests passed: 165 checks, no failures or warnings. Three existing full recovery tests were skipped.

## Design

One simulated longitudinal-only dataset with 60 subjects, one study and one arm, observed at either five (sparse) or eight (dense) time points. The two implementations receive identical Stan data and matching natural-scale initial values. Each fit uses four parallel chains, 1,000 warmup and 1,000 retained draws per chain, adapt_delta=0.9 and max_treedepth=12. Two seeds are run, reversing implementation order for the second seed. Fits run sequentially; compilation finishes before timing.

Wall time includes warmup, sampling and sampler overhead, excluding compilation and R-side model/data construction. Sampling ESS/s divides the bulk ESS of the combined four chains by the slowest chain's sampling time. Only shared natural-scale quantities are compared: seven population parameters and 180 subject effects. Hyper ESS/s is the median over the seven population parameters. Values below are averages over two seeds; ratios are ratios of those averages.

## Results

| Data | Parameterisation | Main wall (s) | Affine wall (s) | Wall change | Median ESS/s change | Hyper ESS/s change |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| sparse | non-centred | 16.34 | 18.07 | +10.6% | -6.7% | -14.4% |
| sparse | centred | 5.86 | 5.95 | +1.4% | -9.9% | -2.7% |
| dense | non-centred | 26.87 | 28.40 | +5.7% | -15.8% | -5.8% |
| dense | centred | 8.72 | 8.71 | -0.1% | +3.4% | +0.6% |

Positive wall changes mean slower execution; positive ESS/s changes mean higher sampling efficiency. ESS/s excludes warmup, so it can move in a different direction from total wall time.

| Data | Parameterisation | Main max R-hat | Affine max R-hat | Main min E-BFMI | Affine min E-BFMI |
| --- | --- | ---: | ---: | ---: | ---: |
| sparse | non-centred | 1.0183 | 1.0181 | 0.466 | 0.468 |
| sparse | centred | 1.0044 | 1.0053 | 0.542 | 0.586 |
| dense | non-centred | 1.0148 | 1.0132 | 0.520 | 0.600 |
| dense | centred | 1.0047 | 1.0060 | 0.686 | 0.734 |

Across all 16 fits: 0 post-warmup divergences and 0 maximum-treedepth hits. R-hat and E-BFMI extrema above span both seeds and all shared parameters/chains.

Posterior means are averaged over the two independent runs per version. Their differences are divided by the combined Monte Carlo standard error, propagating each run's MCSE. The maximum absolute difference is 3.19 combined MCSE, with 3 of 748 comparisons exceeding 3 MCSE. This is a diagnostic comparison, not a formal equivalence test.

## Limits

Two repetitions on one machine support an indicative comparison, not a precise speedup claim. R-hat above 1.01 limits confidence in efficiency and posterior comparisons for the affected runs. This benchmark does not cover survival associations, covariates, multiple arms/studies or other longitudinal models. An exploratory three-observation case had poor energy diagnostics on both versions; it was excluded before the final five/eight-observation benchmark.

Compilation timings in compilation.csv are single measurements, reported separately from sampling. session-info.txt and provenance.txt record the software environment. No executables or posterior draws are stored in the repository.

## Reproduction

Run from the repository root with the affine changes present:

```sh
mkdir -p /tmp/jmpost-affine-main
git archive 118884ebec28e4b0e9a84e4eecd2ea3179f3b907 | tar -x -C /tmp/jmpost-affine-main
Rscript design/benchmarks/affine-stein-fojo/benchmark.R prepare /tmp/jmpost-affine-main main /tmp/jmpost-affine-benchmark
Rscript design/benchmarks/affine-stein-fojo/benchmark.R prepare "$PWD" affine /tmp/jmpost-affine-benchmark
Rscript design/benchmarks/affine-stein-fojo/benchmark.R run /tmp/jmpost-affine-benchmark "$PWD/design/benchmarks/affine-stein-fojo"
python3 design/benchmarks/affine-stein-fojo/summarise.py
```
