# NoFib: Haskell Benchmark Suite

This is the root directory of the "NoFib Haskell benchmark suite". It
should be part of a GHC source tree, that is the 'nofib' directory
should be at the same level in the tree as 'compiler' and 'libraries'.
This makes sure that NoFib picks up the stage 2 compiler from the
surrounding GHC source tree.

You can also clone this repository in isolation, in which case it will
pick `$(which ghc)` or whatever the `HC` environment variable is set to.

Additional information can also be found on
[NoFib's wiki page](https://ghc.haskell.org/trac/ghc/wiki/Building/RunningNoFib).

## Package Depedencies

Please make sure you have the following packages installed for your
system GHC:
 * html
 * regex-compat (will install: mtl, regex-base, regex-posix)

## Using

Install the [package dependencies](#package-dependencies):

```
$ cabal install html regex-compat
```

Then, to run the tests, execute:

```
$ make clean # or git clean -fxd, it's faster
$ # Generates input files for the benchmarks and builds compilation
$ # dependencies for make (ghc -M)
$ make boot
$ # Builds the benchmarks and runs them $NoFibRuns (default: 5) times
$ make
```

This will put the results in the file `nofib-log`. You can pass extra
options to a nofib run using the `EXTRA_HC_OPTS` variable like this:

```
$ make clean
$ make boot
$ make EXTRA_HC_OPTS="-fllvm"
```

To compare the results of multiple runs, save the output in a logfile
and use the program in `../utils/nofib-analyse`, for example:

```
...
$ make 2>&1 | tee nofib-log-6.4.2
...
$ make 2>&1 | tee nofib-log-6.6
$ nofib-analyse nofib-log-6.4.2 nofib-log-6.6 | less
```

to generate a comparison of the runs in captured in `nofib-log-6.4.2`
and `nofib-log-6.6`. When making comparisons, be careful to ensure
that the things that changed between the builds are only the things
that you _wanted_ to change. There are lots of variables: machine,
GHC version, GCC version, C libraries, static vs. dynamic GMP library,
build options, run options, and probably lots more. To be on the safe
side, make both runs on the same unloaded machine.

## Modes

Each benchmark is runnable in three different time `mode`s:

- `fast`: 0.1-0.2s
- `norm`: 1-2s
- `slow`: 5-10s

You can control which mode to run by setting an additional `mode` variable for
`make`. The default is `mode=norm`. Example for `mode=fast`:

```
$ make clean
$ make boot mode=fast
$ make mode=fast
```

Note that the `mode`s set in `make boot` and `make` need to agree. Otherwise you
will get output errors, because `make boot` will generate input files for a
different `mode`. A more DRY way to control the `mode` would be

```
$ make clean
$ export mode=fast
$ make boot
$ make
```

As CPU architectures advance, the above running times may drift and
occasionally, all benchmarks will need adjustments.

Be aware that `nofib-analyse` will ignore the result if it falls below 0.2s.
This is the default of its `-i` option, which is of course incompatible with
`mode=fast`. In that case, you should just set `-i` as appropriate, even
deactivate it with `-i 0`.

## Configuration

There are some options you might want to tweak; search for nofib in
`../mk/config.mk`, and override settings in `../mk/build.mk` as usual.

## Extra Metrics: Valgrind

To get instruction counts, memory reads/writes, and "cache misses",
you'll need to get hold of Cachegrind, which is part of
[Valgrind](http://valgrind.org).

You can then pass `-cachegrind` as `EXTRA_RUNTEST_OPTS`. Counting
instructions slows down execution by a factor of ~30. But it's
a deterministic metric, so you can combine it with `NoFibRuns=1`:

```
$ (make EXTRA_RUNTEST_OPTS="-cachegrind" NoFibRuns=1) 2>&1 | tee nofib-log
```

Optionally combine this with `mode=fast`, see [Modes](#modes).

## Extra Packages

Some benchmarks aren't run by default and require extra packages are
installed for the GHC compiler being tested. These packages include:
 * stm - for smp benchmarks

## Adding benchmarks

If you add a benchmark try to set the problem sizes for
fast/normal/slow reasonably. [Modes](#modes) lists the recommended brackets for
each mode.

### Stability wrt. GC paramerisations

Additionally, pay attention that your benchmarks are stable wrt. different 
GC paramerisations, so that small changes in allocation don't lead to big,
unexplicable jumps in performance. See Trac #15999 for details. Also make sure
that you run the benchmark with the default GC settings, as enlarging Gen 0 or
Gen 1 heaps just amplifies the problem.

As a rule of thumb on how to ensure this: Make sure that your benchmark doesn't
just build up one big data and consume it in a final step, but rather that the
working set grows and shrinks (e.g. is approximately constant) over the whole
run of the benchmark. You can ensure this by iterating your main logic $n times
(how often depends on your program, but in the ball park of 100-1000).
You can test stability by plotting productivity curves for your `fast` settings
with the `prod.py` script attached to Trac #15999.

If in doubt, ask Sebastian Graf for help.
