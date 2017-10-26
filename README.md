# bowtie ⋈ #
A program to perform analysis on Time Interval Error (TIE)

This program was build to perform [MTIE](https://en.wikipedia.org/wiki/Maximum_time_interval_error) and [TDEV](https://en.wikipedia.org/wiki/Time_deviation) analysis on TIE values. The program was tested with data recorded with the omniber 718 and the sj300e. Other devices should be compatible if they use plain column based text files.

Further information and copyright can be found in bowtie.cabal

## Build/run/debug commands ##

### Without llvm ###
`stack build --executable-stripping --ghc-options="-j4 -O2 -fexcess-precision -optc-O3 -optc-ffast-math -rtsopts=none -no-rtsopts-suggestions -split-objs -split-sections"`

### With llvm 3.7 ###
`stack build --executable-stripping --ghc-options="-j4 -O2 -fexcess-precision -optc-O3 -optc-ffast-math -rtsopts=none -no-rtsopts-suggestions -split-objs -split-sections -fllvm"`

### With llvm 3.7 into release folder ###
Use this build command to make the release binary.
`stack install --local-bin-path release --executable-stripping --ghc-options="-j4 -O2 -fexcess-precision -optc-O3 -optc-ffast-math -rtsopts=none -no-rtsopts-suggestions -split-objs -split-sections -fllvm"`

### With llvm 3.7 and threading ###
Do not use this build command. There is no parallelism in the program and it makes the program actually slower.
`stack build --executable-stripping --ghc-options="-j4 -O2 -fexcess-precision -optc-O3 -optc-ffast-math -threaded -rtsopts=none -with-rtsopts=-N -no-rtsopts-suggestions -split-objs -split-sections -fllvm"`

### Build for strace (to see system calls) ###
`stack build --executable-stripping --ghc-options="-j4 -O2 -fexcess-precision -optc-O3 -optc-ffast-math -rtsopts -no-rtsopts-suggestions -split-objs -split-sections -fllvm"`
Run with `stack exec -- strace -e 'trace=!getrusage,rt_sigprocmask' bowtie args +RTS -V0`

### Build for profiling ###
`stack build --profile --fast --ghc-options="-j -auto-all +RTS -A128m -n2m -RTS"`
Run with profiling: `stack exec -- bowtie +RTS -p`
Run with profiling for exceptions: `stack exec -- bowtie +RTS -xc`

### Verifying program stdout/stderr ###
`bowtie > >(sed 's/^/stdout:/') 2> >(sed 's/^/stderr:/' >&2); sleep 1`

### (GHCI) run commands for testing ###
```
  :set -fbreak-on-exception
  :set -fbreak-on-error
  :set args -c mtie -i sample_sj300e_bin.dat -d sj300e_binary
  :set args -c mtie -i processed_samples_time.dat -t 1 -v 2
  :trace main

  :main -c mtie -i sample_sj300e_bin_short.dat -d sj300e_binary
```

Measure time: `/usr/bin/time stack exec -- bowtie -c mtie -i sample_sj300e_bin.dat -d sj300e_binary`

## Todo's ##
* Instead of using `evalStateC` in `adjustSampleInterval` use `runStateC` so that a summary of the input data can be shown.
* Create a command line flag to limit the upper bound of the calculated samples. This is now always the maximum see `n_max` in `tdevs9` (but also applies for mtie). This is useful for two reasons 1. the standards often require that more samples are needed to give a data point reliable, that means for the samples at the upper bound need to be trimmed 2. saves some calculations.
* Remove the skipValues option. The data should already be clean when feeding into bowtie.
* Add [exit codes](https://hackage.haskell.org/package/base-4.10.0.0/docs/System-Exit.html) `[exitFoo, exitBar] = map (exitWith . exitFailure) [2..]` or `(foo, bar, _) map bla [1..]`
* Show the program version without having to specify the `-c` flag (this is just silly right now).
* Investigate to use logarithmic for mtie just as for tdev.

## Nice to have's ##
* Criterion benchmarking for different algorithm implementations [criterion tutorial](http://www.serpentine.com/criterion/tutorial.html)
* Any further speed optimization, [flamegraphs](https://www.fpcomplete.com/blog/2015/04/ghc-prof-flamegraph) are useful.
* Try to boost performance with unpacking values. [1](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#unpack-pragma) [2](http://johantibell.com/files/haskell-performance-patterns.html)
* Look into exporting information about the data (possible to a file), this could be used to assist gnuplot.
* Look into refactoring accConduit and tdev into a single function somehow, they belong together but are not spread out in the code.
* Test if parallelism helps with calculating the mtie (on a simple test it didn't help with tdev). Possibly use [stm-conduit](https://hackage.haskell.org/package/stm-conduit)
* Optimize the parsing function (this showed to be a bit slow during profiling) [Efficient parsing guide](http://hbtvl.banquise.net/series/Efficient%20parsing.html)
* Remove resampling function from the program. The data should already be clean when feeding into bowtie. If it's still needed this could be broken out in a separate program and use linux pipes (bowtie supports stdin/stdout for data).
* Possibly make another program that can add timestamps to single-column data or turn two-column (time, value) into value only. Likely it's a good idea to put this with the resampling program (see point above).
* Recalculate samples given a `t`. Useful when two-column input doesn't start at `t=0` or if a part of the recording needs to be skipped. All samples need to be corrected for the offset at the given time.
* Proof that all the safety checks in the mtie algorithm are actually needed or if they can be removed for more performance.
* Look into further algorithm improvements for mtie (look in Algos.hs for recommendations)
