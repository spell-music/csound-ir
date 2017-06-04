# Csound-ir -- Make your instrument shine with Pro reverbs

It's a package for loading Impulse responses from the [samplicity collection](http://www.samplicity.com/).
It's made for the library csound-expression.

Right now two collections are implemented:

* Bricasti M7 (M7)

* TCE System 6000 (T6)

## Install

The library can be installed with cabal. Download the source cd to the directory
and run just run:

~~~
cabal install
~~~

Or with Hackage run anywhere:

~~~
cabal update
cabal install csound-ir
~~~

This installs the haskell definitions. But to use them we also need
the actual samples to be installed.

Download them from the [samplicity site](http://www.samplicity.com/).
Note that the M7 collection is free but the T6 comes with modest price.
to try the things out we can use only M7. Download it and place in your
home directory inside the directory:

~~~
USER_HOME/.csound-ir
~~~

We need to create the directory `.csound-ir` in our home directory.
Next place the collection of IRs there. It should look something like this:

~~~
USER_HOME/.csound-ir/Samplicity M7 Main - 01 - Wave, 32 bit, 44.1 Khz, v1.1
~~~

We are ready to go!

## Quick start


To use this library just import the preset collection
and apply the reverb to the signal:

~~~haskell
import Csound.Base
import Csound.Ir.M7

pluck = fromMono $linseg [1, 0.2, 0] * osc 220

main = dac $ hDense 0.35 pluck
~~~

The first argument is Dry/Wet ratio the second argument is an imput signal
(preferably stereo Sig2 or SE Sig2).

All presets have the signature:

~~~haskell
hDense :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
~~~

The type is rather dense and brainy indeed. But this is all need
to signify that we are fgoing to apply it to something that is stereo
or stereo-like signal. It's to be able to apply it to both `Sig2` and `SE Sig2`.
That is pure signals and signals with side effects.

## Common naming conventions

All presets have single letter prefix that signifies the type of the reverb.
conventions:

* `hAbc`  - hall Abc

* `rAbc` - room abc

* `cAbc` - chamber abc

* `aAbc` - ambience abc

* `sAbc` - Abc space

* `pAbc` - plate abc

## Local IRs

Sometimes we want to distribute the samples with generated csd in a single archive.
But the library of IR's ties us to the USER_HOME. Not so good :(

We can use the local version for the IRs placement if we import the module with
suffix Local:

~~~haskell
import Csound.Ir.M7.Local
~~~

Then the samples are going to be searched for in the directory `samples`
located at the same level as generated `csd` file.

Note that there is no need to copy all samples. Copy only those you truly need and like.


Happy Csounding
in Adventurous spaces!


## Acknowledgments

Big thanks to [Peter Emanuel Roos](http://site.peterroos.com/index.html) who made
available Hi-quality Impulse responses and prepared them with such a precision and dedication.
