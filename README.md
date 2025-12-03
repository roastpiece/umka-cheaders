# umka-cheaders
is a CLI Tool to generate type declarations from C Header files for [umka-lang](https://github.com/vtereshkov/umka-lang).

## Features
- [x] Support for *function* definitions
- [x] Support for *struct* definitions
- [x] Support for *enum* definitions
- [x] Zero third-party dependencies

## Usage
It is recommended to run header files through the C Preprocessor (`cpp -P`) as all prerocessor statements are ignored.
```terminal
Usage: umka-cheaders [arguments] <file.h>
Arguments:
	-output=<file.um>	optional	 Output to file instead of stdout
	-fnprefix=<prefix>	optional	 Prefix fn declarations with <prefix> (eg: -fnPrefix=ffi -> `ffi fn foo*();`)
	-nostructs		optional	 Do not generate struct declarations
	-noenums		optional	 Do not generate enum declarations
	-nofuncs		optional	 Do not generate fn declarations
	-keepunresolved	optional	 Do not skip declarations with unresolved types
```

## Building
Only `ghc` is required.
```terminal
$ ghc Main.hs -o umka-cheaders
```

### Project structure
- `C.hs` contains the C parser
- `Umka.hs` contains the generator for umka from the C ast
- `Main.hs` contains cli
