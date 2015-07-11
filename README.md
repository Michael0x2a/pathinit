# Pathinit

A Windows path management system

## Motivation

In order to use an arbitrary binary from anywhere in your system on Windows, you need to add the path to the directory containing the binary to the PATH environment variable.

Unfortunately, it turned out that environment variables in Windows have a character limit, preventing me from extending the PATH with arbitrary directories. Given that I like to the bulk of my work exclusively from the command line, and given that I have a tendency to install and experiment with a multitude of programming languages, this presents a problem.

I decided to resolve this problem by using the approach advocated on [this StackOverflow post](http://stackoverflow.com/a/11705295/646543), and wrote this program to help manage my batch files.

## Usage

    pathinit add <new_path> <group>
    pathinit remove <path> <group>
    pathinit list all
    pathinit list summary
    pathinit list <group>
    pathinit regenerate
    pathinit help
    pathinit -h|--help
    init-<group>

## Build

    ghc pathinit.hs

## Todo

- Allow the user to initialize by using `pathinit <group>` instead of having to run a separate batch file under `init-<group>`.
- Expand this readme, and otherwise make this project slightly more suitable for general use
