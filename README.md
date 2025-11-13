 # Agda backend for [Forester](https://www.forester-notes.org/)

This is an experimental project implementing a backend to export
literate agda to forester trees.

## Building

The project depends on Agda, and is compatable with Forester 5

If you have nix installed, the most simple way to build and run this tool is via `nix-build` or `nix-shell`. Running `nix-shell` will put you in an environment with access to the `agda-forester` command.

## Usage

Example usage:

```
agda-forester --forest -o trees/agda -S1 src/Everything.agda 
```

Will compile all `.agda` and `.lagda.tree` files to `.tree` files and place them in the `trees/agda` directory.

## Project setup

The recommended way to structure your project is with a source directory containing agda code, seperate to the forester src folder.

It is recommended to use the theme at [github:samtoth/forest-theme/tree/agda-forester](https://github.com/samtoth/forest-theme/tree/agda-forester), which provides the `Agda.css` file.

You will also require that the macros defined in [./macros.tree](./macros.tree) are imported somewhere in each literate tree file.

For an example of how to structure a project check out [this project](https://github.com/samtoth/agda-synthetic-categories)

## Enabling links

~~Currently, in order to get links to work, you need to annotate your
literate tree files with meta commands.~~

Links now work automatically :)
