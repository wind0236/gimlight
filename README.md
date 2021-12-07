# Gimlight

## Running

### Windows

I think we can build and run this project on Windows, but I don't know how. So, if you find the way please tell me the instructions.

### Linux

You need to install `libsdl2` and `libglew`. For Ubuntu you need to run this command:

```sh
sudo apt install libsdl2-dev libglew-dev
```

After installing these dependencies, run this command on the project root:

```sh
LANG=C cabal run
```

`LANG=C` is necessary to prevent a build error. See [the c2hs' issue](https://github.com/haskell/c2hs/issues/238).

## License

All files **EXCEPT** any materials in the `third_party` directory are licensed under BSD 3-Clause License. For the licenses of the contents in the `third_party` directory, see the LICENSE files in each directory.
