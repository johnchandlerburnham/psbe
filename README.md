# psbe-exercises
Exercises for Purescript by Example (Freeman)

## Purescript Setup on NixOS

```
nix-shell
npm install bower pulp
PATH=node_modules/.bin:$PATH
alias pp='pulp --psc-package'
pp init
```

In the chapter directories, symlink the root directory `node_modules` with

```
ln -s ../node_modules node_modules
```


