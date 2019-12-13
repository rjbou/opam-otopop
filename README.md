# opam-otopop: install package and automatically populate opam file

This opam plugin can install a package, as opam would do, and then adds it as a
dependency in the given opam file.

opam file to update is specified using option `--opam-file` (file or
directory), or current directory if nothing specified. Note that an error is
raised if multiple opam files are found in a directory.

The plugin operates in three steps:
- check that the opam file don't error on lint
- launch opam install operation (all install arguments are usable)
- update well installed specified packages as dependencies in the the opam file

By default, `depends:` field is updated. It is possible to use `--depopt`
option to update `depopts:` field. In the case of pinned packages install,
`pin-depends:` field is also updated.

When constraints are expressed on the package to install, they are also added
in the dependencies as a filter, replacing previous ones and keeping non
version related filters (version constraints are all removed):

```
$ opam show -f depends: ./foo.opam
"lorem"
"ipsum"
"bar" {<= "1.0" & build}

$ opam otopop "bar<0.5" "bar>0.2"
Update local opam file foo.opam? [Y/n] y
The following actions will be performed:
  ∗ install bar 0.3

<><> Processing actions <><><><><><><><><><><><><><><><><><><><><><><><><><><><>
▼ retrieved bar.0.3  (cached)
∗ installed bar.0.3
Done.
Update dependencies in foo.opam:
  - update: [bar {<= "1.0" & build} -> bar {> "0.2" & < "0.4" & build}]

$ opam show -f depends: ./foo.opam
"lorem"
"ipsum"
"bar" {> "0.2" & < "0.4" & build}
```

This project is currently in early beta.
