# Definiti - scala model

For core project, see [Definiti/Definiti](https://github.com/definiti/definiti).

## Get started

⚠ This project is the subject of active research.
It is not ready for production use yet.

This part describes how to test the generator with the core compiler.
For more advanced usage, please come back later.

### Dependency: Core

Clone the repository:

```sh
$ git clone git@github.com:definiti/definiti.git
```

Go into the project and publish to local repository:

```
$ cd definiti
$ sbt publishLocal
```

⚠️ Be careful of the core version during development.
Check it in `build.sbt` file of each project.

### Scala model generator

Clone the repository:

```sh
$ git clone git@github.com:definiti/definiti-scala-model.git
```

Go into the project and launch sbt:

```
$ cd definiti
$ sbt run
```

It will read the file `src/main/resources/samples/first.def`
and save the results into `target/samples/result.scala` directory.

Feel free to change the `first.def` file to test it.

## Roadmap (not ordered)

* [x] First alpha alpha MVP
* [ ] Generate a more usable and maintainable scala AST
* [ ] Create documentation
* [ ] Create command-line tool
* [ ] Follow updates of core project