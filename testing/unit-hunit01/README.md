# unit-hunit01

## Install hackages

```sh
stack install HUnit
```

## Create a new project

```sh
stack new <project-name>
cd <project-name>
```

## Add hackages to project's package config file

```sh
vim package.yaml
```

```yaml
...

dependencies:
- base >= 4.9.1.0 && < 5
- HUnit
```

## Add test code

Please check test/*.hs files

## Run test

```sh
stack test
```
