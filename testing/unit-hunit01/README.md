# unit-hunit01

## Install hackages

```bash
stack install HUnit
```

## Create a new project

```bash
stack new <project-name>
cd <project-name>
```

## Add hackages to project's package config file

```bash
vim package.yaml
```

```yaml
...

dependencies:
- base >= 4.7 && < 5
- HUnit
```

## Add test code

Please check test/*.hs files

## Run test

```bash
stack test
```
