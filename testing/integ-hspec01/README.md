# integ-hspec01

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
- base >= 4.9.1.0 && < 5
- hspec
- QuickCheck
```

## Add test code

Please check test/*.hs files

## Run test

```bash
stack test
```
