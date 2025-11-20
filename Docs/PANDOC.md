# Pandoc

## Docx to Markdown with Pandoc on Ubuntu 24.04

```bash
$ sudo apt install -y pandoc
$ pandoc a.docx -t markdown --wrap=none -o a.md
```

## Docx to PDF with Pandoc on Ubuntu 24.04

```bash
$ sudo apt install -y pandoc
$ sudo apt install texlive-latex-base texlive-xetex texlive-fonts-recommended texlive-latex-extra texlive-fonts-extra fonts-noto-cjk fonts-noto-cjk-extra
$ pandoc a.docx --pdf-engine=xelatex -V mainfont="Noto Sans CJK KR" -V geometry:margin=0.5in -o a.pdf
```
