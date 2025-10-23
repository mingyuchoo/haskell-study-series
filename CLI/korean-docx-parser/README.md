# korean-docx-parser

한글 DOCX 파일을 Markdown 형식으로 변환하는 Haskell 파서입니다.

## 기능

- DOCX 파일에서 한글 텍스트 추출
- 텍스트 스타일 변환 (굵게, 기울임, 밑줄)
- 문서 레이아웃 정보 추출 (제목 레벨)
- Markdown 형식으로 출력

## 설치

Stack을 사용하여 빌드합니다:

```bash
stack build
```

## 사용법

프로그램은 `input/애국가.docx` 파일을 읽어서 `output/애국가.md` 파일로 변환합니다:

```bash
stack exec korean-docx-parser-exe
```

### 입력/출력

- **입력**: `input/애국가.docx` - 변환할 한글 DOCX 파일
- **출력**: `output/애국가.md` - 변환된 Markdown 파일

## 스타일 변환 규칙

| DOCX 스타일 | Markdown 형식 |
|------------|--------------|
| 굵게 (Bold) | `**텍스트**` |
| 기울임 (Italic) | `*텍스트*` |
| 밑줄 (Underline) | `_텍스트_` |
| 굵게 + 기울임 | `***텍스트***` |
| 제목 1 | `# 텍스트` |
| 제목 2 | `## 텍스트` |
| 제목 3 | `### 텍스트` |
| 제목 4 | `#### 텍스트` |

## 기술 스택

- **Haskell**: 함수형 프로그래밍 언어
- **zip-archive**: DOCX ZIP 아카이브 처리
- **xml-conduit**: XML 파싱 및 커서 기반 탐색
- **text**: 유니코드 텍스트 처리

## 프로젝트 구조

```
korean-docx-parser/
├── app/
│   └── Main.hs           # 메인 실행 파일
├── src/
│   └── Lib.hs            # DOCX 파싱 및 변환 로직
├── input/                # 입력 DOCX 파일 디렉터리
├── output/               # 출력 Markdown 파일 디렉터리
├── package.yaml          # 프로젝트 설정
└── stack.yaml            # Stack 빌드 설정
```

## 라이선스

BSD-3-Clause

## 작성자

Mingyu Choo (mingyuchoo@gmail.com)
