# Changelog for `my-org`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Changed

- 선택 PostgreSQL 저장소를 SQLite(`sqlite-simple`, `MY_ORG_SQLITE_FILE`)로 교체했습니다. 기본 JSON 저장, 기존 이벤트 형식과 데모의 전용 JSON 저장은 유지합니다.
- SQLite 이벤트 저장은 트랜잭션과 연결 수명 동안의 독점 잠금을 사용합니다. 기존 JSON 또는 PostgreSQL 데이터를 자동 변환하지 않습니다.
- README에 SQLite 실행 방법, 저장소 선택 우선순위 및 잠금 동작을 추가했습니다.

## 0.1.0.0 - YYYY-MM-DD
