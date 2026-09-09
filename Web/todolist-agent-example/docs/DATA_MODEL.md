# 데이터 모델

## `todos`

| 열 | 타입 | Null 허용 | 의미 |
|---|---|---:|---|
| `id` | UUID | 아니요 | 불투명한 Todo ID |
| `title` | VARCHAR(200) | 아니요 | 검증된 제목 |
| `description` | TEXT | 예 | 선택적 설명 |
| `status` | VARCHAR(20) | 아니요 | `ACTIVE` 또는 `COMPLETED` |
| `created_at` | TIMESTAMPTZ | 아니요 | 생성 시각 |
| `completed_at` | TIMESTAMPTZ | 예 | 완료 시각 |

제약 조건은 유효한 상태값과 상태 및 `completed_at` 간의 일관성을 보장한다.

이미 배포된 마이그레이션은 변경할 수 없으며, 스키마 변경에는 새 마이그레이션 파일이 필요하다.
