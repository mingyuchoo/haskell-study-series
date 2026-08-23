# API Contract: 이름

- 상태: Draft
- 소유 패키지: 패키지 이름
- 소비자: 소비자 목록
- 버전: v1

## Purpose

이 API가 제공하는 비즈니스 기능을 설명합니다.

## Authentication and Authorization

필요한 주체와 권한을 설명합니다. 없다면 없다는 사실과 그 전제를 명시합니다.

## Resources

경로, 메서드와 의미를 설명합니다. 라우트의 기계적 사실은 `../../generated/route-map.md`에 있습니다.

## Request

요청 스키마와 제약을 설명합니다.

## Response

성공 응답과 상태 의미를 설명합니다.

## Errors

안정적인 오류 코드, HTTP 상태 매핑, 재시도 가능 여부와 소비자 동작을 설명합니다.

## Idempotency and Retry

멱등성 범위, 반복 요청의 결과와 재시도 동작을 설명합니다.

## Compatibility

호환성 보장과 폐기 절차를 설명합니다.

## Verification

계약 테스트 위치를 연결합니다.
