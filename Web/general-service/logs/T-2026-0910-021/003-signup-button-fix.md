task_id: T-2026-0910-021
from: coder
status: REVIEW
summary: "회원가입 버튼이 비활성화된 원인을 수정했다."
assumptions: []
result: |
  `TaskBoard.initialModel.loading`이 `True`였지만 인증 화면에서는 초기 `LoadTasks` 효과가 없다. 화면이 첫 렌더링된 뒤에도 `loading`이 갱신되지 않아 `disabled model.loading`인 가입 버튼이 클릭을 받을 수 없었다. 초기값을 `False`로 변경했다.
sources: []
open_questions: []
risks: []
cost:
  tokens: 0
  tool_calls: 2
