# T-GUIDE-COLLAPSED

요구사항: 체험 가이드는 웹 앱의 최초 진입 및 새로고침 시 기본적으로 접힌 상태여야 한다.

확인한 사실: Main.elm 초기 모델은 guideOpen = True이며, ToggleGuide가 현재 상태를 반전한다. Ui.Guide.view는 False일 때 체험 단계와 보조 링크를 숨기고 “체험 가이드 열기” 버튼 및 요약을 표시한다. 별도 저장된 접힘 상태는 사용하지 않는다.

계획:
1. coder가 static/src/Main.elm 초기 guideOpen을 False로 변경한다. 기존 ToggleGuide 및 화면 이동 중 선택 상태 유지 동작은 보존한다.
2. 배포 화면에 반영하도록 기존 빌드 명령으로 static/app.js 산출물을 갱신한다.
3. tester가 make test와 프런트엔드 빌드를 검증한다. reviewer는 변경이 초기값과 생성 산출물에 국한되는지, 기존 열기/접기 기능이 유지되는지 확인한다.

완료 조건: 첫 로드에서는 단계가 접혀 있고 사용자가 버튼으로 열고 다시 접을 수 있다.
