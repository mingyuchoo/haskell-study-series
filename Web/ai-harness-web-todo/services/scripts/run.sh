#!/usr/bin/env bash
set -euo pipefail

# services 아래 모든 패키지를 한 번에 다루는 스크립트입니다.
#
# 패키지 하나만 다룰 때는 services/<패키지>/scripts/run.sh를 사용합니다.
# 이 스크립트는 그 지역 스크립트들에 일을 넘기고 결과를 모아 보여줍니다.
# 따라서 빌드와 테스트 방법의 정의는 한 곳에만 있습니다.
#
# 저장소 전체 검증(형식, 아키텍처 규칙, 컨텍스트 무결성)은 이 스크립트가 아니라
# ../../scripts/quality와 ../../scripts/context 아래 스크립트가 담당합니다.
#
# web 명령은 브라우저 표면을 기동합니다. 서버는 루프백에만 바인딩하며 이 스크립트가
# 수신 주소를 바꿀 수 없습니다. 그것은 설정이 아니라 경계입니다
# (../../docs/decisions/ADR-0004-web-surface.md).

services_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
repo_root="$(cd "$services_root/.." && pwd)"

# 브라우저 표면과 HTTP API의 패키지 이름 및 기본 포트입니다.
# 기본값의 Canonical Source는 각 실행 파일이며 여기서는 안내와 사전 확인에만 씁니다.
web_package="todo-web"
api_package="todo-api"
default_web_port=8081
default_api_port=8080
default_database="$repo_root/todo.db"

# 패키지는 자기 이름과 같은 cabal 파일을 가진 디렉터리로 정의합니다.
# 이 정의 덕분에 services/scripts 같은 공용 디렉터리가 패키지로 오인되지 않고,
# 새 패키지를 추가해도 이 스크립트를 고칠 필요가 없습니다.
packages() {
  local directory name
  for directory in "$services_root"/*; do
    [[ -d "$directory" ]] || continue
    name="$(basename "$directory")"
    [[ -f "$directory/$name.cabal" ]] || continue
    printf '%s\n' "$name"
  done
}

executables_of() {
  awk '/^executable[[:space:]]+/ { print $2 }' "$services_root/$1/$1.cabal"
}

runnable_packages() {
  local package
  for package in $(packages); do
    if [[ -n "$(executables_of "$package")" ]]; then
      printf '%s\n' "$package"
    fi
  done
}

package_script() {
  printf '%s\n' "$services_root/$1/scripts/run.sh"
}

# 상대 경로는 사용자가 스크립트를 실행한 위치를 기준으로 풉니다.
# 지역 스크립트가 저장소 루트로 이동하므로, 풀어 두지 않으면 데이터베이스 파일이
# 사용자가 기대하지 않은 곳에 생깁니다.
absolute_path() {
  case "$1" in
    /*) printf '%s\n' "$1" ;;
    *) printf '%s/%s\n' "$PWD" "$1" ;;
  esac
}

require_port() {
  local label="$1" value="$2"
  if [[ ! "$value" =~ ^[0-9]+$ ]] || ((value < 1 || value > 65535)); then
    printf '%s 포트가 올바르지 않습니다: %s\n' "$label" "$value" >&2
    exit 2
  fi
}

# bash 내장 /dev/tcp로 확인합니다. lsof나 nc가 없는 환경에서도 동작합니다.
# 연결에 성공하면 이미 무언가 듣고 있다는 뜻입니다.
port_in_use() {
  (exec 3<>"/dev/tcp/127.0.0.1/$1") 2>/dev/null
}

require_free_port() {
  local label="$1" port="$2"
  if port_in_use "$port"; then
    printf '%s 포트를 이미 무언가 사용하고 있습니다: %s\n' "$label" "$port" >&2
    printf '다른 포트를 지정하거나 그 프로세스를 먼저 종료하십시오.\n' >&2
    exit 1
  fi
}

usage() {
  cat <<USAGE
사용법: services/scripts/run.sh [명령] [인자...]

명령
  build          모든 패키지를 빌드합니다
  test           모든 패키지의 테스트를 실행합니다
  all            빌드한 뒤 테스트합니다 (기본값)
  web [옵션]     브라우저 표면을 빌드하고 기동합니다
  run PACKAGE    지정한 앱을 실행합니다. 뒤의 인자는 앱에 그대로 전달됩니다
  list           패키지와 실행 가능 여부를 보여줍니다
  help           이 도움말을 출력합니다

web 옵션
  --port N       수신 포트. 기본값 $default_web_port
  --db PATH      SQLite 파일 경로. 기본값 $default_database
  --with-api     HTTP API도 함께 기동합니다. 같은 데이터베이스를 봅니다
  --api-port N   API 수신 포트. 기본값 $default_api_port
  --test         기동 전에 모든 패키지의 테스트를 실행합니다
  --no-build     빌드를 건너뜁니다. 이미 빌드한 뒤 다시 띄울 때 씁니다

예시
  services/scripts/run.sh
  services/scripts/run.sh test
  services/scripts/run.sh web
  services/scripts/run.sh web --test --with-api
  services/scripts/run.sh web --port 9000 --db /tmp/todo.db
  services/scripts/run.sh run todo-cli -- list --all

build와 test는 한 패키지가 실패해도 멈추지 않고 나머지를 계속 실행한 뒤
마지막에 요약을 보여줍니다. 하나라도 실패하면 종료 코드가 1입니다.

서버는 127.0.0.1에만 바인딩합니다. 이 앱에는 인증이 없어 접근 경로 제한이
유일한 방어선이며, 수신 주소를 바꾸는 설정은 제공하지 않습니다.
USAGE
}

# 각 패키지의 지역 스크립트에 같은 명령을 넘기고 결과를 모읍니다.
passed=""
failed=""

for_each_package() {
  local action="$1"
  local package script
  passed=""
  failed=""

  for package in $(packages); do
    script="$(package_script "$package")"
    printf '\n==== %s: %s ====\n' "$package" "$action"
    if [[ ! -x "$script" ]]; then
      printf '지역 스크립트를 실행할 수 없습니다: %s\n' "$script" >&2
      failed="$failed $package"
      continue
    fi
    if "$script" "$action"; then
      passed="$passed $package"
    else
      failed="$failed $package"
    fi
  done
}

report() {
  local action="$1"
  printf '\n==== 요약: %s ====\n' "$action"
  if [[ -n "$passed" ]]; then
    printf '통과:%s\n' "$passed"
  fi
  if [[ -n "$failed" ]]; then
    printf '실패:%s\n' "$failed" >&2
    return 1
  fi
  return 0
}

if [[ -z "$(packages)" ]]; then
  printf 'services 아래에서 패키지를 찾지 못했습니다.\n' >&2
  exit 1
fi

command_name="${1:-all}"
if [[ "$#" -gt 0 ]]; then
  shift
fi

case "$command_name" in
  help | -h | --help)
    usage
    ;;

  list)
    # printf의 폭 지정은 바이트를 세므로 한글을 열에 넣으면 정렬이 깨집니다.
    # 머리글은 정렬 없이 한 줄로 두고 데이터 열만 ASCII로 맞춥니다.
    printf '패키지 / 실행 파일 / 지역 스크립트\n\n'
    for package in $(packages); do
      executable="$(executables_of "$package" | awk 'NR == 1')"
      printf '%-12s %-10s %s\n' \
        "$package" \
        "${executable:--}" \
        "services/$package/scripts/run.sh"
    done
    ;;

  build | test)
    for_each_package "$command_name"
    report "$command_name"
    ;;

  all)
    for_each_package build
    report build
    for_each_package test
    report test
    ;;

  web)
    web_port="${TODO_WEB_PORT:-$default_web_port}"
    api_port="${TODO_API_PORT:-$default_api_port}"
    database="${TODO_DB:-$default_database}"
    with_api=0
    run_tests=0
    do_build=1

    while [[ "$#" -gt 0 ]]; do
      case "$1" in
        --port)
          web_port="${2:-}"
          shift 2
          ;;
        --port=*)
          web_port="${1#*=}"
          shift
          ;;
        --api-port)
          api_port="${2:-}"
          shift 2
          ;;
        --api-port=*)
          api_port="${1#*=}"
          shift
          ;;
        --db)
          database="${2:-}"
          shift 2
          ;;
        --db=*)
          database="${1#*=}"
          shift
          ;;
        --with-api)
          with_api=1
          shift
          ;;
        --test)
          run_tests=1
          shift
          ;;
        --no-build)
          do_build=0
          shift
          ;;
        *)
          printf 'web 명령이 모르는 옵션입니다: %s\n\n' "$1" >&2
          usage >&2
          exit 2
          ;;
      esac
    done

    if [[ -z "$database" ]]; then
      printf '데이터베이스 경로가 비어 있습니다.\n' >&2
      exit 2
    fi
    database="$(absolute_path "$database")"
    require_port "웹" "$web_port"
    if [[ "$with_api" -eq 1 ]]; then
      require_port "API" "$api_port"
      if [[ "$web_port" == "$api_port" ]]; then
        printf '웹과 API가 같은 포트를 쓸 수 없습니다: %s\n' "$web_port" >&2
        exit 2
      fi
    fi

    # 빌드와 테스트의 정의는 지역 스크립트에 있습니다. 여기서 cabal을 직접 부르지
    # 않는 이유는 방법이 두 곳에 생기면 갈라지기 때문입니다.
    if [[ "$do_build" -eq 1 ]]; then
      printf '\n==== %s: build ====\n' "$web_package"
      "$(package_script "$web_package")" build
      if [[ "$with_api" -eq 1 ]]; then
        printf '\n==== %s: build ====\n' "$api_package"
        "$(package_script "$api_package")" build
      fi
    fi

    if [[ "$run_tests" -eq 1 ]]; then
      for_each_package test
      report test
    fi

    # 기동 직전에 확인합니다. 빌드와 테스트에 시간이 걸리는 동안 포트 상황이
    # 달라질 수 있습니다.
    require_free_port "웹" "$web_port"
    [[ "$with_api" -eq 1 ]] && require_free_port "API" "$api_port"

    export TODO_DB="$database"
    export TODO_WEB_PORT="$web_port"
    export TODO_API_PORT="$api_port"

    printf '\n==== 기동 ====\n'
    printf '  브라우저   http://127.0.0.1:%s\n' "$web_port"
    if [[ "$with_api" -eq 1 ]]; then
      printf '  HTTP API   http://127.0.0.1:%s/todos\n' "$api_port"
    fi
    printf '  데이터     %s\n' "$database"
    printf '  종료       Ctrl+C\n\n'

    if [[ "$with_api" -eq 0 ]]; then
      # 감쌀 필요가 없으면 프로세스를 교체합니다. Ctrl+C가 서버에 바로 갑니다.
      exec "$(package_script "$web_package")" run
    fi

    # 두 서버를 함께 띄울 때만 이 스크립트가 부모로 남습니다. 둘 중 하나가 끝나면
    # 다른 하나도 정리해 고아 프로세스를 남기지 않습니다.
    #
    # 정리에 두 가지 장치가 필요합니다.
    #
    # 1. 작업 제어를 켜서(set -m) 각 자식이 자기 프로세스 그룹의 리더가 되게 합니다.
    #    지역 스크립트는 cabal을 부르고 cabal이 다시 실행 파일을 띄우므로, 자식 하나만
    #    죽이면 그 아래 실행 파일이 포트를 쥔 채 남습니다. 그룹째 종료해야 합니다.
    # 2. 두 서버를 모두 배경으로 띄우고 wait로 기다립니다. 포그라운드 자식을 기다리는
    #    동안에는 트랩이 실행되지 않아, 신호를 받아도 정리가 시작되지 않습니다.
    set -m

    api_pid=""
    web_pid=""

    # 프로세스 그룹 전체에 신호를 보냅니다. 그룹이 없으면 자식 하나에만 보냅니다.
    signal_servers() {
      local signal="$1" pid
      for pid in "$web_pid" "$api_pid"; do
        [[ -n "$pid" ]] || continue
        if ! kill "-$signal" -- "-$pid" 2>/dev/null; then
          kill "-$signal" "$pid" 2>/dev/null || true
        fi
      done
    }

    servers_alive() {
      local pid
      for pid in "$web_pid" "$api_pid"; do
        [[ -n "$pid" ]] || continue
        if kill -0 "$pid" 2>/dev/null; then
          return 0
        fi
      done
      return 1
    }

    stop_servers() {
      trap - EXIT INT TERM
      signal_servers TERM

      # 기다리는 시간에 상한을 둡니다. 인자 없는 wait를 쓰면 자식 하나가 신호에
      # 응답하지 않을 때 정리가 영영 끝나지 않습니다.
      local attempt
      for attempt in $(seq 1 20); do
        if ! servers_alive; then
          return 0
        fi
        sleep 0.25
      done

      printf '서버가 제때 종료하지 않아 강제로 정리합니다.\n' >&2
      signal_servers KILL
    }
    trap stop_servers EXIT INT TERM

    "$(package_script "$api_package")" run &
    api_pid="$!"

    "$(package_script "$web_package")" run &
    web_pid="$!"

    # 웹이 끝나면 트랩이 API도 정리합니다. wait는 신호로 중단되므로 Ctrl+C가
    # 곧바로 트랩으로 이어집니다.
    wait "$web_pid" || true
    ;;

  run)
    target="${1:-}"
    if [[ "$#" -gt 0 ]]; then
      shift
    fi
    if [[ -z "$target" ]]; then
      printf '실행할 앱을 지정하십시오.\n\n' >&2
      printf '실행 가능한 앱:\n' >&2
      runnable_packages | sed 's/^/  /' >&2
      printf '\n예: services/scripts/run.sh run todo-cli -- list\n' >&2
      exit 2
    fi
    if [[ ! -f "$services_root/$target/$target.cabal" ]]; then
      printf '그런 패키지가 없습니다: %s\n' "$target" >&2
      exit 2
    fi
    if [[ -z "$(executables_of "$target")" ]]; then
      printf '%s는 라이브러리 전용 패키지여서 실행할 수 없습니다.\n\n' "$target" >&2
      printf '실행 가능한 앱:\n' >&2
      runnable_packages | sed 's/^/  /' >&2
      exit 1
    fi
    # `run todo-cli -- 인자` 와 `run todo-cli 인자` 를 모두 허용합니다.
    if [[ "${1:-}" == "--" ]]; then
      shift
    fi
    exec "$(package_script "$target")" run -- "$@"
    ;;

  *)
    printf '알 수 없는 명령입니다: %s\n\n' "$command_name" >&2
    usage >&2
    exit 2
    ;;
esac
