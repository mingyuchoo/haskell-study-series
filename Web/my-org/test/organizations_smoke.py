"""Multi-organization API checks. Requires a fresh disposable server."""
import json
import sys
import urllib.error
import urllib.request

BASE = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:18086"
checks = 0


def call(path, body=None, method=None, expected=200):
    global checks
    request = urllib.request.Request(BASE + "/api/" + path,
        data=None if body is None else json.dumps(body).encode(), method=method,
        headers={"Content-Type": "application/json"})
    try:
        response = urllib.request.urlopen(request)
    except urllib.error.HTTPError as error:
        response = error
    payload = json.load(response)
    assert response.status == expected, (path, response.status, expected, payload)
    checks += 1
    return payload


def stable(state):
    for entry in state.get("goals", []):
        entry.pop("evaluation", None)
    return state


def org(oid, suffix=""):
    return "organizations/" + oid + ("/" + suffix if suffix else "")


assert call("organizations") == [], "Refusing to use non-empty test server"
call("organizations", {"id": "a", "name": "A 가상 조직"}, expected=201)
assert call("dashboard")["organization"]["id"] == "a"
call("organizations", {"id": "b", "name": "B 가상 조직"}, expected=201)
assert len(call("organizations")) == 2
call("organizations", {"id": "a", "name": "중복"}, expected=409)
for path in ["dashboard", "people", "goals", "events", "reviews", "compiler", "graph"]:
    call(path, expected=409)
call("people", {"id": "ambiguous", "name": "모호함", "role": "역할"}, expected=409)
a = call(org("a"))
b_before = call(org("b", "dashboard"))
call(org("b", "people"), {"id": "b-only", "name": "B 전용", "role": "역할"}, expected=201)
call(org("a"), {"name": "A 새 이름", "expectedVersion": a["version"]}, "PATCH")
call(org("a"), {"name": "오래된 이름", "expectedVersion": a["version"]}, "PATCH", 409)
assert call(org("a"))["organization"]["name"] == "A 새 이름"
call(org("a", "people"), {"id": "new", "name": "새 사람", "role": "역할", "reportsTo": "b-only"}, expected=404)
call(org("a", "people"), {"id": "new", "name": "새 사람", "role": "역할", "actor": "b-only"}, expected=404)
for oid in ["a", "b"]:
    call(org(oid, "people"), {"id": "same", "name": oid + " 담당자", "role": "역할"}, expected=201)
    goal = {"id": "same-goal", "organization": oid, "description": oid + " 목표",
        "metric": {"id": "metric", "name": "지표", "unit": "점", "direction": "HigherIsBetter"},
        "baseline": 0, "target": 100, "startsAt": "2026-01-01T00:00:00Z",
        "deadline": "2027-01-01T00:00:00Z", "requiredPermissions": [], "requiredBudget": 0}
    call(org(oid, "goals"), goal, expected=201)
    call(org(oid, "people/same/authority"), {"owner": "same", "budgetLimit": 0, "canHire": False, "canChangePrice": False, "canApprove": []}, expected=201)
    call(org(oid, "goals/same-goal/owner"), {"owner": "same"}, expected=201)
    call(org(oid, "goals/same-goal/activate"), {}, expected=201)
    call(org(oid, "goals/same-goal/results"), {"value": 20 if oid == "a" else 80, "reportedBy": "same", "note": "결과"}, expected=201)
    call(org(oid, "evaluations"), {"goal": "same-goal"}, expected=201)
    call(org(oid, "reviews"), {"id": "same-review", "goal": "same-goal", "note": "회고", "learnings": [{"text": oid + " 학습"}]}, expected=201)
    for path in ["dashboard", "people", "goals", "events", "reviews", "compiler", "graph"]:
        call(org(oid, path))
assert call(org("a", "goals"))[0]["evaluation"]["latestValue"] == 20
assert call(org("b", "goals"))[0]["evaluation"]["latestValue"] == 80
call(org("a", "goals"), dict(goal, id="wrong", organization="b"), expected=400)
call(org("a", "goals/same-goal/owner"), {"owner": "b-only"}, expected=404)
call(org("a", "goals/same-goal/results"), {"value": 2, "reportedBy": "b-only", "note": "결과"}, expected=404)
call(org("a", "reviews"), {"id": "invalid", "goal": "same-goal", "note": "회고", "decisions": [{"text": "결정", "owner": "b-only"}]}, expected=404)
b = stable(call(org("b", "dashboard")))
a = call(org("a"))
call(org("a"), {"confirmName": "A 새 이름", "expectedVersion": a["version"]}, "DELETE")
assert stable(call(org("b", "dashboard"))) == b
call(org("a"), expected=404)
assert len(call("organizations")) == 1
call("organizations", {"id": "a", "name": "A 재생성"}, expected=201)
assert call(org("a", "people")) == [] and len(call(org("a", "events"))) == 1
call(org("a"), {"confirmName": "A 재생성", "expectedVersion": a["version"]}, "DELETE", 409)
call("demo", {}, expected=201)
assert stable(call(org("b", "dashboard"))) == b
call("demo", {}, expected=409)
assert len(call("organizations")) == 3
print(f"PASS: {checks} scoped CRUD/isolation/version/demo API checks")
