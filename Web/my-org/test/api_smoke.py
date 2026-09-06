"""Run against a fresh local server: python3 test/api_smoke.py http://127.0.0.1:18080."""
import json
import sys
import urllib.error
import urllib.request

BASE = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:18080"
checks = 0


def call(path, body=None, expected=200):
    global checks
    request = urllib.request.Request(
        BASE + "/api/" + path,
        data=None if body is None else json.dumps(body).encode(),
        headers={"Content-Type": "application/json"},
    )
    try:
        response = urllib.request.urlopen(request)
    except urllib.error.HTTPError as error:
        response = error
    payload = json.load(response)
    assert response.status == expected, (path, response.status, expected, payload)
    checks += 1
    return payload


call("organizations", {"id": "org", "name": "HTTP 테스트 조직"}, 201)
call("organizations", {"id": "org", "name": "중복"}, 409)
call("people", {"id": "owner", "name": "<img src=x onerror=alert(1)>", "role": "영업"}, 201)
goal = {
    "id": "revenue", "organization": "org", "description": "매출 성장",
    "metric": {"id": "sales", "name": "매출", "unit": "KRW", "direction": "HigherIsBetter"},
    "baseline": 100, "target": 200, "startsAt": "2026-01-01T00:00:00Z",
    "deadline": "2027-01-01T00:00:00Z", "requiredPermissions": ["Pricing"], "requiredBudget": 100,
}
call("goals", goal, 201)
call("goals", goal, 409)
call("goals/revenue/activate", {}, 400)
call("goals/revenue/owner", {"owner": "missing"}, 404)
call("goals/revenue/owner", {"owner": "owner"}, 201)
call("goals/revenue/activate", {}, 400)
authority = {"owner": "owner", "budgetLimit": 100, "canHire": False, "canChangePrice": True, "canApprove": []}
call("people/owner/authority", authority, 201)
call("goals/revenue/activate", {"actor": "missing"}, 404)
call("goals/revenue/activate", {"actor": "owner"}, 201)
call("goals/revenue/activate", {}, 409)
call("goals/revenue/results", {"value": 200, "reportedBy": "owner", "note": "달성", "actor": "owner"}, 201)
call("evaluations", {"goal": "revenue"}, 201)
call("reviews", {"id": "review1", "goal": "revenue", "note": "학습 없는 회고"}, 201)
call("reviews", {"id": "badreview", "goal": "revenue", "note": "잘못된 결정", "decisions": [{"text": "실험", "owner": "missing"}]}, 404)
call("reviews", {"id": "review2", "goal": "revenue", "note": "다음 실험", "learnings": [{"text": "가격 권한 위임이 효과적"}], "decisions": [{"text": "가격 실험", "owner": "owner", "deadline": "2027-02-01T00:00:00Z"}]}, 201)
call("goals/revenue/strategy", {"note": "기업 고객 확대"}, 201)
state = call("dashboard")
assert state["goals"][0]["evaluation"]["status"] == "Achieved", state
assert len(state["reviews"]) == 2
assert "O040" in [d["code"] for d in state["compiler"]["diagnostics"]]
assert state["people"][0]["name"] == "<img src=x onerror=alert(1)>"
for path in ["organization", "people", "goals", "compiler", "graph", "events", "reviews"]:
    call(path)
events = call("events")
assert [e["seq"] for e in events] == list(range(1, len(events) + 1))
assert any(e.get("actor") == "owner" for e in events)
call("people/owner/authority", dict(authority, canChangePrice=False), 201)
assert call("goals")[0]["active"] is False
call("goals/revenue/results", {"value": 200, "reportedBy": "owner", "note": "권한 회수 후"}, 409)
call("people/owner/authority", authority, 201)
call("goals/revenue/activate", {}, 201)
call("missing", expected=404)
print(f"PASS: {checks} HTTP checks; goal → owner/authority → result → evaluation → review and authority revocation")
