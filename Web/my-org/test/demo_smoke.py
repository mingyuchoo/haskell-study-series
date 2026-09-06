"""Verify atomic demo initialization against a fresh, disposable local server."""
import concurrent.futures
import json
import sys
import urllib.error
import urllib.request

BASE = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:18082"


def call(path, body=None):
    request = urllib.request.Request(
        BASE + "/api/" + path,
        data=None if body is None else json.dumps(body).encode(),
        headers={"Content-Type": "application/json"},
    )
    try:
        response = urllib.request.urlopen(request)
    except urllib.error.HTTPError as error:
        response = error
    return response.status, json.load(response)


with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
    responses = list(pool.map(lambda _: call("demo", {}), range(4)))
assert sorted(status for status, _ in responses) == [201, 409, 409, 409], responses
status, state = call("dashboard")
assert status == 200
assert len(state["people"]) >= 6 and len(state["goals"]) >= 7
active = [entry for entry in state["goals"] if entry["active"]]
assert {entry["evaluation"]["status"] for entry in active} == {
    "NoData", "OnTrack", "AtRisk", "OffTrack", "Achieved"
}
assert len(state["goals"]) - len(active) >= 2
assert any(entry.get("owner") is None for entry in state["goals"])
assert {"O001", "O017", "O031", "O040"} <= {
    diagnostic["code"] for diagnostic in state["compiler"]["diagnostics"]
}
assert {"Owns", "DependsOn", "Controls", "Measures"} <= {
    edge["kind"] for edge in state["graph"]["edges"]
}
assert len(state["reviews"]) >= 2
assert any(review["learnings"] and review["decisions"] for review in state["reviews"])
status, events = call("events")
assert status == 200 and len(events) > 20
assert [event["seq"] for event in events] == list(range(1, len(events) + 1))
assert len({event["at"] for event in events}) == 1
assert call("demo", {})[0] == 409
assert call("events")[1] == events, "Rejected re-seed changed events"
for path in ["organization", "people", "goals", "compiler", "graph", "reviews"]:
    assert call(path)[0] == 200
print(f"PASS: concurrent seed once, {len(events)} audit events, five statuses, diagnostics/graph/reviews, immutable repeat")
