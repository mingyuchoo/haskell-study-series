"""Deletion API regression: ONLY run against a fresh disposable test server."""
import json
import sys
import urllib.error
import urllib.request

BASE = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:18083"


def call(path, body=None, method=None, expected=200):
    request = urllib.request.Request(
        BASE + "/api/" + path,
        data=None if body is None else json.dumps(body).encode(),
        headers={"Content-Type": "application/json"}, method=method,
    )
    try:
        response = urllib.request.urlopen(request)
    except urllib.error.HTTPError as error:
        response = error
    payload = json.load(response)
    assert response.status == expected, (path, response.status, expected, payload)
    return payload


def delete(identifier, name, version, expected=200):
    return call("organizations/" + identifier,
                {"confirmName": name, "expectedVersion": version}, "DELETE", expected)


initial = call("dashboard")
assert initial["organization"] is None and initial["version"] == 0, "Refusing non-empty server"
call("demo", {}, expected=201)
state = call("dashboard")
org = state["organization"]
version = state["version"]
old_events = call("events")
old_people = call("people")
delete("missing", org["name"], version, 404)
delete(org["id"], "wrong", version, 400)
delete(org["id"], org["name"], version - 1, 409)
call("organizations/" + org["id"], {"confirmName": org["name"]}, "DELETE", 400)
assert call("events") == old_events
call("people", {"id": "deletion-test", "name": "삭제 검증 가상 인물", "role": "검증"}, expected=201)
delete(org["id"], org["name"], version, 409)
version = call("dashboard")["version"]
delete(org["id"], org["name"], version)
empty = call("dashboard")
assert empty["version"] == version + 1 and empty["organization"] is None
for key in ["people", "goals", "authorities", "reviews", "events"]:
    assert empty[key] == [], (key, empty[key])
for path in ["people", "goals", "events", "reviews"]:
    assert call(path) == []
assert call("graph")["nodes"] == [] and call("graph")["edges"] == []
assert call("organization") is None
delete(org["id"], org["name"], empty["version"], 404)
call("organizations", {"id": org["id"], "name": org["name"]}, expected=201)
new = call("dashboard")
assert new["demo"] is False
assert new["people"] == [] and new["goals"] == [] and new["reviews"] == []
assert len(call("events")) == 1
assert call("events")[0]["seq"] == empty["version"] + 1
delete(org["id"], org["name"], version, 409)
delete(org["id"], org["name"], new["version"])
call("demo", {}, expected=201)
seeded = call("dashboard")
assert seeded["demo"] and len(seeded["goals"]) == 7
assert call("events")[0]["seq"] > new["version"]
assert call("people") == old_people
print("PASS: wrong target/name/revision, delete projection isolation, same-ID recreation, non-demo provenance, reseed monotonic sequence")
