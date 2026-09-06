"""Exercise demo startup only in a disposable cwd, preserving any live 8081 server."""
import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import tempfile
import time
import urllib.request

BINARY = str(Path(sys.argv[1]).resolve())
ROOT = Path(__file__).resolve().parent.parent
PORT = 18084


def request(path, body=None, method=None):
    req = urllib.request.Request(
        f"http://127.0.0.1:{PORT}/api/" + path,
        data=None if body is None else json.dumps(body).encode(), method=method,
        headers={"Content-Type": "application/json"},
    )
    with urllib.request.urlopen(req) as response:
        return json.load(response)


def stop(process):
    if process.poll() is None:
        process.send_signal(signal.SIGINT)
        process.wait(timeout=10)


with tempfile.TemporaryDirectory(prefix="my-org-delete-startup-") as directory:
    staging = Path(directory)
    (staging / "static").symlink_to(ROOT / "static", target_is_directory=True)
    event_file = staging / "runs/demo/events.json"
    env = dict(os.environ)
    env.pop("MY_ORG_TEST_DATABASE_URL", None)
    env.pop("MY_ORG_DEMO", None)
    env.update(MY_ORG_EVENT_FILE=str(event_file), MY_ORG_PORT=str(PORT))

    def bootstrap():
        demo_env = dict(env, MY_ORG_DEMO="1")
        process = subprocess.Popen([BINARY], cwd=staging, env=demo_env,
                                   stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        try:
            try:
                output, _ = process.communicate(timeout=3)
            except subprocess.TimeoutExpired:
                stop(process)
                output, _ = process.communicate()
            # Occupied 8081 is expected only AFTER the startup/store branch succeeds.
            assert "Address already in use" in output or process.returncode in (0, -signal.SIGINT), output
            assert "refusing" not in output.lower() and "initialization failed" not in output.lower(), output
            return output
        finally:
            stop(process)

    def serve(action):
        process = subprocess.Popen([BINARY], cwd=staging, env=env,
                                   stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
        try:
            for _ in range(50):
                try:
                    request("dashboard")
                    break
                except OSError:
                    if process.poll() is not None:
                        raise RuntimeError(process.stderr.read().decode())
                    time.sleep(0.05)
            else:
                raise AssertionError("Test server did not start")
            return action()
        finally:
            stop(process)

    bootstrap()
    seeded = json.loads(event_file.read_bytes())
    assert len(seeded) > 50

    def delete_demo():
        state = request("dashboard")
        org = state["organization"]
        request("organizations/" + org["id"],
                {"confirmName": org["name"], "expectedVersion": state["version"]}, "DELETE")
        assert request("dashboard")["organization"] is None

    serve(delete_demo)
    deleted_bytes = event_file.read_bytes()
    bootstrap()
    assert event_file.read_bytes() == deleted_bytes, "Demo was revived on startup"

    def create_regular():
        assert request("dashboard")["organization"] is None
        request("organizations", {"id": "new-local-org", "name": "가상의 재생성 조직"})
        request("people", {"id": "new-person", "name": "새 가상 인물", "role": "담당자"})

    serve(create_regular)
    recreated_bytes = event_file.read_bytes()
    bootstrap()
    assert event_file.read_bytes() == recreated_bytes, "Recreated organization changed on startup"
    state = serve(lambda: request("dashboard"))
    assert state["organization"]["id"] == "new-local-org" and len(state["people"]) == 1
    assert state["demo"] is False
    event_file.rename(staging / "previous-lifecycle-audit.json")
    serve(lambda: request("organizations", {"id": "unrelated", "name": "기존 별도 가상 조직"}))
    unrelated_bytes = event_file.read_bytes()
    rejected = subprocess.run([BINARY], cwd=staging, env=dict(env, MY_ORG_DEMO="1"),
                              stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, timeout=10)
    assert rejected.returncode != 0 and "refusing to change" in rejected.stdout, rejected.stdout
    assert event_file.read_bytes() == unrelated_bytes
    print("PASS unrelated existing workspace refused without mutation")
    print("PASS isolated startup branches: initial seed, no revival after deletion, regular replacement preserved")
    print("8081 binding may conflict with the live preview; HTTP lifecycle checks ran separately on 18084")
