#!/usr/bin/env bash
# Run manually from repository root only when intentionally changing the wire contract.
# Tests never regenerate fixtures. Review every wire change before accepting it.
set -euo pipefail
fixture_generator=$(mktemp --suffix=.hs)
trap 'rm -f "$fixture_generator"' EXIT
cat > "$fixture_generator" <<'HS'
import ContractSpec (fixtureTime, normalizeDashboard, withFixtureServer)
import MyOrg.Serialization.JSON (encodeWire)
import qualified Data.ByteString.Lazy as BL
import MyOrg.Demo (demoOrganizationId, legacyDemoEvents)
import MyOrg.Domain.Event
import SmokeSupport (get)
main :: IO ()
main = do
  -- Fixtures freeze the historical 52-event seed; the survey appended by the current demo is a separate contract.
  events <- either (fail . show) pure (legacyDemoEvents fixtureTime)
  BL.writeFile "test/fixtures/legacy-events.json" (encodeWire events)
  BL.writeFile "test/fixtures/scoped-events.json" (encodeWire (map (\e -> e {storedEvent = OrganizationScoped demoOrganizationId (storedEvent e)}) events))
  withFixtureServer "legacy-events.json" $ \client -> do
    dashboard <- get client "organizations/demo-northstar-v2/dashboard"
    BL.writeFile "test/fixtures/dashboard.json" (encodeWire (normalizeDashboard dashboard))
HS
stack exec -- runghc -XOverloadedStrings -itest "$fixture_generator"
