module Test.Generated.Main exposing (main)

import ActivityTest
import AgentTest
import AppFixture
import DecoderTest
import DiscoveryPageTest
import DiscoveryTest
import FormTest
import GraphFixture
import ListViewTest
import PageTest
import PeopleTest
import ResponsibilityGraphTest
import StateTest

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    Test.Runner.Node.run
        { runs = 100
        , report = ConsoleReport Monochrome
        , seed = 52605835732413
        , processes = 2
        , globs =
            []
        , paths =
            [ "/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/ActivityTest.elm"
            , "/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/AgentTest.elm"
            , "/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/AppFixture.elm"
            , "/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/DecoderTest.elm"
            , "/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/DiscoveryPageTest.elm"
            , "/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/DiscoveryTest.elm"
            , "/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/FormTest.elm"
            , "/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/GraphFixture.elm"
            , "/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/ListViewTest.elm"
            , "/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/PageTest.elm"
            , "/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/PeopleTest.elm"
            , "/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/ResponsibilityGraphTest.elm"
            , "/Users/a81720/github/mingyuchoo/study-series-for-haskell/Web/my-org/static/tests/StateTest.elm"
            ]
        }
        [ ( "ActivityTest"
          , [ Test.Runner.Node.check ActivityTest.tests
            ]
          )
        , ( "AgentTest"
          , [ Test.Runner.Node.check AgentTest.tests
            ]
          )
        , ( "AppFixture"
          , [ Test.Runner.Node.check AppFixture.mapPage
            , Test.Runner.Node.check AppFixture.mapSession
            ]
          )
        , ( "DecoderTest"
          , [ Test.Runner.Node.check DecoderTest.tests
            ]
          )
        , ( "DiscoveryPageTest"
          , [ Test.Runner.Node.check DiscoveryPageTest.tests
            ]
          )
        , ( "DiscoveryTest"
          , [ Test.Runner.Node.check DiscoveryTest.tests
            ]
          )
        , ( "FormTest"
          , [ Test.Runner.Node.check FormTest.tests
            ]
          )
        , ( "GraphFixture"
          , [ Test.Runner.Node.check GraphFixture.edge
            , Test.Runner.Node.check GraphFixture.goal
            , Test.Runner.Node.check GraphFixture.node
            , Test.Runner.Node.check GraphFixture.sample
            , Test.Runner.Node.check GraphFixture.workspace
            ]
          )
        , ( "ListViewTest"
          , [ Test.Runner.Node.check ListViewTest.ready
            , Test.Runner.Node.check ListViewTest.sample
            , Test.Runner.Node.check ListViewTest.step
            , Test.Runner.Node.check ListViewTest.tests
            ]
          )
        , ( "PageTest"
          , [ Test.Runner.Node.check PageTest.tests
            ]
          )
        , ( "PeopleTest"
          , [ Test.Runner.Node.check PeopleTest.tests
            ]
          )
        , ( "ResponsibilityGraphTest"
          , [ Test.Runner.Node.check ResponsibilityGraphTest.tests
            ]
          )
        , ( "StateTest"
          , [ Test.Runner.Node.check StateTest.tests
            ]
          )
        ]