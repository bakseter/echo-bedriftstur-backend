import           SchemaTypes      (Degree (..))
import           Test.Tasty       (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (assertBool, testCase)


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests for converting a Degree to and from a string." [degreeReadShow, degreeShowRead]


degreeReadShow :: TestTree
degreeReadShow =
    testGroup
        "(read . show) applied to any Degree should give us the same Degree back."
        [ testCase "read composed with show is the identity on Degree (DTEK)" $
            assertBool "read composed with show is NOT the identity on Degree (DTEK)" $
                (read . show) DTEK == DTEK
        , testCase "read composed with show is the identity on Degree (DSIK)" $
            assertBool "read composed with show is NOT the identity on Degree (DSIK)" $
                (read . show) DSIK == DSIK
        , testCase "read composed with show is the identity on Degree (DVIT)" $
            assertBool "read composed with show is NOT the identity on Degree (DVIT)" $
                (read . show) DVIT == DVIT
        , testCase "read composed with show is the identity on Degree (BINF)" $
            assertBool "read composed with show is NOT the identity on Degree (BINF)" $
                (read . show) BINF == BINF
        , testCase "read composed with show is the identity on Degree (IMØ)" $
            assertBool "read composed with show is NOT the identity on Degree (IMØ)" $
                (read . show) IMØ == IMØ
        , testCase "read composed with show is the identity on Degree (IKT)" $
            assertBool "read composed with show is NOT the identity on Degree (IKT)" $
                (read . show) IKT == IKT
        , testCase "read composed with show is the identity on Degree (KOGNI)" $
            assertBool "read composed with show is NOT the identity on Degree (KOGNI)" $
                (read . show) KOGNI == KOGNI
        , testCase "read composed with show is the identity on Degree (INF)" $
            assertBool "read composed with show is NOT the identity on Degree (INF)" $
                (read . show) INF == INF
        , testCase "read composed with show is the identity on Degree (PROG)" $
            assertBool "read composed with show is NOT the identity on Degree (PROG)" $
                (read . show) PROG == PROG
        , testCase "read composed with show is the identity on Degree (POST)" $
            assertBool "read composed with show is NOT the identity on Degree (POST)" $
                (read . show) POST == POST
        , testCase "read composed with show is the identity on Degree (MISC)" $
            assertBool "read composed with show is NOT the identity on Degree (MISC)" $
                (read . show) MISC == MISC
        ]


degreeShowRead :: TestTree
degreeShowRead =
    testGroup
        "(show . read) applied to any string corresponding to a Degree should give us the same string back."
        [ testCase "show composed with read is the identity on a valid string (DTEK)" $
            assertBool "show composed with read is NOT the identity on a valid string (DTEK)" $
                (show . (\x -> read x :: Degree)) "DTEK" == "DTEK"
        , testCase "show composed with read is the identity on a valid string (DSIK)" $
            assertBool "show composed with read is NOT the identity on a valid string (DSIK)" $
                (show . (\x -> read x :: Degree)) "DSIK" == "DSIK"
        , testCase "show composed with read is the identity on a valid string (DVIT)" $
            assertBool "show composed with read is NOT the identity on a valid string (DVIT)" $
                (show . (\x -> read x :: Degree)) "DVIT" == "DVIT"
        , testCase "show composed with read is the identity on a valid string (BINF)" $
            assertBool "show composed with read is NOT the identity on a valid string (BINF)" $
                (show . (\x -> read x :: Degree)) "BINF" == "BINF"
        , testCase "show composed with read is the identity on a valid string (IMØ)" $
            assertBool "show composed with read is NOT the identity on a valid string (IMØ)" $
                (show . (\x -> read x :: Degree)) "IMØ" == "IMØ"
        , testCase "show composed with read is the identity on a valid string (IKT)" $
            assertBool "show composed with read is NOT the identity on a valid string (IKT)" $
                (show . (\x -> read x :: Degree)) "IKT" == "IKT"
        , testCase "show composed with read is the identity on a valid string (KOGNI)" $
            assertBool "show composed with read is NOT the identity on a valid string (KOGNI)" $
                (show . (\x -> read x :: Degree)) "KOGNI" == "KOGNI"
        , testCase "show composed with read is the identity on a valid string (INF)" $
            assertBool "show composed with read is NOT the identity on a valid string (INF)" $
                (show . (\x -> read x :: Degree)) "INF" == "INF"
        , testCase "show composed with read is the identity on a valid string (PROG)" $
            assertBool "show composed with read is NOT the identity on a valid string (PROG)" $
                (show . (\x -> read x :: Degree)) "PROG" == "PROG"
        , testCase "show composed with read is the identity on a valid string (POST)" $
            assertBool "show composed with read is NOT the identity on a valid string (POST)" $
                (show . (\x -> read x :: Degree)) "POST" == "POST"
        , testCase "show composed with read is the identity on a valid string (MISC)" $
            assertBool "show composed with read is NOT the identity on a valid string" $
                (show . (\x -> read x :: Degree)) "MISC" == "MISC"
        ]
