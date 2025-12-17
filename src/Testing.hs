-- A stand-alone testing module for Regex parser. Call @Testing.main@ to execute on all
-- test cases.

module Testing where

import Parser      (parseReg)
import Regex       (match)
import Debug.Trace (trace)

main :: IO ()
main = do
  mapM_ (\res -> putStrLn $ show res ++ "\n" ++ sep) tests
  where tests = [testParser, testMatching] -- list of tests yielding a Bool
        sep   = [ '=' | _ <- [1..80]] 

testParser :: Bool 
testParser = process testSuite 1 True
    where
        process []     _ success = success
        process (s:ss) k success = case parseReg s of
            Just reg -> trace (show k ++ ") [OK]\t" ++ show reg) process ss (k+1) success
            Nothing  -> trace (show k ++ ") [FAILED]\t: " ++ s) $ process ss (k+1) False

testMatching :: Bool
testMatching = process testSuiteMatching 1 True
  where
    process [] _ success = success
    process ((pattern,input,expected):ss) k success =
      let res = match pattern input in
        trace ( show k ++ ") " ++
          prefix (res == expected) ++
          "Pattern:" ++ show pattern ++
          "\tinput: " ++ show input)
        $ process ss (k+1) (success && res == expected)
      where prefix b = if b then "[OK]:\t" else "[FAIL]:\t"

testSuite = testSuite1 ++ testSuite2 ++ complexRegexes

testSuiteMatching :: [(String, String, Bool)]
testSuiteMatching = [
    ("",                 "",           True)
  , ("",                 "a",          False)
  , ("a",                "a",           True)
  , ("a",                "b",          False)
  , (".",                "x",           True)
  , (".",                "",           False)
  , ("a*",               "",            True)
  , ("a*",               "aaa",          True)
  , ("a*",               "b",           False)
  , ("a+",               "a",            True)

  , ("a+",               "",           False)
  , ("ab",               "ab",           True)
  , ("ab",               "a",           False)
  , ("a.b",              "acb",          True)
  , ("a.b",              "ab",          False)
  , ("(ab)",             "ab",           True)
  , ("(ab)",             "a",           False)
  , ("(a)b",             "ab",           True)
  , ("a(b)",             "ab",           True)
  , ("(a)(b)",           "ab",           True)

  , ("(ab)*",            "",            True)
  , ("(ab)*",            "abab",         True)
  , ("(ab)*",            "aba",         False)
  , ("(ab)+",            "ab",           True)
  , ("(ab)+",            "",           False)
  , ("a(bc)",            "abc",          True)
  , ("a(bc)",            "ab",          False)
  , ("(a*)(b+)",         "aaabbb",       True)
  , ("(a*)(b+)",         "aaa",         False)
  , ("(a+)(b*)",         "a",            True)

  , ("(a+)(b*)",         "aaabbb",       True)
  , ("(a+)(b*)",         "bbb",         False)
  , ("(a.)",             "ab",           True)
  , ("(a.)",             "a",           False)
  , ("(a.)*",            "abab",         True)
  , ("(a.)*",            "aba",         False)
  , ("(a.)+",            "ab",           True)
  , ("(a.)+",            "",           False)
  , ("(a.b)",            "acb",          True)
  , ("(a.b)",            "ab",          False)

  , ("a(.*)b",           "ab",           True)
  , ("a(.*)b",           "axxxb",        True)
  , ("a(.*)b",           "axxx",        False)
  , ("(.*)",             "",            True)
  , ("(.*)",             "abc",          True)
  , ("(.+)",             "a",            True)
  , ("(.+)",             "",           False)
  , ("(a.*c)",           "ac",           True)
  , ("(a.*c)",           "abbbc",        True)
  , ("(a.*c)",           "abbb",        False)

  , ("(a+b)c",           "aaabc",        True)
  , ("(a+b)c",           "bc",          False)
  , ("a+(bc)",           "aaabc",        True)
  , ("a+(bc)",           "aabc",         True)
  , ("a+(bc)",           "aaab",        False)
  , ("(ab.c)",           "abxc",         True)
  , ("(ab.c)",           "abc",         False)
  , ("(a.*b.*c)",        "axbyc",        True)
  , ("(a.*b.*c)",        "ac",          False)
  , ("(a+b*)(c+)",       "aaabbbccc",    True)

  , ("(a+b*)(c+)",       "ccc",         False)
  , ("((ab)+c)",         "abc",          True)
  , ("((ab)+c)",         "ababc",        True)
  , ("((ab)+c)",         "ab",          False)
  , ("(a(bc)+)",         "abcbc",        True)
  , ("(a(bc)+)",         "abc",          True)
  , ("(a(bc)+)",         "ab",          False)
  , ("((a.)+b)",         "abababb",     True)
  , ("((a.)+b)",         "aabab",       False)
  , ("((a.)+b)",         "aba",         False)

  , ("(a(b(c)))",        "abc",          True)
  , ("(a(b(c)))",        "ab",          False)
  , ("((a*)(bc))",       "bc",           True)
  , ("((a*)(bc))",       "aaabc",        True)
  , ("((a*)(bc))",       "aab",         False)
  , ("(a(b*)c)",         "ac",           True)
  , ("(a(b*)c)",         "abbbc",        True)
  , ("(a(b*)c)",         "abcx",        False)
  , ("((ab)*(c+))",      "ababccc",      True)
  , ("((ab)*(c+))",      "abab",        False)

  , ("(a.(b+))",         "acbbb",        True)
  , ("(a.(b+))",         "ac",          False)
  , ("((a*)(b*)(c*))",   "",             True)
  , ("((a*)(b*)(c*))",   "aaabbbccc",    True)
  , ("((a*)(b*)(c*))",   "aaacccbbb",   False)
  , ("(a(b(c*)))",       "ab",           True)
  , ("(a(b(c*)))",       "abccc",        True)
  , ("(a(b(c*)))",       "ac",          False)
  , ("((a.)*(b+))",      "ababbb",       True)
  , ("((a.)*(b+))",      "aba",         False)

  , ("(((ab)+)*)",       "",             True)
  , ("(((ab)+)*)",       "ababab",       True)
  , ("(((ab)+)*)",       "aba",         False)
  , ("(a+(b(c+)))",      "aabccc",       True)
  , ("(a+(b(c+)))",      "ab",          False)
  , ("((a+)(b+)(c+))",   "aaabbbccc",    True)
  , ("((a+)(b+)(c+))",   "aaabbb",      False)
  , ("((a.*)(b+))",      "axxxbbb",      True)
  , ("((a.*)(b+))",      "axxx",        False)
  , ("(.(.)*)",          "abc",          True) ]

testSuite1 :: [String]
testSuite1 = 
            [ ""
            , "a"
            , "b"
            , "c"
            , "."
            , "ε"
            , "a*"
            , "b+"
            , "c*"
            , ".+"
            , "a+"
            , "(a)"
            , "(b*)"
            , "(c+)"
            , "(.)"
            , "()"
            , "aε"
            , "εa"
            , "a.b"
            , "(a.b)"
            , "(a)(b)"
            , "(a*)(b+)"
            , "(a+)(b*)"
            , "(a*)(b*)(c*)"
            , "a(bc)"
            , "(ab)c"
            , "(a)(bc)"
            , "((ab)c)"
            , "(a(b)c)"
            , "((a)(b)(c))"
            , "abc"
            , "a(bc)+"
            , "(ab)+"
            , "(ab)*"
            , "((ab)+)*"
            , "a(b+)"
            , "(a*)(bc*)"
            , "a.(b)"
            , "(a.)+"
            , ".a*"
            , "(..)"
            , "(..)*"
            , "(.a)*"
            , "a(.b)"
            , "(a.b+)"
            , "(.b)*"
            , "(.b+)"
            , "((a.b))"
            , "(a.(b))"
            , "a(b.)c"
            , "((a(b.)c))"
            , "a((bc)*)"
            , "(a((bc)*))"
            , "(a(bc)*)"
            , "(a(bc)+)"
            , "((a*)(b+))"
            , "a*(bc)"
            , "a*(bc)+"
            , "(ab)(cd)"
            , "(ab)(cd+)"
            , "((ab)(cd*))"
            , "((ab)*)(cd)"
            , "(a(bc)d)"
            , "(a(b(c)))"
            , "(a(b(c+)))"
            , "((a.(b.c)))"
            , "(a(b.(c)))"
            , "(((a.b).c))"
            , "(a*(b.c))"
            , "((a*b*)c)"
            , "(a(bc)*d)"
            , "(a(bc)+d)"
            , "(a.(b.(c)))"
            , "(a.(b.(c*)))"
            , "(a.(b.(c+)))"
            , "((a.b).(c*))"
            , "((a*)(b.c+))"
            , "a*b.c+"
            , "(((a*)b)c)"
            , "(a((b*)c))"
            , "(a(b(c)d))"
            , "((a)(bc))(d)"
            , "(a)((bc)(d+))"
            , "(ab)(c+)(d*)"
            , "(a.(b*c))"
            , "(a.(b+)c)"
            , "((a.(b+)(c)))"
            , "(a(b+)).(c)"
            , "((a(b+)).(c))"
            , "(a(bc.)*)"
            , "(a.(bc.)*)"
            , "((a.(bc.)*))"
            , "(a+)(b+)(c+)"
            , "(a+b+c+)"
            , "a+(bc)+"
            , "(a+(bc)+)"
            , "((a+)(bc)+)"
            , "(a.b.c)+"
            ]

testSuite2 :: [String]
testSuite2 =
  [ "a*a"
  , "b*b"
  , "c*c"
  , "d+d"
  , "e*e"
  , "f+f"
  , "g*g"
  , "h+h"
  , "i*i"
  , "j+j"
  , "k*k"
  , "l+l"
  , "m*m"
  , "n+n"
  , "o*o"
  , "p+p"
  , "q*q"
  , "r+r"
  , "s*s"
  , "t+t"
  , "u*u"
  , "v+v"
  , "w*w"
  , "x+x"
  , "y*y"
  , "z+z"
  , "a*b*c"
  , "a*b+c"
  , "a*bc+"
  , "(ab)*c"
  , "a(bc)*"
  , "a(bc)+"
  , "(ab)+(cd)"
  , "(ab)(cd+)"
  , "(ab*)(cd*)"
  , "(a*b)(c*d)"
  , "a(b)c(d)"
  , "a(bc)d+"
  , "(a)(b)(c)(d)"
  , "(a*)(b+)(c*)"
  , "(a*)(b*)(c*)(d*)"
  , "(a+)(b*)(c*)(d+)"
  , "a(bc)(de)"
  , "(ab)(cde)"
  , "a(b(cd))"
  , "(a(b(cd)))"
  , "((a)b)c+"
  , "((a*)(b+))c"
  , "(a(b(c)))"
  , "a(b(c(d)))"
  , "(a(b(c(d+))))"
  , "(a.(b.(c)))"
  , "(a.(b.(c*)))"
  , "(a.(b.(c+)))"
  , "((a*b)c)d"
  , "(a(b(c*)d))"
  , "(a(b(c+)d))"
  , "(ab)(c)(d)(e)"
  , "(a)(bc)(d*)(e+)"
  , "((a)(bc))(d*)(e*)"
  , "(a.(b*c))d"
  , "(a.(b+)c)d"
  , "(a.(b+)(c))d"
  , "((a.(b+)(c)))d"
  , "a(b+).(c)"
  , "(a(b+)).(c)"
  , "(a(b+)).(c+)"
  , "(a(bc.)*)d"
  , "(a.(bc.)*)e"
  , "((a.(bc.)*))f"
  , "(a+)(b+)(c+)(d+)"
  , "a+(bc)+d+"
  , "(a+(bc)+)d"
  , "((a+)(bc)+)d"
  , "(a.b.c)+d"
  , "(a.b)*(c.d)"
  , "((a.b)*)(c.d)"
  , "(a.(b*)).(c*)"
  , "(a.(b+)).(c+)"
  , "(a(b(c(d+)))).e"
  , "(a(b(c(d*))))f"
  , "a(b(c(d*)))"
  , "(ab)*(bc)*(cd)"
  , "(ab)+(bc)+(cd+)"
  , "(ab*)(bc+)(cd*)"
  , "(a*)(b*)(c*)(d*)(e*)"
  , "(a+)(b+)(c+)(d+)(e+)"
  , "a(bcdef)+"
  , "(abcdef)+"
  , "(abc*)(def+)"
  , "a(bc*)(de+)"
  , "a.(b.c.d.e)"
  , "(a.b.c.d)+"
  , "((a.b).(c.d))"
  , "(a.(b.(c.(d))))"
  , "(a*(b*(c*(d*))))"
  , "(a+(b+(c+(d+))))"
  , "εa+"
  , "aε*"
  , "()*"
  , "()+"
  , "a()b"
  , "(aε)b+"
  , "(a)()(b)(c*)"
  , "((a)ε(b))"
  , "(a(εc))"
  , "(a(bεc))"
  , "(a(b(εc)))"
  , "a(b(ε(c)))"
  , "(a.b.ε)+"
  , "(ε.*)+"
  , "(a*ε*)"
  , "(a+ε+)"
  , "a*()b*"
  , "(abεcd)+"
  , "(aεbεc)*"
  , "((aε)(bε))"
  , "(a()b(c*))"
  , "((((a))))"
  , "((((a*))))"
  , "((((a+))))"
  , "(a(b(c(d(e)))))"
  , "(a(b(c(d(e*)))))"
  , "(a(b(c(d(e+)))))"
  , "a(bc)(de)(fg)"
  , "(ab)(cd)(ef)(gh)"
  , "(ab)*(cd)*(ef*)"
  , "(ab)+(cd+)(ef)"
  , "a(bc+)(de*)"
  , "(ab)(bc+)(cd*)"
  , "((ab)(bc))(cd)"
  , "(a.(b.c))*"
  , "(a.(b.c))+"
  , "((a.(b.c)))+"
  , "a(b.(c.d))"
  , "(a.(b.(c.d)))"
  , "(a.(b.(c.(d))))"
  , "(a*b*c*d*)"
  , "(a+b+c+d+)"
  , "a*(b+(c*(d+)))"
  , "(a*(b+(c*(d+))))"
  , "(a+)(b*)(c+)(d*)"
  , "a.(b+).c*"
  , "(a.(b+).c*)"
  , "(a.(b+).c*)+"
  , "((a.(b+).c*))"
  , "((a.(b+).c*))+"
  , "(a.(b.c))*"
  , "(a.(b.c))+"
  , "(ab)(bc)*(cd)(de)+"
  , "a(bc)*(cd)*(de)+"
  , "((a(bc)*(cd)*(de)+))"
  , "(a(b(c(d(e(f))))))"
  , "(a(b(c(d(e(f*))))))"
  , "(a(b(c(d(e(f+))))))"
  , "(.*a.*)"
  , "(a.*b*)"
  , "(a.+b+)"
  , "(a*.b+)"
  , "(a+.b*)"
  , "(a+(bc*)d+)"
  , "(a*(bc+)d*)"
  , "(a*(b+(c*(d+(e)))))"
  , "a(bcdefg)*"
  , "a(bcdefg)+"
  , "(abcdefg)*"
  , "(abcdefg)+"
  , "(a*)(bc+)(d*)(ef*)"
  , "(a+)(bc*)(d+)(ef+)"
  , "(a.(b.c.d))"
  , "(a.(b.c.d))*"
  , "(a.(b.c.d))+"
  , "((a.(b.c.d)))"
  , "((a.(b.c.d)))+"
  , "a(b(c(d(e(f(g))))))"
  , "(a(b(c(d(e(f(g*)))))))"
  , "(a(b(c(d(e(f(g+)))))))"
  , "(ab)c*(de)f+"
  , "(ab+)c*(de*)f"
  , "(a(bc)*d(e+))"
  , "((a(bc)*d(e+)))"
  , "((a(bc)*(d(e+))))"
  , "(a+)(b*)(c*)(d+)(e*)(f*)"
  , "a(bc)(de)*f+"
  , "(ab)*(cd)+(ef*)"
  , "(ab)*(cd)*(ef*)"
  , "(ab+)(cd*)(ef+)"
  , "(ab*)(cd+)(ef*)"
  , "(a*(b*(c*(d*(e*)))))"
  , "(a+(b+(c+(d+(e+)))))"
  , "((a*b*c)d)e"
  , "a*((bc)+)d"
  , "(a*((bc)+)d)"
  , "((a*((bc)+)d))"
  , "(a.(b.(c.(d.(e.(f))))))"
  , "((a.(b.(c.(d.(e.(f)))))))"
  , "a*(b*(c*(d*(e*(f*(g*))))))"
  , "(a+(b+(c+(d+(e+(f+(g+)))))))"
  , "ε"
  , "ε*"
  , "ε+"
  , "()a(bc)"
  , "(aε)(bε)"
  , "((aε)b)+"
  , "(aε)*"
  , "(aε)+"
  , "aεbεcε"
  , "(aεbεcε)*"
  , "((aε)(bε)(cε))"
  , "(εa*)(εb+)"
  , "a*(b*ε)c+"
  , "(a*(b*ε)c+)"
  , "(a*(b*ε)c+)*"
  , "(abc*)(εde+)"
  , "((abc*)(εde+))"
  , "a.()b*(c+)"
  , "((a.()b*(c+)))"
  , "(a.b.c.d.e.f)"
  , "((a.b.c.d.e.f))"
  , "((a.b.c.d.e.f))*"
  , "((a.b.c.d.e.f))+"
  , "(a(bc)+d(e*))(f+)"
  , "(a*(bc)d*(ef))"
  , "(a+(bc)+d+(ef)*)"
  , "(ab*c*d+e*f+)"
  , "(abcd)*(efgh)+"
  , "(abc*d+)(ef*g+)"
  , "(a.(b.c))*"
  , "(a.(b.c.d))*"
  , "(a.(b.(c.(d.(e.(f))))))"
  , "(a*(b.(c*(d.(e*(f))))))"
  , "(a+(b.(c+(d.(e+(f))))))"
  , "a(bcdefgh)*"
  , "a(bcdefgh)+"
  , "(abcdefgh)*"
  , "(abcdefgh)+"
  , "(a*)(b+)(c*)(d+)(e*)(f+)"
  , "((a*)(b+)(c*)(d+)(e*)(f+))"
  , "((a*)(b+)(c*)(d+)(e*)(f+))*"
  ]

complexRegexes :: [String]
complexRegexes =
    [
    "(a*(bc+(d.(e*f+))))(a*(bc+(d.(e*f+))))()(a+b*c*(d+))(a.(b.(c.(d.(e*(f+))))))"
  , "(a*(b*(c+(d*(e+(f*(g*(h+))))))))(bc*(de+))(a.(b.(c.(d.(e.(f.(g)))))))()(abc+)*"
  , "((a.b.c.d.e.f.g.h)+)(a*(b*(c*(d*(e*(f*(g*(h*(i+)))))))))(abc*(def+))()(a+)(b*)(c*)"
  , "(a(bc*(d+(e.(f*(g+))))))(a*(b.(c.(d.(e.(f.(g.(h))))))))(abc*(de+f*))(g+h*)*()a*b*c+"
  , "((a*(b+)(c*)(d*(e+))))(f.(g.(h.(i.(j.(k*(l+)))))))(a*b*c*d*)(e+f*)(g*(h+))()"
  ]
