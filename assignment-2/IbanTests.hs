module IbanTests where

import System.Random
import Data.Char

import Iban
import Lecture2Test
import ListHelper
import RandomHelper
import StringHelper

{-
IBAN specification: http://www.europeanpaymentscouncil.eu/documents/ECBS%20IBAN%20standard%20EBS204_V3.2.pdf

The IBAN specification says that validation of an IBAN account identifier is
performed by check digits. These digits are present in every identifier on the
third and fourth place of the string.

The algorithm for validation by check digits is pretty simple, some
elementary string transformation operations are performed to convert the
IBAN string into a large integer and the resulting integer should be congruent
to 1 (mod 97).

This is our first requirement:

1. After the elementary string transformation operations are performed as
described in chapter 6.1 of the specification the resulting integer should
be congruent to 1 (mod 97).

However, the fact that these elementary string operations succeed followed
by the validation that the resulting number is congruent to 1 (mod 97)
does not in itself prove that a given string is a valid IBAN identifier,
for instance, the string:

"9798 9797"

is valid according to the procedure but it is not a valid IBAN identifier
because it does not begin with an ISO country code. So is the string:

"98"

even though it only consists of the check digits.

So in addition our validation procedure should validate that:

2. The first two alphanumeric characters constitute a valid ISO country code.
3. The third and fourth alphanumeric characters are digits.

The above 2 requirements also imply that the procedure should validate that:

4. The number of alphanumeric characters in the string is at least 4.

Further validation looks difficult because it would require validating that what
follows the first 4 characters is a valid local bank account identifier.
The formats of these local bank account identifiers are unknown to us.

We use a known list of good identifiers to test that our validation
procedure succesfully validates some good ones:
http://www.rbs.co.uk/corporate/international/g0/guide-to-international-business/regulatory-information/iban/iban-example.ashx

The test case for this is called `testGoodIban`.

The other test case `testBadIban` chooses randomly among one of the
following cases:

1. Take a known good IBAN identifier and increment the second check digit
(mod 10), resulting in an incongruence to 1 (mod 97).

2. Take between 1 and 3 (inclusive) alphanumeric characters from the front of a
known good IBAN identifier.

3. Take a known good IBAN identifier and substitute the first two alphanumeric
characters with two random alphanumeric characters that do not form a valid ISO
country code.

4. Take a known good IBAN identifier and substitute the third digit with a
random non-numeric character.

5. Take a known good IBAN identifier and substitute the fourth digit with a
random non-numeric character.
-}

testAllIbans :: IO ()
testAllIbans = sequence_ [testGoodIban, testBadIban]

goodIbanCase :: IO String
goodIbanCase = do
    i <- randomRIO (0, length goodIbans - 1)
    return $ goodIbans !! i

testGoodIban :: IO ()
testGoodIban = testPost iban id goodIbanCase

badIbanCase :: IO String
badIbanCase = do
    testCase <- randomRIO (0, 4) :: IO Int
    iban <- goodIbanCase
    case testCase of
        0 -> return $ incrementDigit 3 iban
        1 -> takeLt4Characters iban 
        2 -> substituteCountryCode iban
        3 -> substituteCheckDigit 2 iban
        4 -> substituteCheckDigit 3 iban

testBadIban :: IO ()
testBadIban = testPost iban not badIbanCase

-- n is zero-based, can be negative
incrementDigit :: Int -> String -> String
incrementDigit n l = rotate (-n') $ incrementHead $ rotate n' l
    where n'               = n `mod` length l
          incrementHead xs = replaceHead
                                (flip (!!) 0 $ show $ (`mod` 10) $ (+1) $ read [head xs])
                                xs

takeLt4Characters :: String -> IO String
takeLt4Characters iban = do
    n <- randomRIO (1, 3)
    return $ take n $ removeNonAlphaNum iban

substituteCountryCode :: String -> IO String
substituteCountryCode iban = do
    x <- randomElement alphaNum
    y <- randomElement alphaNum
    if not (elem [toUpper x, toUpper y] iso3166)
    then return $ replaceHead (toUpper x) $ rotate (-1) 
                $ replaceHead (toUpper y) $ rotate 1
                $ removeNonAlphaNum iban
    else substituteCountryCode iban

substituteCheckDigit :: Int -> String -> IO String
substituteCheckDigit i iban = do
    a <- randomElement alpha
    return $ rotate (-i) $ replaceHead a $ rotate i iban

goodIbans :: [String]
goodIbans = ["AL47 2121 1009 0000 0002 3569 8741"
            ,"AD12 0001 2030 2003 5910 0100"
            ,"AT61 1904 3002 3457 3201"
            ,"AZ21 NABZ 0000 0000 1370 1000 1944"
            ,"BH67 BMAG 0000 1299 1234 56"
            ,"BE62 5100 0754 7061"
            ,"BA39 1290 0794 0102 8494"
            ,"BG80 BNBG 9661 1020 3456 78"
            ,"HR12 1001 0051 8630 0016 0"
            ,"CY17 0020 0128 0000 0012 0052 7600"
            ,"CZ65 0800 0000 1920 0014 5399"
            ,"DK50 0040 0440 1162 43"
            ,"EE38 2200 2210 2014 5685"
            ,"FO97 5432 0388 8999 44"
            ,"FI21 1234 5600 0007 85"
            ,"FR14 2004 1010 0505 0001 3M02 606"
            ,"GE29 NB00 0000 0101 9049 17"
            ,"DE89 3704 0044 0532 0130 00"
            ,"GI75 NWBK 0000 0000 7099 453"
            ,"GR16 0110 1250 0000 0001 2300 695"
            ,"GL56 0444 9876 5432 10"
            ,"HU42 1177 3016 1111 1018 0000 0000"
            ,"IS14 0159 2600 7654 5510 7303 39"
            ,"IE29 AIBK 9311 5212 3456 78"
            ,"IL62 0108 0000 0009 9999 999"
            ,"IT40 S054 2811 1010 0000 0123 456"
            ,"JO94 CBJO 0010 0000 0000 0131 0003 02"
            ,"KW81 CBKU 0000 0000 0000 1234 5601 01"
            ,"LV80 BANK 0000 4351 9500 1"
            ,"LB62 0999 0000 0001 0019 0122 9114"
            ,"LI21 0881 0000 2324 013A A"
            ,"LT12 1000 0111 0100 1000"
            ,"LU28 0019 4006 4475 0000"
            ,"MK072 5012 0000 0589 84"
            ,"MT84 MALT 0110 0001 2345 MTLC AST0 01S"
            ,"MU17 BOMM 0101 1010 3030 0200 000M UR"
            ,"MD24 AG00 0225 1000 1310 4168"
            ,"MC93 2005 2222 1001 1223 3M44 555"
            ,"ME25 5050 0001 2345 6789 51"
            ,"NL39 RABO 0300 0652 64"
            ,"NO93 8601 1117 947"
            ,"PK36 SCBL 0000 0011 2345 6702"
            ,"PL60 1020 1026 0000 0422 7020 1111"
            ,"PT50 0002 0123 1234 5678 9015 4"
            ,"QA58 DOHB 0000 1234 5678 90AB CDEF G"
            ,"RO49 AAAA 1B31 0075 9384 0000"
            ,"SM86 U032 2509 8000 0000 0270 100"
            ,"SA03 8000 0000 6080 1016 7519"
            ,"RS35 2600 0560 1001 6113 79"
            ,"SK31 1200 0000 1987 4263 7541"
            ,"SI56 1910 0000 0123 438"
            ,"ES80 2310 0001 1800 0001 2345"
            ,"SE35 5000 0000 0549 1000 0003"
            ,"CH93 0076 2011 6238 5295 7"
            ,"TN59 1000 6035 1835 9847 8831"
            ,"TR33 0006 1005 1978 6457 8413 26"
            ,"AE07 0331 2345 6789 0123 456"]
