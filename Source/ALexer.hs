{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
{-# LINE 1 "./LexerParser/ALexer.x" #-}
module ALexer where
import AST
#if __GLASGOW_HASKELL__ >= 603
#include "ghcconfig.h"
#elif defined(__GLASGOW_HASKELL__)
#include "config.h"
#endif
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
import Data.Array.Base (unsafeAt)
#else
import Array
#endif
{-# LINE 1 "templates/wrappers.hs" #-}
{-# LINE 1 "templates/wrappers.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 9 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc5f56_0/ghc_2.h" #-}














































































































































































{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "templates/wrappers.hs" #-}
-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.






import Data.Word (Word8)
{-# LINE 28 "templates/wrappers.hs" #-}

import Data.Char (ord)
import qualified Data.Bits

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]



type Byte = Word8

-- -----------------------------------------------------------------------------
-- The input type


type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- pending bytes on current char
                  String)       -- current input string

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,_ps,s) = (p,c,[],s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_p,c,_bs,_s) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (_,_,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c
                                  (b:bs) = utf8Encode c
                              in p' `seq`  Just (b, (p', c, bs, s))


{-# LINE 102 "templates/wrappers.hs" #-}

{-# LINE 120 "templates/wrappers.hs" #-}

{-# LINE 138 "templates/wrappers.hs" #-}

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.


data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+alex_tab_size-1) `div` alex_tab_size)*alex_tab_size+1)
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)


-- -----------------------------------------------------------------------------
-- Default monad

{-# LINE 274 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Monad (with ByteString input)

{-# LINE 379 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Basic wrapper

{-# LINE 406 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Basic wrapper, ByteString version

{-# LINE 425 "templates/wrappers.hs" #-}

{-# LINE 440 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Posn wrapper

-- Adds text positions to the basic model.


--alexScanTokens :: String -> [token]
alexScanTokens str0 = go (alexStartPos,'\n',[],str0)
  where go inp__@(pos,_,_,str) =
          case alexScan inp__ 0 of
                AlexEOF -> []
                AlexError ((AlexPn _ line column),_,_,_) -> error $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
                AlexSkip  inp__' _ln     -> go inp__'
                AlexToken inp__' len act -> act pos (take len str) : go inp__'



-- -----------------------------------------------------------------------------
-- Posn wrapper, ByteString version

{-# LINE 473 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- GScan wrapper

-- For compatibility with previous versions of Alex, and because we can.

alex_tab_size :: Int
alex_tab_size = 8
alex_base :: Array Int Int
alex_base = listArray (0 :: Int, 94)
  [ -8
  , -55
  , -54
  , -113
  , 59
  , -102
  , -101
  , -104
  , -89
  , -84
  , 187
  , -97
  , -90
  , -85
  , -91
  , -93
  , -87
  , -80
  , -81
  , -78
  , -77
  , -4
  , -72
  , -61
  , -74
  , -60
  , 0
  , 300
  , -59
  , -52
  , -51
  , 0
  , -73
  , -111
  , -49
  , -38
  , -53
  , -44
  , -50
  , -47
  , -48
  , -46
  , 31
  , -39
  , -36
  , -43
  , 556
  , 492
  , 0
  , 739
  , 743
  , 0
  , 0
  , 0
  , 0
  , 0
  , 706
  , 667
  , 677
  , 698
  , 719
  , 729
  , 750
  , 902
  , 912
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 29
  , 0
  , 0
  , 0
  , 14
  , 15
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  ]

alex_table :: Array Int Int
alex_table = listArray (0 :: Int, 1167)
  [ 0
  , 49
  , 49
  , 49
  , 49
  , 49
  , 65
  , 80
  , 93
  , 44
  , 43
  , 40
  , 39
  , 38
  , 5
  , 6
  , 7
  , 20
  , 14
  , 23
  , 16
  , 37
  , 24
  , 17
  , 49
  , 2
  , 29
  , 81
  , 18
  , 74
  , 42
  , 19
  , 68
  , 69
  , 72
  , 70
  , 25
  , 71
  , 28
  , 73
  , 56
  , 56
  , 56
  , 56
  , 56
  , 56
  , 56
  , 56
  , 56
  , 56
  , 1
  , 94
  , 75
  , 79
  , 76
  , 12
  , 8
  , 3
  , 36
  , 45
  , 82
  , 92
  , 90
  , 89
  , 21
  , 91
  , 87
  , 85
  , 84
  , 54
  , 86
  , 53
  , 52
  , 51
  , 50
  , 78
  , 77
  , 0
  , 55
  , 0
  , 0
  , 0
  , 83
  , 66
  , 0
  , 67
  , 0
  , 0
  , 0
  , 64
  , 11
  , 15
  , 0
  , 62
  , 60
  , 0
  , 0
  , 30
  , 0
  , 0
  , 61
  , 9
  , 0
  , 34
  , 0
  , 0
  , 62
  , 58
  , 62
  , 62
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 41
  , 0
  , 47
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 0
  , 0
  , 0
  , 33
  , 46
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 4
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 32
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 49
  , 49
  , 49
  , 49
  , 49
  , -1
  , 56
  , 56
  , 56
  , 56
  , 56
  , 56
  , 56
  , 56
  , 56
  , 56
  , 62
  , 0
  , 0
  , 0
  , 62
  , 62
  , 0
  , 49
  , 0
  , 0
  , 62
  , 62
  , 88
  , 0
  , 62
  , 62
  , 0
  , 62
  , 62
  , 62
  , 62
  , 62
  , 0
  , 0
  , 0
  , 0
  , 0
  , 62
  , 62
  , 62
  , 57
  , 62
  , 0
  , 0
  , 0
  , 62
  , 62
  , 13
  , 0
  , 0
  , 0
  , 0
  , 62
  , 0
  , 0
  , 0
  , 0
  , 0
  , 62
  , 62
  , 62
  , 62
  , 62
  , 0
  , 0
  , 0
  , 62
  , 62
  , 0
  , 0
  , 22
  , 0
  , 63
  , 62
  , 0
  , 0
  , 62
  , 62
  , 0
  , 62
  , 62
  , 62
  , 62
  , 62
  , 0
  , 0
  , 0
  , 0
  , 0
  , 62
  , 62
  , 62
  , 62
  , 62
  , 0
  , 0
  , 0
  , 62
  , 62
  , 0
  , 0
  , 0
  , 0
  , 0
  , 62
  , 0
  , 0
  , 0
  , 0
  , 0
  , 62
  , 62
  , 62
  , 62
  , 0
  , 0
  , 0
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 46
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 48
  , 47
  , 4
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 31
  , 32
  , 10
  , 26
  , 26
  , 26
  , 27
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 62
  , 0
  , 0
  , 0
  , 62
  , 62
  , 0
  , 0
  , 0
  , 0
  , 62
  , 62
  , 0
  , 0
  , 62
  , 62
  , 0
  , 59
  , 62
  , 62
  , 62
  , 62
  , 0
  , 35
  , 0
  , 0
  , 0
  , 62
  , 62
  , 62
  , 62
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  ]

alex_check :: Array Int Int
alex_check = listArray (0 :: Int, 1167)
  [ -1
  , 9
  , 10
  , 11
  , 12
  , 13
  , 61
  , 61
  , 121
  , 111
  , 111
  , 115
  , 101
  , 97
  , 111
  , 105
  , 101
  , 108
  , 111
  , 99
  , 101
  , 105
  , 109
  , 101
  , 32
  , 33
  , 117
  , 35
  , 32
  , 37
  , 38
  , 108
  , 40
  , 41
  , 42
  , 43
  , 108
  , 45
  , 112
  , 47
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 58
  , 59
  , 60
  , 61
  , 62
  , 116
  , 116
  , 116
  , 110
  , 110
  , 68
  , 172
  , 100
  , 116
  , 115
  , 114
  , 110
  , 114
  , 116
  , 38
  , 120
  , 110
  , 108
  , 116
  , 45
  , 61
  , 61
  , -1
  , 124
  , -1
  , -1
  , -1
  , 90
  , 91
  , -1
  , 93
  , -1
  , -1
  , -1
  , 97
  , 98
  , 99
  , -1
  , 101
  , 102
  , -1
  , -1
  , 105
  , -1
  , -1
  , 108
  , 109
  , -1
  , 111
  , -1
  , -1
  , 114
  , 115
  , 116
  , 117
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 124
  , -1
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , -1
  , -1
  , -1
  , 194
  , 128
  , 129
  , 130
  , 131
  , 132
  , 133
  , 134
  , 135
  , 136
  , 137
  , 138
  , 139
  , 140
  , 141
  , 142
  , 143
  , 144
  , 145
  , 146
  , 147
  , 148
  , 149
  , 150
  , 151
  , 152
  , 153
  , 154
  , 155
  , 156
  , 157
  , 158
  , 159
  , 160
  , 161
  , 162
  , 163
  , 164
  , 165
  , 166
  , 167
  , 168
  , 169
  , 170
  , 171
  , 172
  , 173
  , 174
  , 175
  , 176
  , 177
  , 178
  , 179
  , 180
  , 181
  , 182
  , 183
  , 184
  , 185
  , 186
  , 187
  , 188
  , 189
  , 190
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 128
  , 129
  , 130
  , 131
  , 132
  , 133
  , 134
  , 135
  , 136
  , 137
  , 138
  , 139
  , 140
  , 141
  , 142
  , 143
  , 144
  , 145
  , 146
  , 147
  , 148
  , 149
  , 150
  , 151
  , 152
  , 153
  , 154
  , 155
  , 156
  , 157
  , 158
  , 159
  , 160
  , 161
  , 162
  , 163
  , 164
  , 165
  , 166
  , 167
  , 168
  , 169
  , 170
  , 171
  , 172
  , 173
  , 174
  , 175
  , 176
  , 177
  , 178
  , 179
  , 180
  , 181
  , 182
  , 183
  , 184
  , 185
  , 186
  , 187
  , 188
  , 189
  , 190
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 143
  , 144
  , 145
  , 146
  , 147
  , 148
  , 149
  , 150
  , 151
  , 152
  , 153
  , 154
  , 155
  , 156
  , 157
  , 158
  , 159
  , 160
  , 161
  , 162
  , 163
  , 164
  , 165
  , 166
  , 167
  , 168
  , 169
  , 170
  , 171
  , 172
  , 173
  , 174
  , 175
  , 176
  , 177
  , 178
  , 179
  , 180
  , 181
  , 182
  , 183
  , 184
  , 185
  , 186
  , 187
  , 188
  , 189
  , 190
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 0
  , 1
  , 2
  , 3
  , 4
  , 5
  , 6
  , 7
  , 8
  , 9
  , 10
  , 11
  , 12
  , 13
  , 14
  , 15
  , 16
  , 17
  , 18
  , 19
  , 20
  , 21
  , 22
  , 23
  , 24
  , 25
  , 26
  , 27
  , 28
  , 29
  , 30
  , 31
  , 32
  , 33
  , 34
  , 35
  , 36
  , 37
  , 38
  , 39
  , 40
  , 41
  , 42
  , 43
  , 44
  , 45
  , 46
  , 47
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 58
  , 59
  , 60
  , 61
  , 62
  , 63
  , 64
  , 65
  , 66
  , 67
  , 68
  , 69
  , 70
  , 71
  , 72
  , 73
  , 74
  , 75
  , 76
  , 77
  , 78
  , 79
  , 80
  , 81
  , 82
  , 83
  , 84
  , 85
  , 86
  , 87
  , 88
  , 89
  , 90
  , 91
  , 92
  , 93
  , 94
  , 95
  , 96
  , 97
  , 98
  , 99
  , 100
  , 101
  , 102
  , 103
  , 104
  , 105
  , 106
  , 107
  , 108
  , 109
  , 110
  , 111
  , 112
  , 113
  , 114
  , 115
  , 116
  , 117
  , 118
  , 119
  , 120
  , 121
  , 122
  , 123
  , 124
  , 125
  , 126
  , 127
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 9
  , 10
  , 11
  , 12
  , 13
  , 10
  , 48
  , 49
  , 50
  , 51
  , 52
  , 53
  , 54
  , 55
  , 56
  , 57
  , 97
  , -1
  , -1
  , -1
  , 101
  , 102
  , -1
  , 32
  , -1
  , -1
  , 97
  , 108
  , 109
  , -1
  , 101
  , 102
  , -1
  , 114
  , 115
  , 116
  , 117
  , 108
  , -1
  , -1
  , -1
  , -1
  , -1
  , 114
  , 115
  , 116
  , 117
  , 97
  , -1
  , -1
  , -1
  , 101
  , 102
  , 103
  , -1
  , -1
  , -1
  , -1
  , 108
  , -1
  , -1
  , -1
  , -1
  , -1
  , 114
  , 115
  , 116
  , 117
  , 97
  , -1
  , -1
  , -1
  , 101
  , 102
  , -1
  , -1
  , 105
  , -1
  , 97
  , 108
  , -1
  , -1
  , 101
  , 102
  , -1
  , 114
  , 115
  , 116
  , 117
  , 108
  , -1
  , -1
  , -1
  , -1
  , -1
  , 114
  , 115
  , 116
  , 117
  , 97
  , -1
  , -1
  , -1
  , 101
  , 102
  , -1
  , -1
  , -1
  , -1
  , -1
  , 108
  , -1
  , -1
  , -1
  , -1
  , -1
  , 114
  , 115
  , 116
  , 117
  , -1
  , -1
  , -1
  , 128
  , 129
  , 130
  , 131
  , 132
  , 133
  , 134
  , 135
  , 136
  , 137
  , 138
  , 139
  , 140
  , 141
  , 142
  , 143
  , 144
  , 145
  , 146
  , 147
  , 148
  , 149
  , 150
  , 151
  , 152
  , 153
  , 154
  , 155
  , 156
  , 157
  , 158
  , 159
  , 160
  , 161
  , 162
  , 163
  , 164
  , 165
  , 166
  , 167
  , 168
  , 169
  , 170
  , 171
  , 172
  , 173
  , 174
  , 175
  , 176
  , 177
  , 178
  , 179
  , 180
  , 181
  , 182
  , 183
  , 184
  , 185
  , 186
  , 187
  , 188
  , 189
  , 190
  , 191
  , 192
  , 193
  , 194
  , 195
  , 196
  , 197
  , 198
  , 199
  , 200
  , 201
  , 202
  , 203
  , 204
  , 205
  , 206
  , 207
  , 208
  , 209
  , 210
  , 211
  , 212
  , 213
  , 214
  , 215
  , 216
  , 217
  , 218
  , 219
  , 220
  , 221
  , 222
  , 223
  , 224
  , 225
  , 226
  , 227
  , 228
  , 229
  , 230
  , 231
  , 232
  , 233
  , 234
  , 235
  , 236
  , 237
  , 238
  , 239
  , 240
  , 241
  , 242
  , 243
  , 244
  , 245
  , 246
  , 247
  , 248
  , 249
  , 250
  , 251
  , 252
  , 253
  , 254
  , 255
  , 97
  , -1
  , -1
  , -1
  , 101
  , 102
  , -1
  , -1
  , -1
  , -1
  , 97
  , 108
  , -1
  , -1
  , 101
  , 102
  , -1
  , 114
  , 115
  , 116
  , 117
  , 108
  , -1
  , 110
  , -1
  , -1
  , -1
  , 114
  , 115
  , 116
  , 117
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  ]

alex_deflt :: Array Int Int
alex_deflt = listArray (0 :: Int, 94)
  [ -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 31
  , 31
  , -1
  , -1
  , -1
  , 48
  , 48
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 50
  , 50
  , 50
  , -1
  , 50
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  ]

alex_accept = listArray (0 :: Int, 94)
  [ AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccSkip
  , AlexAccSkip
  , AlexAcc 43
  , AlexAcc 42
  , AlexAcc 41
  , AlexAcc 40
  , AlexAcc 39
  , AlexAcc 38
  , AlexAcc 37
  , AlexAcc 36
  , AlexAcc 35
  , AlexAcc 34
  , AlexAcc 33
  , AlexAcc 32
  , AlexAcc 31
  , AlexAcc 30
  , AlexAcc 29
  , AlexAcc 28
  , AlexAcc 27
  , AlexAcc 26
  , AlexAcc 25
  , AlexAcc 24
  , AlexAcc 23
  , AlexAcc 22
  , AlexAcc 21
  , AlexAcc 20
  , AlexAcc 19
  , AlexAcc 18
  , AlexAcc 17
  , AlexAcc 16
  , AlexAcc 15
  , AlexAcc 14
  , AlexAcc 13
  , AlexAcc 12
  , AlexAcc 11
  , AlexAcc 10
  , AlexAcc 9
  , AlexAcc 8
  , AlexAcc 7
  , AlexAcc 6
  , AlexAcc 5
  , AlexAcc 4
  , AlexAcc 3
  , AlexAcc 2
  , AlexAcc 1
  , AlexAcc 0
  ]

alex_actions = array (0 :: Int, 44)
  [ (43,alex_action_2)
  , (42,alex_action_3)
  , (41,alex_action_4)
  , (40,alex_action_5)
  , (39,alex_action_6)
  , (38,alex_action_7)
  , (37,alex_action_8)
  , (36,alex_action_8)
  , (35,alex_action_8)
  , (34,alex_action_8)
  , (33,alex_action_8)
  , (32,alex_action_8)
  , (31,alex_action_8)
  , (30,alex_action_8)
  , (29,alex_action_9)
  , (28,alex_action_10)
  , (27,alex_action_11)
  , (26,alex_action_12)
  , (25,alex_action_13)
  , (24,alex_action_14)
  , (23,alex_action_15)
  , (22,alex_action_16)
  , (21,alex_action_17)
  , (20,alex_action_18)
  , (19,alex_action_19)
  , (18,alex_action_20)
  , (17,alex_action_21)
  , (16,alex_action_22)
  , (15,alex_action_23)
  , (14,alex_action_24)
  , (13,alex_action_25)
  , (12,alex_action_26)
  , (11,alex_action_27)
  , (10,alex_action_30)
  , (9,alex_action_32)
  , (8,alex_action_33)
  , (7,alex_action_34)
  , (6,alex_action_35)
  , (5,alex_action_36)
  , (4,alex_action_37)
  , (3,alex_action_38)
  , (2,alex_action_39)
  , (1,alex_action_40)
  , (0,alex_action_41)
  ]

{-# LINE 54 "./LexerParser/ALexer.x" #-}

data Token = TokenLBrak       {position :: AlexPosn} -- [
    | TokenRBrak       {position :: AlexPosn} -- ]
    | TokenLet         {position :: AlexPosn} -- :=
    | TokenNeg         {position :: AlexPosn} -- ¬
    | TokenAdd         {position :: AlexPosn} -- +
    | TokenMinus       {position :: AlexPosn} -- -
    | TokenTimes       {position :: AlexPosn} -- *
    | TokenDiv         {position :: AlexPosn} -- /
    | TokenMod         {position :: AlexPosn} -- %
    | TokenSgn         {position :: AlexPosn} -- ~
    | TokenIndep       {position :: AlexPosn} -- #
    | TokenOpen        {position :: AlexPosn} -- (
    | TokenClose       {position :: AlexPosn} -- )
    | TokenTInt        {position :: AlexPosn}
    | TokenTBool       {position :: AlexPosn}
    | TokenTColl       {position :: AlexPosn}
    | TokenFopGt       {position :: AlexPosn}
    | TokenFopLt       {position :: AlexPosn}
    | TokenFopGEt      {position :: AlexPosn}
    | TokenFopLEt      {position :: AlexPosn}
    | TokenFopEq       {position :: AlexPosn}
    | TokenFopNEq      {position :: AlexPosn}
    | TokenD           {position :: AlexPosn}
    | TokenZ           {position :: AlexPosn}
    | TokenINT         {position :: AlexPosn}
    | TokenCOLL        {position :: AlexPosn}
    | TokenBOOL        {position :: AlexPosn}
    | TokenVar         {position :: AlexPosn}
    | TokenLeast       {position :: AlexPosn}
    | TokenLargt       {position :: AlexPosn}
    | TokenFilter      {position :: AlexPosn}
    | TokenConcat      {position :: AlexPosn}
    | TokenMAX         {position :: AlexPosn}
    | TokenMIN         {position :: AlexPosn}
    | TokenSUM         {position :: AlexPosn}
    | TokenCOUNT       {position :: AlexPosn}
    | TokenLt          {position :: AlexPosn} -- <
    | TokenGt          {position :: AlexPosn} -- >
    | TokenGEt         {position :: AlexPosn} -- >=
    | TokenLEt         {position :: AlexPosn} -- <=
    | TokenEq          {position :: AlexPosn} -- ==
    | TokenNEq         {position :: AlexPosn} -- /=
    | TokenAND         {position :: AlexPosn} -- &&
    | TokenOR          {position :: AlexPosn} -- ||
    | TokenNOT         {position :: AlexPosn} -- ¬
    | TokenIsEmpty     {position :: AlexPosn}
    | TokenExpr        {position :: AlexPosn}
    | TokenSeq         {position :: AlexPosn} -- ;
    | TokenIfThenElse  {position :: AlexPosn}
    | TokenACCUM       {position :: AlexPosn}
    | TokenREPUNT      {position :: AlexPosn}
    | TokenInt         {position :: AlexPosn, value :: Int}
    | TokenBool        {position :: AlexPosn, valueB :: Bool}
    | TokenCollection  {position :: AlexPosn, valueC :: [Int]}
    | TokenName        {position :: AlexPosn, text :: String}
 deriving (Eq, Show)

lexer = alexScanTokens

alex_action_2 =  \pos str -> TokenTInt pos 
alex_action_3 =  \pos str -> TokenTBool pos 
alex_action_4 =  \pos str -> TokenTColl pos 
alex_action_5 =  \pos str -> TokenAND pos 
alex_action_6 =  \pos str -> TokenOR pos 
alex_action_7 =  \pos str -> TokenInt pos (read str) 
alex_action_8 =  \pos str -> TokenBool pos (read str) 
alex_action_9 =  \pos str -> TokenLet pos 
alex_action_10 =  \pos str -> TokenLBrak pos 
alex_action_11 =  \pos str -> TokenRBrak pos 
alex_action_12 =  \pos str -> TokenOpen pos 
alex_action_13 =  \pos str -> TokenClose pos 
alex_action_14 =  \pos str -> TokenAdd pos 
alex_action_15 =  \pos str -> TokenMinus pos 
alex_action_16 =  \pos str -> TokenTimes pos 
alex_action_17 =  \pos str -> TokenDiv pos 
alex_action_18 =  \pos str -> TokenMod pos 
alex_action_19 =  \pos str -> TokenLt pos 
alex_action_20 =  \pos str -> TokenGt pos 
alex_action_21 =  \pos str -> TokenGEt pos 
alex_action_22 =  \pos str -> TokenLEt pos 
alex_action_23 =  \pos str -> TokenEq pos 
alex_action_24 =  \pos str -> TokenNEq pos 
alex_action_25 =  \pos str -> TokenIndep pos 
alex_action_26 =  \pos str -> TokenD pos 
alex_action_27 =  \pos str -> TokenZ pos 
alex_action_28 =  \pos str -> TokenIndep pos 
alex_action_29 =  \pos str -> TokenLeast pos 
alex_action_30 =  \pos str -> TokenLargt pos 
alex_action_31 =  \pos str -> TokenConcat pos 
alex_action_32 =  \pos str -> TokenFilter pos 
alex_action_33 =  \pos str -> TokenMAX pos 
alex_action_34 =  \pos str -> TokenMIN pos 
alex_action_35 =  \pos str -> TokenSUM pos 
alex_action_36 =  \pos str -> TokenCOUNT pos 
alex_action_37 =  \pos str -> TokenAND pos 
alex_action_38 =  \pos str -> TokenOR pos 
alex_action_39 =  \pos str -> TokenNOT pos 
alex_action_40 =  \pos str -> TokenIsEmpty pos 
alex_action_41 =  \pos str -> TokenSeq pos 
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8a9b_0/ghc_2.h" #-}














































































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine

{-# LINE 21 "templates/GenericTemplate.hs" #-}

{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 72 "templates/GenericTemplate.hs" #-}
alexIndexInt16OffAddr arr off = arr ! off


{-# LINE 93 "templates/GenericTemplate.hs" #-}
alexIndexInt32OffAddr arr off = arr ! off


{-# LINE 105 "templates/GenericTemplate.hs" #-}
quickIndex arr i = arr ! i


-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input__ (sc)
  = alexScanUser undefined input__ (sc)

alexScanUser user__ input__ (sc)
  = case alex_scan_tkn user__ input__ (0) input__ sc AlexNone of
  (AlexNone, input__') ->
    case alexGetByte input__ of
      Nothing ->



                                   AlexEOF
      Just _ ->



                                   AlexError input__'

  (AlexLastSkip input__'' len, _) ->



    AlexSkip input__'' len

  (AlexLastAcc k input__''' len, _) ->



    AlexToken input__''' len (alex_actions ! k)


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user__ orig_input len input__ s last_acc =
  input__ `seq` -- strict in the input
  let
  new_acc = (check_accs (alex_accept `quickIndex` (s)))
  in
  new_acc `seq`
  case alexGetByte input__ of
     Nothing -> (new_acc, input__)
     Just (c, new_input) ->



      case fromIntegral c of { (ord_c) ->
        let
                base   = alexIndexInt32OffAddr alex_base s
                offset = (base + ord_c)
                check  = alexIndexInt16OffAddr alex_check offset

                new_s = if (offset >= (0)) && (check == ord_c)
                          then alexIndexInt16OffAddr alex_table offset
                          else alexIndexInt16OffAddr alex_deflt s
        in
        case new_s of
            (-1) -> (new_acc, input__)
                -- on an error, we want to keep the input *before* the
                -- character that failed, not after.
            _ -> alex_scan_tkn user__ orig_input (if c < 0x80 || c >= 0xC0 then (len + (1)) else len)
                                                -- note that the length is increased ONLY if this is the 1st byte in a char encoding)
                        new_input new_s new_acc
      }
  where
        check_accs (AlexAccNone) = last_acc
        check_accs (AlexAcc a  ) = AlexLastAcc a input__ (len)
        check_accs (AlexAccSkip) = AlexLastSkip  input__ (len)

        check_accs (AlexAccPred a predx rest)
           | predx user__ orig_input (len) input__
           = AlexLastAcc a input__ (len)
           | otherwise
           = check_accs rest
        check_accs (AlexAccSkipPred predx rest)
           | predx user__ orig_input (len) input__
           = AlexLastSkip input__ (len)
           | otherwise
           = check_accs rest


data AlexLastAcc
  = AlexNone
  | AlexLastAcc !Int !AlexInput !Int
  | AlexLastSkip     !AlexInput !Int

data AlexAcc user
  = AlexAccNone
  | AlexAcc Int
  | AlexAccSkip

  | AlexAccPred Int (AlexAccPred user) (AlexAcc user)
  | AlexAccSkipPred (AlexAccPred user) (AlexAcc user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user__ in1 len in2
  = p1 user__ in1 len in2 && p2 user__ in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _
alexPrevCharIs c _ input__ _ _ = c == alexInputPrevChar input__

alexPrevCharMatches f _ input__ _ _ = f (alexInputPrevChar input__)

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _
alexPrevCharIsOneOf arr _ input__ _ _ = arr ! alexInputPrevChar input__

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (sc) user__ _ _ input__ =
     case alex_scan_tkn user__ input__ (0) input__ sc AlexNone of
          (AlexNone, _) -> False
          _ -> True
        -- TODO: there's no need to find the longest
        -- match when checking the right context, just
        -- the first match will do.
