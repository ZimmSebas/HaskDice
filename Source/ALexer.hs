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
{-# LINE 1 "/tmp/ghc551d_0/ghc_2.h" #-}














































































































































































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

{-# LINE 79 "templates/wrappers.hs" #-}

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

{-# LINE 161 "templates/wrappers.hs" #-}

-- -----------------------------------------------------------------------------
-- Default monad

{-# LINE 274 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Monad (with ByteString input)

{-# LINE 379 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Basic wrapper


type AlexInput = (Char,[Byte],String)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (c,_,_) = c

-- alexScanTokens :: String -> [token]
alexScanTokens str = go ('\n',[],str)
  where go inp__@(_,_bs,s) =
          case alexScan inp__ 0 of
                AlexEOF -> []
                AlexError _ -> error "lexical error"
                AlexSkip  inp__' _ln     -> go inp__'
                AlexToken inp__' len act -> act (take len s) : go inp__'

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (c,(b:bs),s) = Just (b,(c,bs,s))
alexGetByte (_,[],[])    = Nothing
alexGetByte (_,[],(c:s)) = case utf8Encode c of
                             (b:bs) -> Just (b, (c, bs, s))
                             [] -> Nothing



-- -----------------------------------------------------------------------------
-- Basic wrapper, ByteString version

{-# LINE 425 "templates/wrappers.hs" #-}

{-# LINE 440 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Posn wrapper

-- Adds text positions to the basic model.

{-# LINE 457 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Posn wrapper, ByteString version

{-# LINE 473 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- GScan wrapper

-- For compatibility with previous versions of Alex, and because we can.

alex_tab_size :: Int
alex_tab_size = 8
alex_base :: Array Int Int
alex_base = listArray (0 :: Int, 54)
  [ -8
  , -55
  , -54
  , 59
  , -103
  , -102
  , 187
  , -101
  , -94
  , -87
  , -88
  , -97
  , -93
  , -92
  , -99
  , 0
  , 300
  , -91
  , 0
  , -74
  , -154
  , -104
  , -17
  , -84
  , -86
  , -85
  , 556
  , 492
  , 0
  , 46
  , 738
  , 0
  , 0
  , 0
  , 0
  , 0
  , 12
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , -22
  , 0
  , 0
  , 0
  , -33
  , -25
  , 0
  , 0
  , 0
  , 0
  , 0
  ]

alex_table :: Array Int Int
alex_table = listArray (0 :: Int, 993)
  [ 0
  , 29
  , 29
  , 29
  , 29
  , 29
  , 37
  , 53
  , 24
  , 23
  , 4
  , 5
  , 14
  , 9
  , 12
  , 13
  , 10
  , 8
  , 42
  , 25
  , 35
  , 34
  , 32
  , 30
  , 29
  , 2
  , 33
  , 54
  , 51
  , 47
  , 22
  , 31
  , 40
  , 41
  , 45
  , 43
  , 50
  , 44
  , 0
  , 46
  , 36
  , 36
  , 36
  , 36
  , 36
  , 36
  , 36
  , 36
  , 36
  , 36
  , 1
  , 0
  , 48
  , 52
  , 49
  , 29
  , 29
  , 29
  , 29
  , 29
  , 36
  , 36
  , 36
  , 36
  , 36
  , 36
  , 36
  , 36
  , 36
  , 36
  , 0
  , 0
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
  , 0
  , 38
  , 0
  , 39
  , 0
  , 0
  , 0
  , 0
  , 7
  , 11
  , 0
  , 0
  , 0
  , 0
  , 0
  , 17
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
  , 21
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
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
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
  , 0
  , 20
  , 26
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 3
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 19
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
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
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 26
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 28
  , 27
  , 3
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 18
  , 19
  , 6
  , 15
  , 15
  , 15
  , 16
  , -1
  , -1
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

alex_check :: Array Int Int
alex_check = listArray (0 :: Int, 993)
  [ -1
  , 9
  , 10
  , 11
  , 12
  , 13
  , 61
  , 61
  , 111
  , 111
  , 111
  , 105
  , 99
  , 101
  , 111
  , 108
  , 108
  , 116
  , 172
  , 110
  , 124
  , 38
  , 108
  , 45
  , 32
  , 33
  , 110
  , 35
  , 61
  , 37
  , 38
  , 116
  , 40
  , 41
  , 42
  , 43
  , 61
  , 45
  , -1
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
  , -1
  , 60
  , 61
  , 62
  , 9
  , 10
  , 11
  , 12
  , 13
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
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 32
  , -1
  , -1
  , -1
  , -1
  , 91
  , -1
  , 93
  , -1
  , -1
  , -1
  , -1
  , 98
  , 99
  , -1
  , -1
  , -1
  , -1
  , -1
  , 105
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 124
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
  , 10
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
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
  ]

alex_deflt :: Array Int Int
alex_deflt = listArray (0 :: Int, 54)
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
  , 18
  , 18
  , -1
  , 28
  , 28
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 30
  , 30
  , 30
  , -1
  , 30
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
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

alex_accept = listArray (0 :: Int, 54)
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
  , AlexAccSkip
  , AlexAccSkip
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

alex_actions = array (0 :: Int, 24)
  [ (23,alex_action_2)
  , (22,alex_action_3)
  , (21,alex_action_4)
  , (20,alex_action_5)
  , (19,alex_action_6)
  , (18,alex_action_7)
  , (17,alex_action_8)
  , (16,alex_action_9)
  , (15,alex_action_10)
  , (14,alex_action_11)
  , (13,alex_action_12)
  , (12,alex_action_13)
  , (11,alex_action_14)
  , (10,alex_action_15)
  , (9,alex_action_16)
  , (8,alex_action_17)
  , (7,alex_action_18)
  , (6,alex_action_19)
  , (5,alex_action_20)
  , (4,alex_action_21)
  , (3,alex_action_22)
  , (2,alex_action_23)
  , (1,alex_action_24)
  , (0,alex_action_25)
  ]

{-# LINE 41 "./LexerParser/ALexer.x" #-}

data Token =
    TokenLBrak       {position :: AlexPosn} -- [
    TokenRBrak       {position :: AlexPosn} -- ]
    TokenLet         {position :: AlexPosn} -- :=
    TokenNeg         {position :: AlexPosn} -- ¬
    TokenAdd         {position :: AlexPosn} -- +
    TokenMinus       {position :: AlexPosn} -- -
    TokenTimes       {position :: AlexPosn} -- *
    TokenDiv         {position :: AlexPosn} -- /
    TokenMod         {position :: AlexPosn} -- %
    TokenSgn         {position :: AlexPosn} -- ~
    TokenIndep       {position :: AlexPosn} -- #
    TokenOpen        {position :: AlexPosn} -- (
    TokenClose       {position :: AlexPosn} -- )
    TokenInt         {position :: AlexPosn}
    TokenBool        {position :: AlexPosn}
    TokenCollection  {position :: AlexPosn}
    TokenTInt        {position :: AlexPosn}
    TokenTBool       {position :: AlexPosn}
    TokenTColl       {position :: AlexPosn}
    TokenFopGt       {position :: AlexPosn}
    TokenFopLt       {position :: AlexPosn}
    TokenFopGEt      {position :: AlexPosn}
    TokenFopLEt      {position :: AlexPosn}
    TokenFopEq       {position :: AlexPosn}
    TokenFopNEq      {position :: AlexPosn}
    TokenD           {position :: AlexPosn}
    TokenZ           {position :: AlexPosn}
    TokenINT         {position :: AlexPosn}
    TokenCOLL        {position :: AlexPosn}
    TokenBOOL        {position :: AlexPosn}
    TokenVar         {position :: AlexPosn}
    TokenLeast       {position :: AlexPosn}
    TokenLargt       {position :: AlexPosn}
    TokenFilter      {position :: AlexPosn}
    TokenConcat      {position :: AlexPosn}
    TokenMAX         {position :: AlexPosn}
    TokenMIN         {position :: AlexPosn}
    TokenSUM         {position :: AlexPosn}
    TokenCOUNT       {position :: AlexPosn}
    TokenLt          {position :: AlexPosn} -- <
    TokenGt          {position :: AlexPosn} -- >
    TokenGEt         {position :: AlexPosn} -- >=
    TokenLEt         {position :: AlexPosn} -- <=
    TokenEq          {position :: AlexPosn} -- ==
    TokenNEq         {position :: AlexPosn} -- /=
    TokenAND         {position :: AlexPosn} -- &&
    TokenOR          {position :: AlexPosn} -- ||
    TokenNOT         {position :: AlexPosn} -- ¬
    TokenIsEmpty     {position :: AlexPosn}
    TokenExpr        {position :: AlexPosn}
    TokenLet         {position :: AlexPosn}
    TokenSeq         {position :: AlexPosn} -- ;
    TokenIfThenElse  {position :: AlexPosn}
    TokenACCUM       {position :: AlexPosn}
    TokenREPUNT      {position :: AlexPosn}
    TokenInt         {position :: AlexPosn, value :: Int}
    TokenBool        {position :: AlexPosn, value :: Bool}
    TokenCollection  {position :: AlexPosn, value :: [Int]}
    TokenName        {position :: AlexPosn, text :: String}
 deriving (Eq, Show)

lexer = alexScanTokens

alex_action_2 =  \pos str -> TokenTInt pos 
alex_action_3 =  \pos str -> TokenTBool pos 
alex_action_4 =  \pos str -> TokenTColl pos 
alex_action_5 =  \pos str -> TokenAnd pos 
alex_action_6 =  \pos str -> TokenOr pos 
alex_action_7 =  \pos str -> TokenInt pos (read str) 
alex_action_8 =  \pos str -> TokenLet pos 
alex_action_9 =  \pos str -> TokenLBrak pos 
alex_action_10 =  \pos str -> TokenRBrak pos 
alex_action_11 =  \pos str -> TokenOpen pos 
alex_action_12 =  \pos str -> TokenClose pos 
alex_action_13 =  \pos str -> TokenNeg pos 
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
