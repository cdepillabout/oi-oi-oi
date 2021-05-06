{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lib where

import Control.Comonad (Comonad(extract, extend))
import Control.Exception (evaluate)
import Control.Monad (void)
import System.IO (Handle, IOMode(ReadMode), hClose, hGetChar, hPutChar, openFile)
import System.IO.Unsafe (unsafePerformIO)

data OI a = D a | forall b. Show b => OI b

deriving instance Functor OI

instance Comonad OI where
  extract :: OI a -> a
  extract (D a) = a
  extract _ = error "OI Comonad instance, extract: passed OI"

  extend :: (OI a -> b) -> OI a -> OI b
  extend f w = D (f w)

(=>>) :: Comonad w => w a -> (w a -> b) -> w b
(=>>) = flip extend

stdOI :: OI a
stdOI = OI ()

coPure :: b -> OI a -> b
coPure b (D _) = b
coPure _ _ = error "coPure: passed OI"
{-# NOINLINE coPure #-}

coOpenFile :: FilePath -> IOMode -> OI a -> Handle
coOpenFile s m (D _) = unsafePerformIO $ openFile s m
coOpenFile _ _ _ = error "coOpenFile: passed OI"
{-# NOINLINE coOpenFile #-}

coHGetChar :: OI Handle -> Char
coHGetChar (D h) = unsafePerformIO (hGetChar h)
coHGetChar _ = error "coHGetChar: passed OI"
{-# NOINLINE coHGetChar #-}

coHPutChar :: Char -> OI Handle -> ()
coHPutChar c (D h) = unsafePerformIO (hPutChar h c)
coHPutChar _ _ = error "coHPutChar: passed OI"
{-# NOINLINE coHPutChar #-}

coGetChar :: OI a -> Char
coGetChar _ = unsafePerformIO getChar
{-# NOINLINE coGetChar #-}

coPutChar :: OI Char -> ()
coPutChar (D c) = unsafePerformIO (putChar c)
coPutChar _ = error "coPutChar: passed OI"
{-# NOINLINE coPutChar #-}

coPutStrLn :: OI String -> ()
coPutStrLn (D c) = unsafePerformIO (putStrLn c)
coPutStrLn _ = error "coPutStrLn: passed OI"

coHClose :: OI Handle -> ()
coHClose (D h) = unsafePerformIO (hClose h)
coHClose _ = error "coHClose: passed OI"
{-# NOINLINE coHClose #-}

run :: forall b. (forall a. OI a -> OI b) -> IO b
run f = do
  evaluate $ extract (f stdOI)

echoFunc :: String -> OI a -> OI ()
echoFunc s t =
  t =>>
  coOpenFile s ReadMode =>>
  coHGetChar =>>
  coPutChar =>>
  coPure '\n' =>>
  coPutChar

runEchoFunc :: IO ()
runEchoFunc = run (echoFunc "oi-oi-oi.cabal")

echoFuncDoesn'tWork :: OI a -> OI a
echoFuncDoesn'tWork t = t

echoFunc1 :: OI a -> OI [Maybe ()]
echoFunc1 t =
  t =>>
  coPure "hello" =>> \oiHello ->
    [ Just $ coPutStrLn oiHello
    , Just $ coPutStrLn oiHello
    ]

runEchoFunc1 :: IO ()
runEchoFunc1 = void $ run echoFunc1

