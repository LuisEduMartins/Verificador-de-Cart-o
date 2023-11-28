
addSum :: String -> String
addSum input =
  let digitos = map (\c -> read [c] :: Int) input 
      sumDigits = sum digitos 
      verificadores = show sumDigits 
  in input ++ verificadores 

validaCartao :: String -> String
validaCartao cartao
    |length (cartao) /=10 = "Cartão Invalido"
    |somaOitoNum(take 8 cartao) == ultimos(drop 8 cartao) = "Cartão valido"
    |otherwise = "Cartão invalido"


ultimos :: String -> Int
ultimos num = read(reverse(take 2 (reverse num))) :: Int

somaOitoNum :: String -> Int
somaOitoNum num 
    |((length num <=8) && (length num>0)) = (read(take 1 num) :: Int) + somaOitoNum (drop 1 num)
    |otherwise = 0

Questão 3 (1).txt
﻿import Data.Char (toLower)


eVogal :: Char -> Bool
eVogal c = toLower c `elem` "aeiou"


eConsoante :: Char -> Bool
eConsoante c = toLower c `elem` "bcdfghjklmnpqrstvwxyz"


contaVogais :: String -> Int
contaVogais = length . filter eVogal


contaConsoante :: String -> Int
contaConsoante = length . filter eConsoante


validaSenha :: String -> Bool
validaSenha senha =
  let len = length senha
      vogais = contaVogais senha
      consoantes = contaConsoante senha
  in len >= 4 && len <= 8 && vogais >= 2 && consoantes >= 2



filterBadPassword :: [String] -> [String]
filterBadPassword [] = []
filterBadPassword lista = filter validaSenha lista
