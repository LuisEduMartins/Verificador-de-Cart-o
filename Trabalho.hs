data Dispositivo =  Celular String String Int String
                    |Tablet String String Float Bool Int String
                    |Notebook String String Float Bool Int String
                        deriving (Eq,Ord,Show)

item1,item2,item3 :: Dispositivo
item1 = Notebook "lenovo" "ideaPad" 15.6 False 15 "notebook"
item2 = Notebook "lenovo" "ideaPad" 15.6 False 15 "notebook"
item3 = Notebook "lenovo" "ideaPad" 15.6 False 15 "notebook"
                        
dispositivos :: [Dispositivo]
dispositivos = [item1,item2,item3]

                        
-- QUESTÃO 1 ----------------------------------------------------------------------------------------------
mostraDispositivo (dispositivos) = do
    putStrLn ("Celular: " ++ show item1)
    putStrLn("-----------------------------------------------------------------------")
    putStrLn ("Tablet: " ++ show item2)
    putStrLn("-----------------------------------------------------------------------")
    putStrLn ("Notebook: " ++ show item3)
    putStrLn("-----------------------------------------------------------------------")
-- QUESTÃO 2 ---------------------------------------------------------------------------------------------
qntdadeCelular (dispositivos) = do
    putStrLn ("antidade de Celular: " ++ show (qntCelular(item1)) ++" + " ++ show (qntCelular(item2))++ " + "++ show (qntCelular(item3)) ++"\ntotal : "++ show(soma3(qntCelular(item1))(qntCelular(item2))(qntCelular(item3))) ) 
qntCelular :: Dispositivo -> Int
qntCelular (Celular marca modelo quant cat) 
    |cat == "celular" = 1
    |cat/="celular" = 0

qntCelular (Tablet marca modelo tela caneta quant cat) 
    |cat == "celular" = 1
    |cat/="celular" = 0

qntCelular (Notebook marca modelo tela toque quant cat)
    |cat == "celular" = 1
    |cat/="celular" = 0
    
soma3 :: Int -> Int -> Int -> Int
soma3 a b c = a + b + c
-- QUESTÃO 3 ----------------------------------------------------------------------------------------------
qntdadeTablet (dispositivos) = do
    putStrLn ("Quantidade de Tablet: " ++ show (qntTablet(item1)) ++" + " ++ show (qntTablet(item2))++ " + "++ show (qntTablet(item3)) ++"\ntotal : "++ show(soma3(qntTablet(item1))(qntTablet(item2))(qntTablet(item3))) ) 
qntTablet :: Dispositivo -> Int
qntTablet (Celular marca modelo quant cat) 
    |cat == "tablet" = 1
    |cat/="tablet" = 0

qntTablet (Tablet marca modelo tela caneta quant cat) 
    |cat == "tablet" = 1
    |cat/="tablet" = 0

qntTablet (Notebook marca modelo tela toque quant cat)
    |cat == "tablet" = 1
    |cat/="tablet" = 0
 -- QUESTÃO 4 --------------------------------------------------------------------------------------------   
qntdadeNotebook (dispositivos) = do
    putStrLn ("Quantidade de Notebook: " ++ show (qntNotebook(item1)) ++" + " ++ show (qntNotebook(item2))++ " + "++ show (qntNotebook(item3)) ++"\ntotal : "++ show(soma3(qntNotebook(item1))(qntNotebook(item2))(qntNotebook(item3))) ) 
qntNotebook :: Dispositivo -> Int
qntNotebook (Celular marca modelo quant cat) 
    |cat == "notebook" = 1
    |cat/="notebook" = 0

qntNotebook (Tablet marca modelo tela caneta quant cat) 
    |cat == "notebook" = 1
    |cat/="notebook" = 0

qntNotebook (Notebook marca modelo tela toque quant cat)
    |cat == "notebook" = 1
    |cat/="notebook" = 0
-- QUESTÃO 5 --------------------------------------------------------------------------------------------
telaSensivel (dispositivos) = do
    putStrLn (show(tela(item1))++ "\n"++show(tela(item2))++"\n"++show(tela(item3)))
tela :: Dispositivo -> String
tela (Celular marca modelo quant cat) = "Atualmente, todos os celulares tem tela sensivel ao toque"

tela(Tablet marca modelo tela caneta quant cat)
    |caneta == True = "Esse Tablet tem tela sensivel ao toque e vem com caneta junto"
    |caneta == False = "Esse Tablet tem tela sensivel ao toque mas nao vem com caneta junto"

tela(Notebook marca modelo tela toque quant cat)
    |toque == True = "Esse Notebook tem tela sensivel ao toque"
    |toque == False = "Esse Notebook nao tem tela sensivel ao toque"

-- QUESTÃO 6 ---------------------------------------------------------------------------------------------
checarMarca(dispositivos)(marca) = do
     putStrLn (show (checar(item1)(marca))++"\n"++ show (checar(item2)(marca))++"\n"++ show (checar(item3)(marca)))

checar :: Dispositivo -> String -> String
checar(Celular marca modelo quant cat) marcadisp  
    |marca == marcadisp = "O Celular e dessa marca"
    |marca /= marcadisp = "O Celular nao e dessa marca"

checar(Tablet marca modelo tela caneta quant cat) marcadisp
    |marca == marcadisp = "O Tablet e dessa marca"
    |marca /= marcadisp = "O Tablet nao e dessa marca"

checar(Notebook marca modelo tela toque quant cat) marcadisp
    |marca == marcadisp = "O Notebook e dessa marca"
    |marca /= marcadisp = "O Notebook nao e dessa marca"