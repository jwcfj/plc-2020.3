import Control.Concurrent
import Text.Printf

--Autor: André Luiz Pereira da Silva
--alps2@cin.ufpe.br

main = do
    disp <- newMVar True -- True: maquina livre | false: maquina ocupada
    pc <- newMVar 2000  --P-Cola
    gpn <- newMVar 2000 --Guarana Polo Norte
    gq <- newMVar 2000  --Guarana Quate
    clientesPc <- newMVar 0  --Total de clientes de P-Cola
    clientesGpn <- newMVar 0 --Total de clientes de Guarana Polo Norte
    clientesGq <- newMVar 0  --Total de clientes de Guarana Quate
    forkIO $ cliente "P-Cola" disp pc clientesPc
    forkIO $ cliente "Guaraná Polo Norte"  disp gpn clientesGpn
    forkIO $ cliente "Guaraná Quate" disp gq clientesGq
    forkIO $ repor disp pc gpn gq

cliente :: String -> MVar Bool -> MVar Int -> MVar Int -> IO ()
cliente nome disp refri total = loop
    where
        loop = do
            status <- takeMVar disp
            quantidade <- takeMVar refri
            threadDelay 1000000 --conversao de microseconds para milliseconds
            putMVar refri (quantidade - 300)
            totalClientes <- takeMVar total
            printf "O cliente %d do refrigerante %s está enchendo seu copo\n" (totalClientes+1) nome
            putMVar total (totalClientes+1)
            putMVar disp True
            loop


repor :: MVar Bool -> MVar Int -> MVar Int -> MVar Int -> IO ()
repor disp pc gpn gq = loop
    where
        loop = do
            
            status <- takeMVar disp

            quantidade <- takeMVar pc
            if quantidade < 1000
                then do
                    threadDelay 1500000 --conversao de microseconds para milliseconds
                    putMVar pc (quantidade + 1000)
                    printf "O refrigerante P-Cola foi reabastecido com 1000 ml, e agora possui %d ml\n" (quantidade + 1000)
                else do
                    putMVar pc quantidade

            quantidade <- takeMVar gpn
            if quantidade < 1000
                then do
                    threadDelay 1500000 --conversao de microseconds para milliseconds
                    putMVar gpn (quantidade + 1000)
                    printf "O refrigerante Guaraná Polo Norte foi reabastecido com 1000 ml, e agora possui %d ml\n" (quantidade + 1000)
                else do
                    putMVar gpn quantidade
          
            quantidade <- takeMVar gq
            if quantidade < 1000
                then do
                    threadDelay 1500000 --conversao de microseconds para milliseconds
                    putMVar gq (quantidade + 1000)
                    printf "O refrigerante Guaraná Quate foi reabastecido com 1000 ml, e agora possui %d ml\n" (quantidade + 1000)
                else do
                    putMVar gq quantidade
        
            putMVar disp True
            loop