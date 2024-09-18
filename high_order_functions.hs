soma :: Int -> Int -> Int
soma x y = x + y

main :: IO ()
main = do
    let lista = [1..10]
    let listaComSomaParaCadaElemento = map (soma 10) lista
    print ("Map utilizando funcao soma")
    print (listaComSomaParaCadaElemento)

    let listaFiltrandoOsPares = filter odd lista
    print ("Filtrando os pares")
    print (listaFiltrandoOsPares)

    let lista2 = [10..20]
    print ("Map utilizando map")
    let mapComMap = map (map (+1)) [[1,2,3],[4,5,6]]
    print (mapComMap)

    let filterComMap = map (filter (\x -> x `mod` 2 == 0)) [lista,lista2] 
    print ("Filter com map")
    print (filterComMap)
    
    let olaMundo = ["ola","mundo"]

    let reverseOlaMundo = map reverse ["ola","mundo"]
    print ("Map com reverse")
    print (olaMundo)
    print (reverseOlaMundo)
    print(map reverse reverseOlaMundo)