# Funções de Alta Ordem em Haskell
### O que são Funções de Alta Ordem?

Funções de alta ordem (ou higher-order functions) são funções que podem receber outras funções como argumentos e/ou retornar funções como resultado. Elas são um conceito fundamental em linguagens funcionais como Haskell, permitindo maior modularidade e reutilização de código.

Exemplos comuns de funções de alta ordem incluem:
- `map`: Aplica uma função a todos os elementos de uma lista.
- `filter`: Seleciona elementos de uma lista com base em uma condição.
- `foldl` / `foldr`: Reduz uma lista a um único valor utilizando uma função acumuladora.

```haskell
soma :: Int -> Int -> Int
soma x y = x + y

main :: IO ()
main = do
    let lista = [1..10]
    let listaComSomaParaCadaElemento = map (soma 10) lista
    print ("Map utilizando função soma")
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
```

Explicação do Código
Função soma

soma :: Int -> Int -> Int
soma x y = x + y


map é usada para aplicar uma função a todos os elementos de uma lista. No código, temos diferentes usos de map:


let listaComSomaParaCadaElemento = map (soma 10) lista
print (listaComSomaParaCadaElemento)

Aqui, map aplica a função soma 10 a cada elemento da lista [1..10], resultando em uma nova lista onde 10 foi somado a cada elemento.

Mapeando sobre listas de listas:

let mapComMap = map (map (+1)) [[1, 2, 3], [4, 5, 6]]
print (mapComMap)

Neste caso, map é usado duas vezes. O primeiro map aplica a função (map (+1)) a cada sublista, e o segundo map incrementa cada elemento dessas sublistas.

Map com reverse:

    let reverseOlaMundo = map reverse ["ola", "mundo"]
    print (reverseOlaMundo)

    Aqui, map é utilizado para reverter as letras de cada palavra na lista ["ola", "mundo"].

Função filter

filter é usada para filtrar elementos de uma lista com base em uma condição.

Filtrando os números ímpares:
```
let listaFiltrandoOsPares = filter odd lista
print (listaFiltrandoOsPares)
```

Aqui, filter é usado com a função odd para retornar apenas os números ímpares da lista [1..10].

Combinando filter com map:

    ```
    let filterComMap = map (filter (\x -> x `mod` 2 == 0)) [lista, lista2]
    print (filterComMap)
    ```

    Nesta linha, filter é usado dentro de map para filtrar apenas os números pares de duas listas.

Função reverse

reverse é uma função padrão de Haskell que reverte uma lista (ou string, já que strings são listas de caracteres).

```
let reverseOlaMundo = map reverse ["ola", "mundo"]
```

Aqui, map aplica reverse a cada string da lista ["ola", "mundo"], resultando em ["alo", "odnum"].


# Como rodar
```
ghc -o meuprograma high_order_functions.hs
```


Saída Esperada

Ao executar o código, você verá uma saída similar a esta:

"Map utilizando função soma"
[11,12,13,14,15,16,17,18,19,20]
"Filtrando os pares"
[1,3,5,7,9]
"Map utilizando map"
[[2,3,4],[5,6,7]]
"Filter com map"
[[2,4,6,8,10],[10,12,14,16,18,20]]
"Map com reverse"
["ola","mundo"]
["alo","odnum"]
["ola","mundo"]

### Conclusão

Este exemplo demonstra como funções de alta ordem como `map` e `filter` podem ser usadas para manipular listas. 


### Referencias 
https://haskell.tailorfontela.com.br/higher-order-functions
https://haskell.pesquisa.ufabc.edu.br/haskell/06.higher.order/
https://folivetti.github.io/courses/Haskell/Tipos
