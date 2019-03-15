open System

let stripCharacters (text : string) =
  let separators = Set.ofList [' '; '_'; '-'; '|' ]
  let isSeparator = separators.Contains
  text
  |> String.filter (fun c -> Char.IsLetterOrDigit(c) || isSeparator(c))
  |> String.map (function
                | c when Char.IsLetterOrDigit(c) -> c
                | _ -> ' ')
  |> string

let padWord (text : string) =
  sprintf "  %s " (text.Trim(' '))

let words (text : string) =
  text.Trim(' ').Split([|' '|], StringSplitOptions.RemoveEmptyEntries)

let trigramsOfWord (word : string) = [|
  for i in 0..word.Length-3 do
    yield word.[i..i+2]
|]

let trigramsOfText text = 
  text
  |> stripCharacters
  |> words
  |> Array.map padWord
  |> Array.map trigramsOfWord
  |> Array.concat
  |> Array.distinct
  |> Set.ofArray

let similarityOfSets (set1 : Set<string>) (set2: Set<string>) : float =
  let smaller, larger = if set1.Count < set2.Count then set1, set2 else set2, set1
  let intersection = Set.intersect larger smaller
  (float intersection.Count) / (float (Set.union larger smaller).Count)

let similarity text1 text2 =
  let trigraph1 = trigramsOfText text1
  let trigraph2 = trigramsOfText text2
  similarityOfSets trigraph1 trigraph2

stripCharacters "hase-und & auto"

trigramsOfText "ab"
trigramsOfText "cat"
trigramsOfText "cat & test"
trigramsOfText "foo|bar"

trigramsOfText "a"
trigramsOfText "ab"

similarity "a" "ab"

similarity "cat" "bat"
similarity "fat" "zat"

similarity "0123" "01234"
similarity "0123" "10123"
similarity "0123" "0123"

[<EntryPoint>]
let main argv =
    printf "Trigram"
    0
