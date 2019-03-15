﻿open System

let stripCharacters (text : string) =
  let separators = Set.ofList [' '; '_'; '-' ]
  let isSeparator = separators.Contains
  text
  |> String.filter (fun c -> Char.IsLetterOrDigit(c) || isSeparator(c))
  |> String.map (function
                | c when Char.IsLetterOrDigit(c) -> c
                | _ -> ' ')
  |> string
  
stripCharacters "hase-und & auto"

let padWord (text : string) =
  sprintf "  %s " (text.Trim(' '))

let words (text : string) =
  text.Trim(' ').Split([|' '|], StringSplitOptions.RemoveEmptyEntries)

let trigraphsOfWord (word : string) = [|
  for i in 0..word.Length-3 do
    yield word.[i..i+2]
|]

let trigraphsOfText text = 
  text
  |> stripCharacters
  |> words
  |> Array.map padWord
  |> Array.map trigraphsOfWord
  |> Array.concat
  |> Array.distinct
  |> Set.ofArray

let similiarityOfSets (set1 : Set<string>) (set2: Set<string>) : float =
  let smaller, larger = if set1.Count < set2.Count then set1, set2 else set2, set1
  (float (Set.intersect larger smaller).Count) / (float larger.Count)

let similiarity text1 text2 =
  let trigraph1 = trigraphsOfText text1
  let trigraph2 = trigraphsOfText text2
  similiarityOfSets trigraph1 trigraph2

trigraphsOfText "ab"
trigraphsOfText "cat"
trigraphsOfText "cat & test"

trigraphsOfText "a"
trigraphsOfText "ab"

similiarity "a" "ab"
similiarity "cat" "bat"


[<EntryPoint>]
let main argv =
    printf "Trigraph"
    
    // "01234567890" |> createTrigraphs |> Seq.iter (fun t -> printf "%s" t)
    0