//
// F# program to input a string and print out information
// about the # of vowels and digraphs in that string.
//
// Name:      Becca Nika
// UIC NetID: rnika3
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

//
// explode:
//
// Given a string s, explodes the string into a list of characters.
// Example: explode "apple" => ['a';'p';'p';'l';'e']
//
let explode (S:string) = 
  List.ofArray (S.ToCharArray())

//
// implode:
//
// The opposite of explode --- given a list of characters, returns
// the list as a string. Example: implode ['t';'h';'e'] => "the"
//
let implode (L:char list) = 
  new string(List.toArray L)

//
// length:
//
// returns the length of the exploded input
let rec length L = 
  match L with
    | []      -> 0
    | _::tail -> 1 + length tail  

//
// numVowels:
//
// returns the number of vowels in the exploded input
let rec numVowels L =
  match L with
    | []                         -> 0
    | head::tail when head = 'a' -> 1 + numVowels tail
    | head::tail when head = 'e' -> 1 + numVowels tail
    | head::tail when head = 'i' -> 1 + numVowels tail
    | head::tail when head = 'o' -> 1 + numVowels tail
    | head::tail when head = 'u' -> 1 + numVowels tail
    | head::tail                 -> numVowels tail

//
// specificVowels:
//
// takes in a char and returns the total number of that char from L
let rec specificVowels x L =
  match L with
    | []                       -> 0
    | head::tail when head = x -> 1 + specificVowels x tail
    | head::tail               -> specificVowels x tail

//
// first:
//
// checks if a char c is the first character in a list
// returns true if yes and false if no or empty
let first c L = 
  match L with
    | []                       -> false
    | head::tail when head = c -> true
    | head::tail               -> false

//
// specificDigraphCount:
//
// takes in a specific digraph and returns the total count from L.
// uses first function to see if the second inputed character (y) is 
// the next element in the list
let rec specificDigraphCount x y L =
  match L with
    | []                                                -> 0
    | head::tail when head = x && (first y tail = true) -> 1 + specificDigraphCount x y tail
    | head::tail                                        -> specificDigraphCount x y tail

//
// digraphCount:
//
// counts the total number of digraphs found using specificDigraphCount
let rec digraphCount L =
  match L with
    | [] -> 0
    | head::tail when head > 0 -> head + digraphCount tail
    | head::tail -> digraphCount tail

// 
// main:
//
[<EntryPoint>]
let main argv =
  printfn "Starting"
  printfn ""

  //
  // input string, output length and # of vowels:
  //
  printf("input> ")
  let input = System.Console.ReadLine()

  let L = explode input
  printfn "exploded: %A" L

  // prints out length of input

  let len = length L
  printfn "length: %A" len

  // prints out the number of vowels

  let vowels = ['a';'e';'i';'o';'u']
  let numVowel = numVowels L
  printfn "vowels: %A" numVowel

  // prints out the number of specific vowels

  let numVowels = vowels |> List.map (fun x -> specificVowels x L)
  (vowels, numVowels) ||> List.iter2 (printfn "'%c': %i")
  
  // prints the number of digraphs

  let digraph1 = ['a'; 'c'; 'e'; 'i'; 'o'; 'p'; 's'; 't'; 'w']
  let digraph2 = ['i'; 'h'; 'a'; 'e'; 'u'; 'h'; 'h'; 'h'; 'h']

  let numDigraph = (digraph1, digraph2) ||> List.map2 (fun x y -> specificDigraphCount x y L)
  let totalDigraphs = digraphCount numDigraph
  printfn "digraphs: %A" totalDigraphs

  // prints the number of specific digraphs

  (digraph1, digraph2) ||> List.iteri2 (fun i x y -> printfn "'%c','%c': %i" x y (numDigraph |> List.item i))

  //
  // done: implode list, print, and return
  //
  let S = implode L
  printfn "imploded: %A" S

  printfn ""
  printfn "Done"
  0  // return 0 => success, much like C++
