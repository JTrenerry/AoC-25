(* This was really hard to do in a nice functional way at the current skill level of me
   Would possible be a good execirse to come back to eventually TM.

  TODO: Use a good functional solution and do not brute force this.
(*
  Checking obv bad, working back best

  Possible rules:
    Obv they need to be even digits as double pattern = 2 * pattern size = formula for even number

    Say I get 10 - 23

    how can I do this without just looping through all the possible values

    well split into smaller ranges

    10 - 19 and 20 - 23 since the first digit differ

    This is the half way point so it needs to repeat the pattern we stored, i.e. 1 and 2

    the range 0-9 includes 1 (0 <= 1 <= 9and then range 0 - 3 includes 2 (0 <= 2 <= 3)

    How do I get the numbers?

    each time we recurse we add it to a total and if we cannot find a value that makes it invalid how do we cancel that out?

*)

let rec intPow a b =
  if b = 0 then 1
  else a * intPow a (b-1)

let findBadIDs start fin =
  let startStr = string_of_int start in
  let finStr   = string_of_int fin in
  if 
    String.length startStr <> String.length finStr
  then (* Fix that and make them equal i.e. 1 -> 101 = 1 -> 9 & 10 -> 99 & 100 -> 101 *)
    let bigValSmlRange = let rec bvsrHelper x =
      if x = 0 then 0
      else 9 + 10 * bvsrHelper (x - 1)
    in bvsrHelper (String.length startStr)
    in
    findBadIDs start bigValSmlRange + findBadIDs (bigValSmlRange+1) fin
  else (* Good ranges *)
    if
      String.length startStr mod 2 <> 0
    then
      0 (* Uneven digits ranges with no even digit in between i.e 1 -> 9 *)
    else ((* FindBadIDs *)
      Printf.printf "Good range: %d -> %d\n" start fin;
      (* (* *)
        From here the best way is probs kinda brute force? grab the first half of the number and repeat it
        If that number is smaller than fin, it is invalid, if it is bigger, return whatever we had added together
      *)
      let rec findBadIDsHelper start fin acc =
        let d = intPow 10 (String.length (string_of_int start) / 2) in
        let pattern  = (start + d - 1) / d                          in
        let invalid  = pattern * d + pattern                        in
        if invalid <= fin && invalid >= start
        then (
          findBadIDsHelper (start + d) fin (acc + invalid)
        )
        else acc
      in
      findBadIDsHelper start fin 0
    )
  

let solve ls =
  List.fold_left (fun acc (a,b) -> acc + findBadIDs a b) 0 ls
*)


let is_invalid n =
  let s = string_of_int n in
  let len = String.length s in
  if len mod 2 <> 0 then false
  else
    let half = len / 2 in
    let first = String.sub s 0 half in
    let second = String.sub s half half in
    first = second

let findBadIDs start fin =
  let rec aux n acc =
    if n > fin then acc
    else
      if is_invalid n then aux (n+1) (acc + n)
      else aux (n+1) acc
  in
  aux start 0

let solve ranges =
  List.fold_left (fun acc (a,b) -> acc + findBadIDs1 a b) 0 ranges

let is_invalid2 n =
  let s = string_of_int n in
  let len = String.length s in
  let rec check l =
    if l > len / 2 then false
    else
      let pattern = String.sub s 0 l in
      let repeats = len / l in
      let built = String.concat "" (Array.to_list (Array.make repeats pattern)) in
      if built = s then true else check (l+1)
  in
  check 1

let findBadIDs2 start fin =
  let rec aux n acc =
    if n > fin then acc
    else
      if is_invalid2 n then aux (n+1) (acc + n)
      else aux (n+1) acc
  in
  aux start 0

let solve2 ranges =
  List.fold_left (fun acc (a,b) -> acc + findBadIDs2 a b) 0 ranges

let ranges = [
  (11, 22);
  (95, 115);
  (998, 1012);
  (1188511880, 1188511890);
  (222220, 222224);
  (1698522, 1698528);
  (446443, 446449);
  (38593856, 38593862);
  (565653, 565659);
  (824824821, 824824827);
  (2121212118, 2121212124)
]

let array = [
  (1090286,1131879);
  (3259566,3404881);
  (138124,175118);
  (266204727,266361099);
  (16765,24272);
  (7657360692,7657593676);
  (88857504,88926597);
  (6869078,6903096);
  (48444999,48532270);
  (61427792,61580535);
  (71,103);
  (8077,10421);
  (1920,2560);
  (2,17);
  (951,1259);
  (34,50);
  (28994,36978);
  (1309,1822);
  (9393918461,9393960770);
  (89479,120899);
  (834641,988077);
  (5389718924,5389797353);
  (34010076,34214499);
  (5063,7100);
  (607034,753348);
  (19098586,19261191);
  (125085556,125188689);
  (39839,51927);
  (3246,5037);
  (174,260);
  (439715,473176);
  (187287,262190);
  (348,535);
  (58956,78301);
  (4388160,4505757);
  (512092,584994);
  (13388753,13534387)
]
