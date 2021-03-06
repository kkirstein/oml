(*
   Copyright 2015:
     Leonid Rozenberg <leonidr@gmail.com>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

open Test_utils
open Util
open Uncategorized.Estimations
module D = Statistics.Descriptive

let () =
  let add_random_test
    ?title ?nb_runs ?nb_tries ?classifier
    ?reducer ?reduce_depth ?reduce_smaller ?random_src gen f spec =
    Test.add_random_test_group "Estimations"
      ?title ?nb_runs ?nb_tries ?classifier
      ?reducer ?reduce_depth ?reduce_smaller ?random_src gen f spec
  in

  let polynomial b = Gen.(list (make_int 2 10) (bfloat b)) in
  let poly_to_f = function
    | []      -> fun _ -> 0.0
    | h :: tl -> fun x -> List.fold_left (fun p v -> (p *. x) +. v) h tl
  in
  (* Compute the derivative of the polynomial symbolically *)
  let poly_to_df lst =
    List.rev lst
    |> List.tl
    |> List.rev
    |> List.mapi (fun i c -> (float (i + 1)) *. c)
    |> poly_to_f
  in
  (* TODO: Separate this into 2 cases, one where we know the derivative
    (ex, list of length 2) and not. *)
  add_random_test
    ~title:"Work on polynomials."
    Gen.(zip2 (polynomial 1e5) (bfloat 1e5))
    (fun (clst, x) ->
      let f = poly_to_f clst in
      let s = secant f in
      let o = second_order f in
      if List.length clst = 2 then
        let d_everywhere = List.nth clst 0 in
        equal_floats ~d:1e-3 d_everywhere (s x) &&
        equal_floats ~d:1e-3 d_everywhere (o x)
      else
        let d = poly_to_df clst in
        (* There are so many possible overflows with these two methods,
          perhaps it is easier to test that they're returning the 'same' thing.*)
        let r = Array.range ~start:(x -. 10.) ~stop:(x +. 10.) () in
        let cd = Array.map d r in
        let cs = Array.map s r in
        let co = Array.map o r in
        equal_floats ~d:1e-3 1.0 (D.correlation cs cd) &&
        equal_floats ~d:1e-3 1.0 (D.correlation co cd))
    Spec.([just_postcond_pred is_true]);

  ()

