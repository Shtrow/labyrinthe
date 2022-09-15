type block_scheme = 
    Curv
    | Line
    | Cross

type item = int

type angle =
    |North
    |South
    |East
    |West

type block = 
    {
        item : item option;
        block_scheme : block_scheme;
        angle : angle;
        is_static : bool
    }

type board = block list list

let board_size = 7,7

let to_array board = 
    List.map (Array.of_list ) board |> Array.of_list 

let to_string = function
    | North -> "North"
    | South -> "South"
    | East -> "East"
    | West -> "West"

let block_to_string (b : block) =
    match b.block_scheme, b.angle with
    |Curv, North -> "┌"
    |Curv, East -> "┐"
    |Curv, South -> "┘"
    |Curv, West -> "└"
    |Line, North|Line, South -> "─"
    |Line, East |Line, West -> "⎸"
    |Cross, North -> "┴"
    |Cross, East -> "├"
    |Cross, South -> "┬"
    |Cross, West -> "┤"

let get_exits block =
    let turn_direction_clock = function
        |North -> East
        |East -> South
        |South -> West
        |West -> North
    in
    let dir_to_int = function
        |North -> 0
        |East -> 1
        |South -> 2
        |West -> 3
    in
    let it = dir_to_int block.angle in 
    let rec turn nb angle = 
        if nb <= 0 then angle else
            turn (nb-1) (turn_direction_clock angle)
    in
    let exits_north = 
    match block.block_scheme with
    |Curv -> [East; South]
    |Line -> [West; East]
    |Cross -> [North; West; East]
    in
    exits_north |> List.map (turn (it))


let has_path (c1,b1) (c2,b2) = 
    let has_link_v a b ai bi= match a, b with
        |North,South when bi < ai -> true
        |South, North when ai < bi -> true
        | _ -> false
    in
    let has_link_h a b aj bj= match a, b with
        |East, West when bj > aj -> true
        |West, East when aj > bj -> true
        | _ -> false
    in
    let b1_exits = get_exits b1 in
    let b2_exits = get_exits b2 in
    let (c1i,c1j),(c2i,c2j) =c1,c2 in
    List.exists (fun dirb2 -> 
        List.exists (fun dirb1 ->
            ((has_link_h dirb1 dirb2 c1j c2j))
            ||
            ((has_link_v dirb1 dirb2 c1i c2i))

    )b1_exits) b2_exits

let next_blocks ((block): 'a * block) l: ('a * block )list= 
    List.filter (has_path block) l

module ListMonade = struct
    let join x =x |> List.concat |> List.sort_uniq (compare)
    let return v = [v]
    let empty = []
    let map = List.map
    let bind p f = join (map f p)
    let (let*) = bind
    let (>>=) = bind
end

let is_in_bounds (i,j) = 
    i >= 0 && i < 7 &&
    j >= 0 && j < 7 

let rec possible_moves (a_board : block array array) marked_list ((i,j) as current_block_ij) = 
    let open ListMonade in
    if List.exists ((=) current_block_ij) marked_list then marked_list
    else
    let current_block = (i,j),a_board.(i).(j) in
    let idxs =
    [
        i-1,j;
        i+1,j;
        i,j+1;
        i,j-1;
    ] |> List.filter is_in_bounds in
    let adj_block = List.map (fun ((i,j) as idx) -> idx, (a_board.(i).(j))) idxs in
    let* adj_idx,filtered_adj_block = next_blocks current_block adj_block in
    possible_moves a_board (current_block_ij::marked_list) adj_idx

(**TODO:This board is not correct*)
let init_board (seed : int) : board = 
    let width, height = (fst board_size), (snd board_size) in
    let last_h, last_w = (height -1), (width -1 )in
    List.init height (fun i ->
        List.init width (fun j -> 
            let base = {
                item = Some (i + 2*j);
                block_scheme = Cross;
                angle = North;
                is_static = true
            } 
            in 
            match i,j with 
            | 0,0 ->  {base with block_scheme = Curv; item = None}
            | 0, w when w = last_w ->  {base with block_scheme = Curv; angle = East; item = None}
            | h, 0 when h = last_h ->  {base with block_scheme = Curv; angle = West; item = None}
            | h, w when (h = last_h) && (w = last_w) ->  {base with block_scheme = Curv; angle = South; item = None}

            |0, 2 | 0, 4 -> {base with angle = South}
            |h, w when (h = last_h) && (w = 2 || w = 4 )-> {base with angle = North}
            |2, 0 | 4,0 -> {base with angle = East}
            |h, w when (w = last_h) && (h = 2 || h = 4 )-> {base with angle = West}

            |2,2 -> {base with angle = East}
            |2,4 -> {base with angle = South}
            |4,2 -> {base with angle = North}
            |4,4 -> {base with angle = West}
            | _ -> {base with item = None; block_scheme = Line; is_static = false}
        )
    )


let insert_block (block:block ) (index :int)(board :board): (board*block) =
    let simple_push l block = 
        let r = List.rev l in
        let last = List.hd r in
        let n_l = List.tl r |> List.rev in
        block::n_l,last
    in
    let back_push l block = 
        let last, n_l = List.hd l, List.tl l in
        let n_l = block::(List.rev n_l) |> List.rev
        in n_l,last
    in
    let get_row index = 
        List.fold_left(
            fun l line -> match List.nth_opt line index with
            | None -> failwith "TODO"
            | Some (el) -> el::l
        ) [] board
    in 
    let replace_row index new_row = 
        let _, n_board = List.fold_left_map(fun row_stack line ->
            match row_stack with
            | [] -> failwith "empty row"
            | hd::tl ->
            (tl,
            List.mapi(fun j el -> 
                if j == index then hd else el
            ) line) 
        ) new_row board
    in n_board in
    if index > 0 && index < 7 then
        begin
            let l = List.nth_opt board index in
            match l with
            (*| None-> failwith "TODO"*)
            | None-> board,block
            (*| Some(h::t) when h.is_static -> failwith "TODO : static bloc"*)
            | Some(h::t) when h.is_static -> board,block
            | Some(l) -> let n_l,last = simple_push l block in 
                List.mapi (fun i _l -> if i == index then n_l else _l) board,last
        end
    else if index > 14 && index < 20 then
        begin
            let n_index = 20-index in
            let l = List.nth_opt board n_index in
            match l with
            (*| None-> failwith "TODO"*)
            | None-> board,block
            (*| Some(h::t) when h.is_static -> failwith "TODO : static bloc"*)
            | Some(h::t) when h.is_static -> board,block
            | Some(l) -> let n_l,last = back_push l block in 
                List.mapi (fun i _l -> if i == n_index then n_l else _l) board,last
        end
    else if index > 7 && index < 14 then
         let n_idx = index - 7 in
         let row = get_row n_idx |> List.rev in 
         print_newline ();
         List.iter (fun b -> block_to_string b |> print_endline) row;
         let n_row, last = back_push row block in
         let n_board = replace_row n_idx n_row in
         n_board, last
    else if index > 21 && index < 27 then
         let n_idx = 7-(index -20) in
         let row = get_row n_idx |> List.rev in 
         print_newline ();
         List.iter (fun b -> block_to_string b |> print_endline) row;
         let n_row, last = simple_push row block in
         let n_board = replace_row n_idx n_row in
         n_board, last
    else board,block

let print_board (b : board) : unit =
    List.iteri (fun i arr ->
        print_newline();
        List.iteri (fun j block ->
        begin
            print_string (block_to_string block)
        end
        ) arr
    ) b

(*TESTING*)
let possible_case board position = 
    let a_board = to_array board in
    possible_moves a_board [] position 

let my_board = init_board 0;;
let my_bloc= {
    item = None;
    block_scheme = Curv;
    angle = West;
    is_static = false
} ;;
let () = print_board my_board;;
(*let _ = print_board (insert_block my_bloc 24 my_board |> fst |>insert_block my_bloc 24 |> fst|>insert_block my_bloc 19 |> fst);;*)
let _ = possible_case my_board (6,3) |> List.iter (fun (a,b) -> Printf.printf "%d , %d\n" a b)

