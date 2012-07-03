(* MARTANI Fakhrou - http://martani.net - 2009 (modified April 2012) *)

(* 
To compile:
    - Windows:
        ocamlc -thread unix.cma threads.cma graphics.cma -o bee.exe bee.ml
    - Linux:
        ocamlc -thread unix.cma threads.cma graphics.cma -o bee bee.ml

- To change the size of the maze, change the parameters to the function "make_labyrinth width height" at the end of this file 
- To change the starting/end point change the parameters of the function "solve" as follows:
    solve laby start_x start_y end_x end_y
*)
 
open Graphics;;
 
type door = Left | Right | TopLeft | TopRight | BottomLeft | BottomRight;;
type cell = {mutable color : int ; mutable doors:door list};;
type labyrinth = {width : int; height : int; cells : cell array array};;
 
let new_labyrinth w h =
    { width = w; height = h; cells =
                Array.init w (fun y ->
                            Array.init h (fun x ->
                                {color = y * w + x + 10; doors = []}
                                    )
                        )
    };;
 
exception No_Cell of door;;
Random.self_init ();;
 
let open_door laby x y door =
    let cl = laby.cells.(x).(y) in
    if List.mem door cl.doors = false then
        cl.doors <- door::cl.doors;;
 
let door_opened laby x y door =
    let cl = laby.cells.(x).(y) in
    List.mem door cl.doors;;
    
let door_closed laby x y door =
    not (door_opened laby x y door);;
 
let neighbour laby x y door =
    let newx, newy =
        (*print_string "asking for : ";print_int x;print_string " | ";print_int y;print_string "\n";*)
        match door with
            Left -> if x <1 or x >= laby.width or y < 0 or y >= laby.height then raise (No_Cell Left)
                    else
                 x-1, y
            | Right -> if x < 0 or x >= laby.width - 1 or y < 0 or y >= laby.height then raise (No_Cell Left)
                    else
                 x + 1, y
            | BottomLeft -> if y < 1 or y >= laby.height or (x = 0 && y mod 2 = 0) or x < 0 or x >= laby.width then
                    raise (No_Cell Left)
                else
                (
                if y mod 2 = 0 then
                    x - 1, y - 1
                else
                    x, y-1
                )
            | BottomRight -> if y < 1 or y >= laby.height or x < 0 or (x = laby.width - 1 && y mod 2 = 1) or x > laby.width - 1 then
                    raise (No_Cell Left)
                else
                (
                if y mod 2 = 0 then
                    x, y - 1
                else
                    x + 1, y-1
                )
                    
            | TopLeft -> if y < 0 or y >= laby.height - 1 or (x = 0 && y mod 2 = 0) or x < 0 or x >= laby.width then raise (No_Cell Left)
                else
                (
                if y mod 2 = 0 then
                    x -1 , y + 1
                else
                    x, y+1
                )
                 
            | TopRight -> if y < 0 or y >= laby.height - 1 or x < 0 or (x = laby.width - 1 && y mod 2 = 1) or x > laby.width - 1 then raise (No_Cell Left)
                else
                (
                if y mod 2 = 0 then
                    x, y + 1
                else
                    x + 1, y + 1
                )
    in
    (*print_string "result for : ";print_int newx;print_string " | ";print_int newy;print_string "\n";*)
    newx, newy;;
    
let opposite door =
    match door with
        Left -> Right
        | Right -> Left
        | TopLeft -> BottomRight
        | BottomLeft -> TopRight
        | TopRight -> BottomLeft
        | BottomRight -> TopLeft;;
 
let all_doors = [Left; Right; TopLeft; BottomLeft; TopRight; BottomRight];;
 
let rec choose_door laby op =
    try
        let rnd_x = Random.int laby.width in
        let rnd_y = Random.int laby.height in
        let index = Random.int (List.length all_doors) in
        let dr = List.nth all_doors index in
        let _ = neighbour laby rnd_x rnd_y dr in
            op rnd_x rnd_y dr
    with
        _ -> choose_door laby op
    ;;
let rec change_color laby x y c =
    let cl = laby.cells.(x).(y) in
    cl.color <- c;
    
    List.iter (fun dr ->
                    try
                        let nbx, nby = neighbour laby x y dr in
                        if laby.cells.(nbx).(nby).color <> c then
                        (
                            change_color laby nbx nby c;
                        )
                    with
                    _ -> ()
                    ) cl.doors
    ;;

let connect laby x y d =
    try
        let xn, yn = neighbour laby x y d in
        let cl = laby.cells.(x).(y) in
        let cl_new = laby.cells.(xn).(yn) in
        if cl.color = cl_new.color then
            raise (No_Cell Left)
        else
            (
            change_color laby xn yn (cl.color);
            open_door laby x y d;
            open_door laby xn yn (opposite d);
            true
            )
    with
        _ -> false;;
 
let make_labyrinth width height =
    let laby = new_labyrinth width height in
    
    let colors_count = ref (width * height) in    
    while (!colors_count > 1) do
        if (choose_door laby (connect laby)) then
            colors_count := !colors_count - 1
    done;
    laby
;;
 
(********************************* graphics *****************************************)
open_graph " 700x600";;
set_line_width 1;;
 
let get_closed_doors lst_doors all_doors =
  List.fold_left (fun acc dr -> if List.mem dr lst_doors then acc else dr::acc) [] all_doors;;
 
(* draw cell doors *)
let draw_cell laby x y =
    let cl = laby.cells.(x).(y) in
    let cell_width_tmp = 600 / laby.width in
    let cell_height_tmp = 600 / laby.height in
    let cell_width = if cell_width_tmp < cell_height_tmp then cell_width_tmp else cell_height_tmp in
    set_color 4878475;
    List.iter (fun dr ->
                    match dr with
                    Left -> moveto (50 + cell_width * x + ((y mod 2) * cell_width / 2)) (50 + 3 * cell_width * y / 4 + cell_width / 4);
                            lineto (50 + cell_width * x + ((y mod 2) * cell_width / 2)) (50 + 3 * cell_width * y / 4 + 3 * cell_width / 4)
                            
                    |Right -> moveto (50 + cell_width * (x+1) + ((y mod 2) * cell_width / 2)) (50 + 3 * cell_width * y / 4 + cell_width / 4);
                            lineto (50 + cell_width * (x+1) + ((y mod 2) * cell_width / 2)) (50 + 3 * cell_width * y / 4 + 3 * cell_width / 4)
                            
                    |TopLeft -> moveto (50 + cell_width * x + ((y mod 2) * cell_width / 2)) (50 + 3 * cell_width * y / 4 + 3 * cell_width / 4);
                            lineto (50 + cell_width * x + ((y mod 2) * cell_width / 2) + cell_width / 2) (50 + 3 * cell_width * y / 4 + cell_width)
                            
                    |TopRight -> moveto (50 + cell_width * x + ((y mod 2) * cell_width / 2) + cell_width / 2) (50 + 3 * cell_width * y / 4 + cell_width);
                            lineto (50 + cell_width * (x+1) + ((y mod 2) * cell_width / 2)) (50 + 3 * cell_width * y / 4 + 3 * cell_width / 4)
                            
                    |BottomLeft -> moveto (50 + cell_width * x + ((y mod 2) * cell_width / 2)) (50 + 3 * cell_width * y / 4 + cell_width / 4);
                            lineto (50 + cell_width * x + ((y mod 2) * cell_width / 2) + cell_width / 2) (50 + 3 * cell_width * y / 4)
                            
                    |BottomRight -> moveto (50 + cell_width * x + ((y mod 2) * cell_width / 2) + cell_width / 2) (50 + 3 * cell_width * y / 4);
                            lineto (50 + cell_width * (x+1) + ((y mod 2) * cell_width / 2)) (50 + 3 * cell_width * y / 4 + cell_width / 4)
                    ) (get_closed_doors cl.doors all_doors);;
 
let color_cell laby x y =
    set_color (laby.cells.(x).(y).color);
    let cell_width_tmp = 600 / laby.width in
    let cell_height_tmp = 600 / laby.height in
    let cell_width = if cell_width_tmp < cell_height_tmp then cell_width_tmp else cell_height_tmp in
    let coordinates = [|
        (50 + cell_width * x + ((y mod 2) * cell_width / 2)), (50 + 3 * cell_width * y / 4 + cell_width / 4);
        (50 + cell_width * x + ((y mod 2) * cell_width / 2)), (50 + 3 * cell_width * y / 4 + 3 * cell_width / 4);
        (50 + cell_width * x + ((y mod 2) * cell_width / 2) + cell_width / 2), (50 + 3 * cell_width * y / 4 + cell_width);
        (50 + cell_width * (x+1) + ((y mod 2) * cell_width / 2)), (50 + 3 * cell_width * y / 4 + 3 * cell_width / 4);
        (50 + cell_width * (x+1) + ((y mod 2) * cell_width / 2)), (50 + 3 * cell_width * y / 4 + cell_width / 4);
        (50 + cell_width * x + ((y mod 2) * cell_width / 2) + cell_width / 2), (50 + 3 * cell_width * y / 4)
    |] in
    fill_poly coordinates;;
 
(* iterate through a list with the index of the current element *)
let iteri f l =
    let rec aux f i lst =
          match lst with
          [] -> ()
          |hd::tl -> f i hd; aux f (i+1) tl
    in aux f 0 l;;
    
(* make a uniform color of the maze *)
let reset_color laby =
    Array.iter (fun array_cell ->
            Array.iter (fun cl ->
                    cl.color <- 15461355
                    ) array_cell
            ) laby.cells;;
 
exception Solved;;
 
(* once a door is used, we delete it from the list of doors associated to a cell so that we don't go through it again *)
let delete_door doors dr =
    List.fold_left (fun acc x -> if x = dr then acc else x::acc ) [] doors;;
 
    
let solve laby public_x public_y ex ey =
    (*colorer les 2 cellules cible et destination*)
    laby.cells.(public_x).(public_y).color <- 0;
    laby.cells.(ex).(ey).color <- 0;
    color_cell laby public_x public_y;
    draw_cell laby public_x public_y;
    color_cell laby ex ey;
    draw_cell laby ex ey;
 
    try
        let rec aux x y ex ey allowed_doors =
            List.iter (fun dr -> try
                                    let nx, ny = neighbour laby x y dr in
                                    if nx = public_x && ny = public_y then
                                        raise Solved
                                with
                                    No_Cell x -> ()) allowed_doors;
            if x = ex && y = ey then
                raise Solved
            else
                (
                (* print_string "working : "; print_int x; print_string " | ";print_int y;print_string "\n"; *)
                laby.cells.(x).(y).color <- 13467442;
                color_cell laby x y;
                draw_cell laby x y;
                
                (*[fr] si la cellule a une seule porte alors elle est forcement pas dans le chemin de la solution,
                on retourne false dans ce cas, true si elle l'est. On change sa couleur *)
                if List.length laby.cells.(x).(y).doors = 1 && (x <> public_x or y <> public_y)then
                    (
                    laby.cells.(x).(y).color <- 6724044 ;
                    color_cell laby x y;
                    draw_cell laby x y;
                    (try
		      Thread.delay 0.02;
		    with
		      _ -> ());
		      
                    false
                    )
                else
                    (
                    let res = ref true in
                    iteri (fun i dr ->
                            try
                                let nx, ny = neighbour laby x y dr in
                                let allowed_drs = delete_door laby.cells.(nx).(ny).doors (opposite dr) in
                                
                                (*[fr] si la cellule suivante retourne false (pas dans le chemin) et on est dans la derniere porte
                                alors cette cellule aussi n'est pas dans le chemin *)
                                if (aux nx ny ex ey allowed_drs = false) && (List.length allowed_doors = i + 1) then
                                (
                                    laby.cells.(x).(y).color <- 6724044 ;
                                    laby.cells.(x).(y).color <- 6724044 ;
                                    res := false;
                                )
                                else
                                    res := true;
                                    
                                color_cell laby x y;
                                draw_cell laby x y;
                                (try
				  Thread.delay 0.02;
				 with
				  _ -> ());
                            with
                                No_Cell x -> ()
                        ) allowed_doors;
                        !res;
                    )
                )
        in
        let _ = aux public_x public_y ex ey laby.cells.(public_x).(public_y).doors in ()
    with
        Solved -> print_string "solved";;
 
        
let show_labyrinth laby =
    Array.iteri (fun i array_cell ->
            Array.iteri (fun j cl ->
                    color_cell laby i j;
                    draw_cell laby i j;
                    ) array_cell
            ) laby.cells;;
 
 
let laby = make_labyrinth 50 50;;
let _ = reset_color laby;;
 
show_labyrinth laby;; 
solve laby 0 0 32 43;;

read_line ();;