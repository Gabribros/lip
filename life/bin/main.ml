module T = ANSITerminal
open Life.Main

let _ = match Array.length(Sys.argv) with
    2 -> let k = int_of_string (Sys.argv.(1)) in
    T.erase T.Screen;
    T.save_cursor();
    Random.self_init();
    let w = loop init_w k (Rule([2;3],[3])) in
    display w;
    ignore(read_line());
    T.restore_cursor();
    print_newline()
    |3 -> let k = int_of_string (Sys.argv.(1)) in
    let sb = Sys.argv.(2) in
    T.erase T.Screen;
    T.save_cursor();
    Random.self_init();
    let rl = parse sb in
    let w = loop init_w k rl in 
    display w;
    ignore(read_line());
    T.restore_cursor();
    print_newline()
  (* wrong usage *)
  | _ -> failwith "Usage: dune exec life n_rounds"
