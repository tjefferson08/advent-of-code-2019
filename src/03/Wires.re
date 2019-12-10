type coord = (int, int)
type coord_hashtable = Hashtbl.t(coord, int);

type direction =
  | Down
  | Up
  | Left
  | Right

type segment = (direction, int)
exception InvalidInput;

let input_file = open_in(Filename.dirname(__FILE__) ++ "/input.txt");
let line_1 = input_line(input_file);
let line_2 = input_line(input_file);

let segment_of_string = str => {
  let dir = switch(str -> String.sub(0, 1)) {
    | "D" => Down
    | "U" => Up
    | "L" => Left
    | "R" => Right
    | _ => raise(InvalidInput)
    };

  let str_len = str |> String.length;
  print_endline(str);
  let segment_length = String.sub(str, 1, str_len - 1) |> int_of_string;

  (dir, segment_length)
};

let segment_list_of_line = line => {
  line |> Str.split(Str.regexp(",")) |> List.map(segment_of_string);
};

let segment_list_1 = segment_list_of_line(line_1);
let segment_list_2 = segment_list_of_line(line_2);

let next_coord = (origin, segment) => {
  let (x, y) = origin;

  switch (segment) {
  | (_, len) when len <= 0 => (origin, None)
  | (Down, len) => ((x, y - 1), Some((Down, len - 1)))
  | (Up, len) => ((x, y + 1), Some((Up, len - 1)))
  | (Right, len) => ((x + 1, y), Some((Right, len - 1)))
  | (Left, len) => ((x - 1, y), Some((Left, len - 1)))
  };
};

let rec track_wire = (origin, segment_list, store) => {
  switch (segment_list) {
  | [] => store
  | [segment, ...rest] => {
      Hashtbl.replace(store, origin, 1);

      switch (next_coord(origin, segment)) {
      | (new_origin, Some(partial_segment)) => track_wire(new_origin, [partial_segment, ...rest], store)
      | (_, None) => track_wire(origin, rest, store)
      }
    }
  }
};

let rec track_wire_2 = (origin, segment_list, wire_length, store) => {
  switch (segment_list) {
  | [] => store
  | [segment, ...rest] => {
      switch (Hashtbl.find(store, origin)) {
      | _ => ();
      | exception Not_found => Hashtbl.replace(store, origin, wire_length);
      }

      switch (next_coord(origin, segment)) {
      | (new_origin, Some(partial_segment)) => track_wire_2(new_origin, [partial_segment, ...rest], wire_length + 1, store)
      | (_, None) => track_wire_2(origin, rest, wire_length, store)
      }
    }
  }
};

let rec get_intersections = (origin, segment_list, hsh_store, acc) => {
  switch (segment_list) {
  | [] => acc
  | [segment, ...rest] => {

      let new_acc = switch ((origin, Hashtbl.find(hsh_store, origin))) {
        | ((0, 0), _) => acc
        | (_, _) => [origin, ...acc]
        | exception Not_found => acc
      };

      switch (next_coord(origin, segment)) {
      | (new_origin, Some(partial_segment)) => get_intersections(new_origin, [partial_segment, ...rest], hsh_store, new_acc)
      | (_, None) => get_intersections(origin, rest, hsh_store, new_acc)
      }
    }
  }
};

let rec get_intersections_2 = (origin, segment_list, hsh_store, wire_length, acc) => {
  switch (segment_list) {
  | [] => acc
  | [segment, ...rest] => {

      let new_acc = switch ((origin, Hashtbl.find(hsh_store, origin))) {
        | ((0, 0), _) => acc
        | (_, other_wire_length) => [(origin, wire_length, other_wire_length), ...acc]
        | exception Not_found => acc
      };

      switch (next_coord(origin, segment)) {
      | (new_origin, Some(partial_segment)) => get_intersections_2(new_origin, [partial_segment, ...rest], hsh_store, wire_length + 1, new_acc)
      | (_, None) => get_intersections_2(origin, rest, hsh_store, wire_length, new_acc)
      }
    }
  }
};

let manhattan_distance = ((x, y)) => abs(x) + abs(y);
let pick_closer = (c1, c2) => manhattan_distance(c1) > manhattan_distance(c2) ? c2 : c1;

let rec find_closest_intersection = (coord_list: list(coord)) => {
  switch (coord_list) {
  | exception InvalidInput => raise(InvalidInput);
  | [] => raise(InvalidInput);
  | [c] => c
  | [c, ...rest] => pick_closer(c, find_closest_intersection(rest))
  }
};

let rec find_minimum_distance = (coord_list: list((coord, int, int))) => {
  switch (coord_list) {
  | exception InvalidInput => raise(InvalidInput);
  | [] => raise(InvalidInput);
  | [(c, len1, len2)] => len1 + len2
  | [(c, len1, len2), ...rest] => {
      let min_of_rest = find_minimum_distance(rest);
      len1 + len2 > min_of_rest ? min_of_rest : len1 + len2
    }
  }
};
let run = (seglist1, seglist2) => {
  let hsh: coord_hashtable = Hashtbl.create(1000);
  let hsh_store = track_wire((0, 0), seglist1, hsh);
  print_endline("hsh store created");
  let intersections = get_intersections((0, 0), seglist2, hsh_store, []);
  print_endline("intersections list created");
  print_endline("length: " ++ string_of_int(List.length(intersections)));
  intersections |> List.iter(((x, y)) => print_endline("x: " ++ string_of_int(x) ++ ", y: " ++ string_of_int(y) ++ ", dis: " ++ string_of_int(manhattan_distance((x, y)) )));
  let closest = find_closest_intersection(intersections);
  print_endline("min distance: " ++ string_of_int(manhattan_distance(closest)));
};

let run2 = (seglist1, seglist2) => {
  let hsh: coord_hashtable = Hashtbl.create(1000);
  let hsh_store = track_wire_2((0, 0), seglist1, 0, hsh);
  print_endline("hsh store created");
  let intersections = get_intersections_2((0, 0), seglist2, hsh_store, 0, []);
  print_endline("intersections list created");
  print_endline("length: " ++ string_of_int(List.length(intersections)));
  intersections |> List.iter((((x, y), len1, len2)) => print_endline("x: " ++ string_of_int(x) ++ ", y: " ++ string_of_int(y) ++ ", len: " ++ string_of_int(len1 + len2)));
  let min = find_minimum_distance(intersections);
  print_endline("min distance: " ++ string_of_int(min));
};


let segment_list_test_A1 = segment_list_of_line("R75,D30,R83,U83,L12,D49,R71,U7,L72");
let segment_list_test_A2 = segment_list_of_line("U62,R66,U55,R34,D71,R55,D58,R83");
/* run2(segment_list_test_A1, segment_list_test_A2); */

let segment_list_test_B1 = segment_list_of_line("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51");
let segment_list_test_B2 = segment_list_of_line("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7");
/* run2(segment_list_test_B1, segment_list_test_B2); */

run2(segment_list_1, segment_list_2);
