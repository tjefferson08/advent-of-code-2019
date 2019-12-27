exception Invalid_input;

type space_object = {
  name: string,
  orbits: option(space_object)
}

type orbit = {
  orbiter_name: string,
  orbitee_name: string
};

type object_hashtable = Hashtbl.t(string, option(string));

/* let input_file = open_in(Filename.dirname(__FILE__) ++ "/example.txt"); */
let input_file = open_in(Filename.dirname(__FILE__) ++ "/input.txt");

let slurp_file = (input_channel) => {
  let rec helper = (lines) => {
    switch (input_line(input_channel)) {
    | line => helper([line, ...lines])
    | exception(End_of_file) => lines
    }
  }
    helper([])
};

let orbit_of_input_line = line => {
  let parts = Str.split(Str.regexp(")"), line);
  switch(parts) {
  | [] => raise(Invalid_input)
  | [_part] => raise(Invalid_input)
  | [orbitee, orbiter] => {orbiter_name: orbiter, orbitee_name: orbitee}
  | _ => raise(Invalid_input)
  }
};

let orbit_list = slurp_file(input_file)
|> List.map(orbit_of_input_line);

/* orbit_list |> List.iter(({ orbiter_name, orbitee_name }) => print_endline(orbiter_name ++ " orbits " ++ orbitee_name)); */

let objects = Hashtbl.create(1000);
Hashtbl.add(objects, "COM", None);

let add_object = (universe, new_orbit) => {
  let { orbiter_name, orbitee_name } = new_orbit;

  switch(Hashtbl.find(universe, orbitee_name)) {
  | exception Not_found => Hashtbl.add(universe, orbitee_name, None)
  | _ => ()
  };

  Hashtbl.replace(universe, orbiter_name, Some(orbitee_name));
};

let print_object = obj => {
  let orbitee_name = switch(obj.orbits) {
    | None => "Nothing"
    | Some(orbitee_obj) => orbitee_obj.name
  };
  print_endline(obj.name ++ " directly orbits " ++ orbitee_name);
};

orbit_list |> List.iter(orb => add_object(objects, orb));
/* Hashtbl.iter((k, v) => { */
/*   let orbiter_name = switch (v) { */
/*     | Some(name) => name */
/*     | None => "[null]" */
/*   }; */
/*   print_endline(k ++ " orbits " ++ orbiter_name); */
/* }, objects); */


let rec count_orbits_for_obj = (universe, obj) => {
  switch (Hashtbl.find(universe, obj)) {
  | None => 0
  | Some(orbitee_obj) => 1 + count_orbits_for_obj(universe, orbitee_obj)
  }
};

let all_orbits = Hashtbl.fold((k, _v, acc) => acc + count_orbits_for_obj(objects, k), objects, 0);

let rec print_orbit_chain = start => {
  switch (Hashtbl.find(objects, start)) {
  | Some(orbitee) => {
      print_endline(start ++ " orbits " ++ orbitee);
      print_orbit_chain(orbitee);
    }
  | None => print_endline("done");
  }
};

let rec orbit_chain_size = (start, stop) => {
  switch (Hashtbl.find(objects, start)) {
  | _ when start == stop => 0
  | Some(orbitee) => 1 + orbit_chain_size(orbitee, stop)
  | None => raise(Invalid_input)
  }
};

let rec list_of_orbit_chain = (start, acc) => {
  switch (Hashtbl.find(objects, start)) {
  | Some(orbitee) => list_of_orbit_chain(orbitee, [start, ...acc])
  | None => acc
  }
};

let you_chain = list_of_orbit_chain("YOU", []);
let santa_chain = list_of_orbit_chain("SAN", []);

let common_chain = List.filter((current_you_obj) => List.exists(current_santa_obj => current_you_obj == current_santa_obj, santa_chain), you_chain);

let closest_shared_obj = List.nth(common_chain, List.length(common_chain) - 1);
print_orbit_chain("YOU");
print_orbit_chain("SAN");
/* print_endline("direct: " ++ string_of_int(count_direct_orbits(obj_list))); */
print_orbit_chain(closest_shared_obj);

print_endline("you_chain size " ++ string_of_int(List.length(you_chain)));
print_endline("santa_chain size " ++ string_of_int(List.length(santa_chain)));
print_endline("common_chain size " ++ string_of_int(List.length(common_chain)));

print_endline("you_chain size " ++ string_of_int(orbit_chain_size("YOU", "COM")));
print_endline("santa_chain size " ++ string_of_int(orbit_chain_size("SAN", "COM")));
print_endline("YOU to CLOSEST size " ++ string_of_int(orbit_chain_size("YOU", closest_shared_obj)));
print_endline("SAN to CLOSEST size " ++ string_of_int(orbit_chain_size("SAN", closest_shared_obj)));
/* final answer is you to closest + san to closest - 2 */

/* print_endline("all " ++ string_of_int(all_orbits)); */
