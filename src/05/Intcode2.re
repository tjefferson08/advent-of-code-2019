let input_file = open_in(Filename.dirname(__FILE__) ++ "/input.txt");
let input_contents = input_line(input_file);

let program = input_contents
  |> Str.split(Str.regexp(","))
  |> List.map(int_of_string)
  |> Array.of_list;

type parameter_mode =
  | ImmediateMode
  | PositionMode

type parameter =
  | Immediate(int)
  | Position(int);

type operation =
  | Addition(parameter_mode, parameter_mode, parameter_mode)
  | Multiplication(parameter_mode, parameter_mode, parameter_mode)
  | StoreInput
  | OutputStoredValue(parameter_mode)
  | HaltOp;

exception UnsupportedOperation(int);

let debug_on = false;
let debug = debug_on ? print_endline : (_) => ();

let print_all = int_arr => Array.to_list(int_arr) |>  List.iteri((index, i) => debug("[" ++ string_of_int(index) ++ "]: " ++ string_of_int(i)));

let operation_of_raw_opcode = (opcode) => {
  debug("parsing opcode: " ++ string_of_int(opcode));
  let param_mode_of_int = (mode) => mode == 1 ? ImmediateMode : PositionMode;

  let param_mode_3 = param_mode_of_int(opcode / 10000);
  let param_mode_2 = param_mode_of_int((opcode mod 10000) / 1000);
  let param_mode_1 = param_mode_of_int((opcode mod 1000) / 100);
  debug("param modes: " ++ string_of_int((opcode mod 1000) / 100) ++ ", " ++ string_of_int((opcode mod 10000) / 1000) ++ ", " ++ string_of_int(opcode / 10000));
  debug("opcode: " ++ string_of_int(opcode mod 100));

  switch (opcode mod 100) {
  | 1 => Addition(param_mode_1, param_mode_2, param_mode_3)
  | 2 => Multiplication(param_mode_1, param_mode_2, param_mode_3)
  | 3 => StoreInput
  | 4 => OutputStoredValue(param_mode_1)
  | 99 => HaltOp
  | opcode => raise(UnsupportedOperation(opcode))
  };
};

let access_param = (program, pc, offset, mode) => {
  let raw_value = program[pc + offset];

  switch (mode) {
  | ImmediateMode => raw_value
  | PositionMode => program[raw_value]
  }
}

let execute_op = (program, pc) => {
  let operation = operation_of_raw_opcode(program[pc]);

  let partial_access = access_param(program, pc);
  let result = switch(operation) {
    | Addition(pm1, pm2, _pm3) => partial_access(1, pm1) + partial_access(2, pm2)
    | Multiplication(pm1, pm2, _pm3) => partial_access(1, pm1) * partial_access(2, pm2)
    | StoreInput => partial_access(1, ImmediateMode) // address to store stdin value
    | OutputStoredValue(pm1) => partial_access(1, pm1) // value/address (depending on mode) to send to stdout
    | _ => 0
  };

  debug("result: " ++ string_of_int(result));

  // Side effects (storage, IO)
  switch(operation) {
  | Addition(_) => Array.set(program, program[pc + 3], result);
  | Multiplication(_) => Array.set(program, program[pc + 3], result);
  | StoreInput => {
      let input_to_store = read_int();
      Array.set(program, result, input_to_store);
    }
  | OutputStoredValue(_) => print_endline(string_of_int(result));
  | _ => ()
  };

  let new_pc = switch(operation) {
    | Addition(_) => pc + 4
    | Multiplication(_) => pc + 4
    | StoreInput => pc + 2
    | OutputStoredValue(_) => pc + 2
    | HaltOp => pc
  };

  (operation, result, new_pc)
};

/* let p = [|1,9,10,3,2,3,11,0,99,30,40,50|]; */
/* let p = [|2,3,0,3,99|]; */
/* let p = [|2,4,4,5,99,0|]; */
/* let p = [|1,1,1,4,99,5,6,0,99|]; */
/* let p = [|1,0,0,0,99|]; */
/* let p = [| 1002,4,3,4,99,33 |]; */
/* let p = [|3,0,4,0,99|]; */

let p = program;

let rec run = (program, pc) => {
  switch (execute_op(program, pc)) {
  | (HaltOp, _, _) => ()
  | (_, _, new_pc) => {
      debug("*******************************");
      print_all(program);
      run(program, new_pc)
    }
  | exception e => {
      debug("resulting data at time of err:");
      print_all(p);
      raise(e);
    }
  }
};

run(p, 0);
debug("resulting data:");
print_all(p);
/* run(p2, 0); */
/* run(p3, 0); */
/* run(p4, 0); */
/* run(p5, 0); */

/* let try_inputs = (input1, input2) => { */
/*   let new_program = Array.copy(program) */
/*   new_program[1] = input1; */
/*   new_program[2] = input2; */
/*   run(new_program, 0); */
/*   new_program[0] */
/* } */


/* let target = 19690720; */

/* exception InvalidProgram; */

/* let rec search = (i1, i2) => { */
/*   let result = try_inputs(i1, i2); */
/*   if (result == target) { */
/*     (i1, i2) */
/*   } else { */
/*     if (i1 < 99) { */
/*       search(i1 + 1, i2) */
/*     } else if (i2 < 99) { */
/*       search(0, i2 + 1) */
/*     } else { */
/*       raise(InvalidProgram) */
/*     } */
/*   } */
/* } */

/* let (noun, verb) = search(0, 0); */
/* debug(string_of_int(noun) ++ ", " ++ string_of_int(verb) ++ ", " ++ string_of_int(noun * 100 + verb)); */
