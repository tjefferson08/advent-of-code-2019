let input_file = open_in(Filename.dirname(__FILE__) ++ "/input.txt");
let input_contents = input_line(input_file);

let program = input_contents
  |> Str.split(Str.regexp(","))
  |> List.map(int_of_string)
  |> Array.of_list;

type op_result =
  | NumericResult(int)
  | Halt;

type three_op = { addr1: int, addr2: int, result_addr: int };
type operation =
  | Addition(three_op)
  | Multiplication(int, int, int)
  | HaltOp;

exception UnsupportedOperation;

let debug_result = (r, pc, p) => {

  switch (r) {
  | NumericResult(n) => print_endline("numeric res: " ++ string_of_int(n))
  | Halt => print_endline("halt")
  };

  Array.iter(i => print_string(string_of_int(i) ++ " "), p);
  print_endline("");
  print_endline("for pc: " ++ string_of_int(pc));
};

let execute_op = (program, pc) => {
  let opcode = program[pc];

  let result = switch(opcode) {
    | 1 => NumericResult(program[program[pc + 1]] + program[program[pc + 2]])
    | 2 => NumericResult(program[program[pc + 1]] * program[program[pc + 2]])
    | 99 => Halt
    | _ => raise(UnsupportedOperation)
  };

  // store numeric results
  switch(result) {
    | NumericResult(n) => Array.set(program, program[pc + 3], n)
    | _ => ()
  };

  (result, pc + 4)
};

/* let p = [|1,9,10,3,2,3,11,0,99,30,40,50|]; */
/* let p2 = [|2,3,0,3,99|]; */
/* let p3 = [|2,4,4,5,99,0|]; */
/* let p4 = [|1,1,1,4,99,5,6,0,99|]; */
/* let p5 = [|1,0,0,0,99|]; */

let rec run = (program, pc) => {
  switch (execute_op(program, pc)) {
  | (Halt, _) => ()
  | (_, new_pc) => run(program, new_pc)
  }
};
  /* run(p, 0); */
/* run(p2, 0); */
/* run(p3, 0); */
/* run(p4, 0); */
/* run(p5, 0); */

let try_inputs = (input1, input2) => {
  let new_program = Array.copy(program)
  new_program[1] = input1;
  new_program[2] = input2;
  run(new_program, 0);
  new_program[0]
}


let target = 19690720;

exception InvalidProgram;

let rec search = (i1, i2) => {
  let result = try_inputs(i1, i2);
  if (result == target) {
    (i1, i2)
  } else {
    if (i1 < 99) {
      search(i1 + 1, i2)
    } else if (i2 < 99) {
      search(0, i2 + 1)
    } else {
      raise(InvalidProgram)
    }
  }
}

let (noun, verb) = search(0, 0);
print_endline(string_of_int(noun) ++ ", " ++ string_of_int(verb) ++ ", " ++ string_of_int(noun * 100 + verb));
