/* --- Day 4: Secure Container --- */
/* You arrive at the Venus fuel depot only to discover it's protected by a
password. The Elves had written the password on a sticky note, but someone threw
it out. */

/* However, they do remember a few key facts about the password: */

/* It is a six-digit number. */
/* The value is within the range given in your puzzle input. */
/* Two adjacent digits are the same (like 22 in 122345). */
/* Going from left to right, the digits never decrease; they only ever increase
or stay the same (like 111123 or 135679). */

/* Other than the range rule, the following are true: */

/* 111111 meets these criteria (double 11, never decreases). */
/* 223450 does not meet these criteria (decreasing pair of digits 50). */
/* 123789 does not meet these criteria (no double). */
/* How many different passwords within the range given in your puzzle input meet
these criteria? */

/* Your puzzle input is 178416-676461. */

/* --- Part Two --- */
/* An Elf just remembered one more important detail: the two adjacent matching
digits are not part of a larger group of matching digits. */

/* Given this additional criterion, but still ignoring the range rule, the
following are now true: */

/* 112233 meets these criteria because the digits never decrease and all
repeated digits are exactly two digits long. */

/* 123444 no longer meets the criteria (the repeated 44 is part of a larger
group of 444). */

/* 111122 meets the criteria (even though 1 is repeated more than twice, it
still contains a double 22). */

/* How many different passwords within the range given in your puzzle input meet
all of the criteria? */

/*   Your puzzle input is still 178416-676461. */

let print_all = xs => {
  print_endline("processing");
  List.iter(i => print_endline(string_of_int(i)), xs);
}

let rec count_occurrences = (target_char, str) => {
  switch (String.get(str, 0)) {
  | chr => {
      let len = String.length(str);
      let rest_of_string = String.sub(str, 1, len - 1);
      chr == target_char ? 1 + count_occurrences(target_char, rest_of_string) : count_occurrences(target_char, rest_of_string)
    }
  | exception Invalid_argument(_) => 0
  }
};

let list_of_string = (str) => str |> Str.split(Str.regexp("")) |> List.map(str => String.get(str, 0));

// must be ADJACENT whoops
let check_for_non_adjacent_double = (pw_candidate) => {
  pw_candidate
  |> list_of_string
  |> List.map((current_digit) => count_occurrences(current_digit, pw_candidate))
  |> List.exists(count => count > 1)
};

let count_chunk_size = (str) => {
  let int_list = str
  |> list_of_string
  |> List.map(int_of_char);

  let rec helper = (xs) =>
    switch (xs) {
    | [] => 0
    | [x] => 1
    | [x, y] => x == y ? 2 : 1
    | [x, y, ...rest] => x == y ? 1 + helper([y, ...rest]) : 1
    };

  helper(int_list)
};

let chunk_sizes = (str) => {
  let int_list = str
  |> list_of_string
  |> List.map(int_of_char);

  let rec helper = (xs) => {
    /* print_endline("processing sub-list"); */
    /* print_all(xs); */
    switch (xs) {
    | [] => [0]
    | [x] => [1]
    | [x, y] => x == y ? [2] : [1, 1]
    | [x, y, ...rest] => {
        let [next_chunk, ...rest_of_chunks] = helper([y, ...rest]);
        /* print_endline("next"); */
        /* print_endline(string_of_int(next_chunk)); */
        /* print_endline("rest"); */
        /* print_all(rest_of_chunks); */
        if (x == y) {
          /* print_endline("inc chunk"); */
            [next_chunk + 1, ...rest_of_chunks]
        } else {
          /* print_endline("new chunk"); */
          [1, next_chunk, ...rest_of_chunks]
        }
      }
    };
  };

  helper(int_list)
};

let contains_chunk_size = (pw_candidate, chunk_size) => {
  pw_candidate
  |> list_of_string
  |> List.map(int_of_char)
  |> List.mapi((index, _current_int) => count_chunk_size(String.sub(pw_candidate, index, String.length(pw_candidate) - index)))
  |> List.fold_left((prev, acc) => prev > acc ? prev : acc, 0)
};

print_endline("chunks");
/* print_all(chunk_sizes("1122")); */
/* print_all(chunk_sizes("112233")); */
/* print_all(chunk_sizes("12345")); */
/* print_all(chunk_sizes("11111")); */
/* print_all(chunk_sizes("1122234")); */
/* print_all(chunk_sizes("112223344444123")); */

let check_for_adjacent_double = (pw_candidate) => {
  let int_list = pw_candidate
  |> list_of_string
  |> List.map(int_of_char)

  let rec has_double = xs =>
    switch (xs) {
    | [] => false
    | [x] => false
    | [x, y] => x == y
    | [x, y, ...rest] => has_double([x, y]) || has_double([y, ...rest])
    };
  has_double(int_list)
};

let check_for_increasing = (pw_candidate) => {
  let int_list = pw_candidate
  |> list_of_string
  |> List.map(int_of_char)

  let rec is_increasing = xs =>
    switch (xs) {
    | [] => raise(Invalid_argument("Empty list is invalid"))
    | [x] => true
    | [x, y] => x <= y
    | [x, y, ...rest] => x <= y && is_increasing([y, ...rest])
    };

  is_increasing(int_list)
};

let check_criteria = (number) => {
  let pw_candidate = string_of_int(number);
  check_for_increasing(pw_candidate) && chunk_sizes(pw_candidate) |> List.exists(size => size == 2)
};


// doubles and increasing => true
/* print_endline(string_of_bool(check_criteria(112233))); */

/* // no double, yes increasing => false */
/* print_endline(string_of_bool(check_criteria(123456))); */

/* // double and increasing => true */
/* print_endline(string_of_bool(check_criteria(123455))); */

/* // no double => false */
/* print_endline(string_of_bool(check_criteria(1))); */

/* // doubles but decreasing => false */
/* print_endline(string_of_bool(check_criteria(2222221))); */

// decreasing => false
/* print_endline(string_of_bool(check_for_adjacent_strict_double("233"))); */
/* print_endline(string_of_bool(check_for_adjacent_strict_double("333"))); */
/* print_endline(string_of_bool(check_for_adjacent_strict_double("332"))); */
/* print_endline(string_of_bool(check_for_adjacent_strict_double("123"))); */

/* print_endline(string_of_bool(check_for_adjacent_strict_double("3334"))); */
/* print_endline(string_of_bool(check_for_adjacent_strict_double("2332"))); */
/* print_endline(string_of_bool(check_for_adjacent_strict_double("23332"))); */

/* Your puzzle input is 178416-676461. */
let run = (start, stop) => {
  let rec helper = (current) => {
    print_endline("checking: " ++ string_of_int(current));
    switch (current) {
    | i when i > stop => 0
    | i => {
        let pass = check_criteria(i);
        if (pass) {
          print_endline("pass");
          1 + helper(i + 1)
        } else {
          print_endline("fail");
          helper(i + 1)
        }
      }
    }
  };
  helper(start)
};

let res = run(178416, 676461);
print_endline(string_of_int(res))
