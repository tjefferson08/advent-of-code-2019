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

let check_for_double = (pw_candidate) => {
  pw_candidate
  |> list_of_string
  |> List.map((current_digit) => count_occurrences(current_digit, pw_candidate))
  |> List.exists(count => count > 1)
};

let check_criteria = (number) => {
  let pw_candidate = string_of_int(number);
  check_for_double(pw_candidate)
};

print_endline(string_of_bool(check_criteria(112233)));
print_endline(string_of_bool(check_criteria(123456)));
print_endline(string_of_bool(check_criteria(123455)));
print_endline(string_of_bool(check_criteria(1)));
