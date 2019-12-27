/* procedure generate(k : integer, A : array of any): */
/*   if k = 1 then */
/*   output(A) */
/*   else */
/* // Generate permutations with kth unaltered */
/* // Initially k == length(A) */
/*   generate(k - 1, A) */

/* // Generate permutations for kth swapped with each k-1 initial */
/*   for i := 0; i < k-1; i += 1 do */
/* // Swap choice dependent on parity of k (even or odd) */
/*   if k is even then */
/*   swap(A[i], A[k-1]) // zero-indexed, the kth is at k-1 */
/*   else */
/*   swap(A[0], A[k-1]) */
/*   end if */
/*   generate(k - 1, A) */

/*   end for */
/*   end if */

let arr = [|1, 2, 3|];
let print_all = arr => {
  Array.iter(
  item => {
    item |> print_int;
    print_string(" ");
  }, arr);
  print_endline("");
};

let swap = (arr, i, j) => {
  let temp = arr[i];
  arr[i] = arr[j];
  arr[j] = temp;
};

let rec permute = (k, arr) => {
  if (k == 1) {
    print_all(arr)
  } else {
    permute(k - 1, arr);
    for (i in 0 to k - 2) {
    if (k mod 2 == 0) {
      swap(arr, i, k-1);
    } else {
      swap(arr, 0, k-1);
    }
      permute(k-1, arr);
    }
  }
};

permute(5, [|0, 1, 2, 3, 4|]);
