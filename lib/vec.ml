type 'a t = { mutable data: 'a array; mutable size: int; dummy: 'a }

let[@inline always] size { size; _ } = size
let[@inline always] capacity { data; _ } = Array.length data

let make ~dummy cap =
  let cap = max 10 cap in
  assert (cap < Sys.max_array_length);
  { data = Array.init cap (fun _ -> dummy); size = 0; dummy }

let of_array ~dummy arr =
  { data = Array.copy arr; size = Array.length arr; dummy }

let of_list ~dummy lst =
  { data = Array.of_list lst; size = List.length lst; dummy }

let shrink ({ data; size; dummy } as vec) i =
  if size < i then invalid_arg "shrink"
  else
    for j = size-i to size-1 do
      Array.unsafe_set data j dummy
    done;
    vec.size <- size-i

let pop ({ data; size; dummy } as vec) =
  if size <= 0 then invalid_arg "pop"
  else
    Array.unsafe_set data (size-1) dummy;
    vec.size <- size-1

let grow_to ({ data; size; dummy } as vec) ?pad cap =
  assert (cap < Sys.max_array_length);
  let pad = match pad with Some p -> p | None -> dummy in
  if cap > capacity vec then
    let data =
      Array.init cap (fun i ->
        if i < size then
          Array.unsafe_get data i
        else
          pad)
    in
    vec.data <- data

let push ({ size; _ } as vec) v =
  if size >= capacity vec then begin
    let cap = max 1 (capacity vec) in
    grow_to vec (2 * cap)
  end;
  Array.unsafe_set vec.data size v;
  vec.size <- size + 1

let clear ({ dummy; _ } as vec) =
  vec.data <- Array.init 10 (fun _ -> dummy);
  vec.size <- 0

let last { data; size; _ } =
  if size <= 0 then invalid_arg "last"
  else
    Array.unsafe_get data (size-1)

let get { data; size; _ } i =
  if i < 0 || i >= size then invalid_arg "get"
  else
    Array.unsafe_get data i

let set { data; size; _ } i v =
  if i < 0 || i >= size then invalid_arg "get"
  else
    Array.unsafe_set data i v

let find_first ~p vec =
  let elt = ref vec.dummy in
  try
    for i = 0 to size vec - 1 do
      elt := Array.unsafe_get vec.data i;
      if not (!elt == vec.dummy) && p !elt then
        raise Exit
    done;
    raise Not_found
  with Exit -> !elt

let remove ({ data; size; dummy } as vec) elt =
  try
    for i = 0 to size - 1 do
      if Array.unsafe_get data i = elt then begin
        Array.unsafe_set data i (last vec);
        Array.unsafe_set data (size-1) dummy;
        vec.size <- size-1;
        raise Exit
      end
    done;
    raise Not_found
  with Exit -> ()

let copy { data; size; dummy } =
  let data = Array.copy data in
  { data; size; dummy }

let to_array { data; size; _ } = Array.sub data 0 size

let to_list vec = to_array vec |> Array.to_list

let iteri ~f { data; size; dummy } =
  for i = 0 to size-1 do
    let elt = Array.unsafe_get data i in
    if not (elt == dummy) then
      f i elt
  done

let iter ~f vec = iteri ~f:(fun _ elt -> f elt) vec

exception Terminate

let exists ~f vec =
  try
    iter vec ~f:(fun elt ->
      if f elt then raise Terminate);
    false
  with Terminate -> true

let for_all ~f vec = not @@ exists ~f:(fun elt -> not @@ f elt) vec

let mem v vec = exists ~f:(fun elt -> elt == v) vec

let fold ~f ~init vec =
  let acc = ref init in
  iter vec ~f:(fun elt -> acc := f !acc elt);
  !acc

exception Cmp of int

let compare cmp
  ({ data = data1; size = size1; _ } as vec1)
  ({ data = data2; size = size2; _ } as vec2) =
  if vec1 == vec2 then 0
  else
    let c = size1-size2 in
    if c <> 0 then c
    else
      try
        for i = 0 to size1-1 do
          let c = cmp (Array.unsafe_get data1 i) (Array.unsafe_get data2 i) in
          if c <> 0 then raise (Cmp c)
        done;
        0
      with Cmp c -> c

let pp ?(sep=", ") pp_elt fmt vec =
  let pp_sep fmt () = Format.fprintf fmt "%s@," sep in
  Format.pp_print_list ~pp_sep pp_elt fmt (to_list vec)

let show ?sep pp_elt vec =
  Format.asprintf "%a" (pp ?sep pp_elt) vec
