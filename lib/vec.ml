type 'a t = { mutable data: 'a array; mutable size: int; dummy: 'a }

let[@inline always] uget a = Array.unsafe_get a 
let[@inline always] uset a = Array.unsafe_set a

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
  assert (0 <= i && i <= size);
  for j = size-i to size-1 do
    uset data j dummy
  done;
  vec.size <- size-i

let pop ({ data; size; dummy } as vec) =
  assert (size > 0);
  uset data (size-1) dummy;
  vec.size <- size-1

let grow_to ({ data; size; dummy } as vec) cap =
  assert (cap < Sys.max_array_length);
  if cap > capacity vec then
    let data =
      Array.init cap (fun i ->
        if i < size then
          uget data i
        else
          dummy)
    in
    vec.data <- data

let push ({ size; _ } as vec) v =
  if size >= capacity vec then begin
    let cap = max 1 (capacity vec) in
    grow_to vec (2 * cap)
  end;
  uset vec.data size v;
  vec.size <- size + 1

let clear ({ dummy; _ } as vec) =
  (* TODO: we should write a light version of this function. *)
  vec.data <- Array.init 10 (fun _ -> dummy);
  vec.size <- 0

let last { data; size; _ } =
  assert (size > 0);
  uget data (size-1)

let get { data; size; _ } i =
  assert (0 <= i && i < size);
  uget data i

let set { data; size; _ } i v =
  assert (0 <= i && i < size);
  uset data i v

let find_first p vec =
  let elt = ref vec.dummy in
  try
    for i = 0 to size vec - 1 do
      elt := uget vec.data i;
      if p !elt then
        raise_notrace Exit
    done;
    raise Not_found
  with Exit -> !elt

let remove ({ data; size; dummy } as vec) elt =
  try
    for i = 0 to size - 1 do
      if uget data i == elt then begin
        uset data i (last vec);
        uset data (size-1) dummy;
        vec.size <- size-1;
        raise_notrace Exit
      end
    done;
    raise Not_found
  with Exit -> ()

let copy { data; size; dummy } =
  let data = Array.copy data in
  { data; size; dummy }

let to_array { data; size; _ } = Array.sub data 0 size

let to_list vec = to_array vec |> Array.to_list

let iteri f { data; size; _ } =
  for i = 0 to size-1 do
    let elt = uget data i in
    f i elt
  done

let iter f vec = iteri (fun _ elt -> f elt) vec

exception Terminate

let exists f vec =
  try
    iter (fun elt ->
      if f elt then raise_notrace Terminate) vec;
    false
  with Terminate -> true

let for_all f vec = not @@ exists (fun elt -> not @@ f elt) vec

let mem v vec = exists (fun elt -> elt == v) vec

let fold f init vec =
  let acc = ref init in
  iter (fun elt -> acc := f !acc elt) vec;
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
          let u = uget data1 i in
          let v = uget data2 i in
          let c = cmp u v in
          if c <> 0 then raise_notrace (Cmp c)
        done;
        0
      with Cmp c -> c
