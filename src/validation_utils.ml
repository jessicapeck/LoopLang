exception InternalPatternValidationError of string


(* extract the row number from the row *)
let extract_row_num row =
    try
        (* use a pattern match to capture the row number from the row identifier *)
        Scanf.sscanf row "R%d:" (fun n -> n)
    with
    | Scanf.Scan_failure _ -> 
        raise (InternalPatternValidationError (Printf.sprintf "the provided row '%s' was not of the expected format" row))


(* verify that the row number of the row is as expected *)
let check_row_num row expected_row_num =
    let actual_row_num = extract_row_num row in
    let valid = (actual_row_num = expected_row_num) in
    (valid, actual_row_num)
