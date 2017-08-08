fun hw4(inFile : string, outFile : string) =
let
    val inStream = TextIO.openIn inFile
    val outStream = TextIO.openOut outFile
    val readLine = TextIO.inputLine inStream
    val stk = []
    val env_id = 1
    val data_base : (int*string*string) list = []

        fun get_env_id(env_stk : (int*string*string) list) =
            case env_stk of
                []      => 1
              | x::xs   => #1 x

        fun convert_neg_zero(element : string) =
            if element = "-0" then "0" else element

        fun get_element(n : int, lst : string list) = 
            case lst of
                h::t => if n = 0 then h else get_element(n-1, t)

        fun is_double(element : string) = 
            let
                val element_chars = explode(element)
            in
                case element_chars of
                    []      => (false)
                  | x::xs   => (if x = #"." then true else is_double(implode(xs)))
            end

        fun convert_tilda(element : string) =
            let
                val element_chars = explode(element)
            in
                case element_chars of
                    x::xs   => (if x = #"~" then implode(#"-"::xs) else element)
            end

        fun is_quotation(c : char) = 
            let
                val ascii_c = ord c
            in
                if ascii_c = 226 orelse ascii_c = 34 then true else false
            end

        fun remove_quotations(element : string) =
            let
                val element_chars = explode(element)
                val first_char = case element_chars of x::xs => x
            in
                if is_quotation(first_char) then String.substring(element, 1, (String.size element)-2)
                else element
            end
            
        fun print_list(stk : string list) = 
            case stk of
                []      => (TextIO.output(outStream,""))
              | x::xs   => (TextIO.output(outStream, remove_quotations(convert_tilda(x))^"\n"); print_list(xs))

        fun error_chk(element1 : string, element2 : string) = (* ORELSE ISALPHA REMOVED *)
            let
                val element1_chars = explode(element1)
                val element2_chars = explode(element2)
            in
                case element1_chars of 
                    x::xs   => (if x = #":" orelse is_quotation(x) then true 
                                else case element2_chars of
                                    y::ys   => (if y = #":" orelse is_quotation(y) then true
                                else false))
            end

        fun only_numbers(element : string) =
            let
                val element_chars = explode(element)
            in
                case element_chars of
                    []      => (true)
                  | x::xs   => (if Char.isDigit x then only_numbers(implode(xs)) 
                                else false)
            end  

        fun is_int(element : string) = 
            let
                val element_chars = explode(element)
            in
                case element_chars of
                    []      => (false)
                  | x::xs   => (if x = #"-" then only_numbers(implode(xs))
                                else if Char.isDigit x then only_numbers(implode(xs)) 
                                else false)
            end 

        fun is_literal(element : string) =
            let
                val element_chars = explode(element)
            in
                case element_chars of
                    []      => (false)
                  | x::xs   => (is_quotation(x))
            end

        fun only_alphanumeric(element : string) =
            let
                val element_chars = explode(element)
            in
                case element_chars of
                    []      => (true)
                  | x::xs   => (if Char.isAlphaNum(x) then only_alphanumeric(implode(xs))
                                else false)
            end

        fun is_name(element : string) = 
            let
                val element_chars = explode(element)
            in
                case element_chars of
                    []      => (false)
                  | x::xs   => (if Char.isAlpha(x) then only_alphanumeric(implode(xs))
                                else false)
            end


        fun is_boolean(element : string) =
            case element of
                ":true:"    => true
              | ":false:"   => true
              | _           => false

        fun is_unit(element : string) =
            case element of
                ":unit:"    => true
              | _           => false

    (* THE WAY/ORDER THE FOLLOWING METHOD IS DEFINTED IS IMPORTANT, IT CHECKS FOR THE LATEST VALUE FOR A GIVEN STORED VARIABLE. *)
    (* THIS FUNCTIONALITY IS IMPORTANT FOR WHEN A VRIABLES VALUE IS UPDATED/CHANGED                                             *)
        fun get_pair(val_name : string, environments_left : (int*string*string) list) =
            case environments_left of
                []      => (0, "#NOT_FOUND", "#NOT_FOUND")
              | x::xs   => (if get_env_id(environments_left) = (#1 x) andalso val_name = (#2 x) then x else get_pair(val_name, xs))

        fun valid_binder(element :  string) = 
            if is_int(element) orelse is_literal(element) orelse is_boolean(element) orelse is_unit(element)
            then true else false

        fun bind(var_name : string, var_value : string, stk : string list, current_env : (int*string*string) list ) =
            let 
                val curr_env_id = get_env_id(current_env)
                val pair = (curr_env_id, var_name, var_value)
            in (":unit:"::stk, pair::current_env) end

        fun pair_value(pair : int*string*string) = 
            #3 pair
        
        fun invalid_var_chk(var1 : string, environments_stk : (int*string*string) list) =
            let
                val bool1 = is_name(var1)
                val var1_value = pair_value(get_pair(var1, environments_stk))
                val valid_var_bool = if var1_value="#NOT_FOUND" orelse is_literal(var1_value) 
                                     orelse is_boolean(var1_value) orelse is_unit(var1_value)
                                     then true else false 
            in
                if bool1 then valid_var_bool else false
            end

        fun get_sum(num1 : string, num2 : string, environments_stk : (int*string*string) list) =
            let
                val num1_opt = if is_name(num1) then Int.fromString(pair_value(get_pair(num1, environments_stk))) 
                                else Int.fromString(num1)
                val num1_int = Option.getOpt(num1_opt, 0)
                val num2_opt = if is_name(num2) then Int.fromString(pair_value(get_pair(num2, environments_stk))) 
                                else Int.fromString(num2)
                val num2_int = Option.getOpt(num2_opt, 0)
                val sum = num1_int + num2_int
            in
                Int.toString(sum)
            end

        fun get_difference(num1 : string, num2 : string, environments_stk : (int*string*string) list) =
            let
                val num1_opt = if is_name(num1) then Int.fromString(pair_value(get_pair(num1, environments_stk))) 
                                else Int.fromString(num1)
                val num1_int = Option.getOpt(num1_opt, 0)
                val num2_opt = if is_name(num2) then Int.fromString(pair_value(get_pair(num2, environments_stk))) 
                                else Int.fromString(num2)
                val num2_int = Option.getOpt(num2_opt, 0)
                val difference = num2_int - num1_int
            in
                Int.toString(difference)
            end
            
        fun get_product(num1 : string, num2 : string, environments_stk : (int*string*string) list) =
            let
                val num1_opt = if is_name(num1) then Int.fromString(pair_value(get_pair(num1, environments_stk))) 
                                else Int.fromString(num1)
                val num1_int = Option.getOpt(num1_opt, 0)
                val num2_opt = if is_name(num2) then Int.fromString(pair_value(get_pair(num2, environments_stk))) 
                                else Int.fromString(num2)
                val num2_int = Option.getOpt(num2_opt, 0)
                val product = num1_int * num2_int
            in
                Int.toString(product)
            end

        fun get_quotient(num1 : string, num2 : string, environments_stk : (int*string*string) list) =
            let
                val num1_opt = if is_name(num1) then Int.fromString(pair_value(get_pair(num1, environments_stk))) 
                                else Int.fromString(num1)
                val num1_int = Option.getOpt(num1_opt, 0)
                val num2_opt = if is_name(num2) then Int.fromString(pair_value(get_pair(num2, environments_stk))) 
                                else Int.fromString(num2)
                val num2_int = Option.getOpt(num2_opt, 0)
            in
                if num1_int = 0 then "#ZERO_ERROR" else Int.toString(Int.div(num2_int, num1_int))
            end

        fun get_remainder(num1 : string, num2 : string, environments_stk : (int*string*string) list) =
            let
                val num1_opt = if is_name(num1) then Int.fromString(pair_value(get_pair(num1, environments_stk))) 
                                else Int.fromString(num1)
                val num1_int = Option.getOpt(num1_opt, 0)
                val num2_opt = if is_name(num2) then Int.fromString(pair_value(get_pair(num2, environments_stk))) 
                                else Int.fromString(num2)
                val num2_int = Option.getOpt(num2_opt, 0)
                val remainder = Int.mod(num2_int, num1_int)
            in
                Int.toString(remainder)
            end

        fun negate(num : string, environments_stk : (int*string*string) list) =
            let
                val num_opt = if is_name(num) then Int.fromString(pair_value(get_pair(num, environments_stk))) 
                                else Int.fromString(num)
                val num_int = Option.getOpt(num_opt, 0)
                val negation = 0 - num_int
            in
                Int.toString(negation)
            end

        fun update_stk(stk : string list) =
            case stk of
                x::xs   => if x = "#LET_MARK" then xs
                            else update_stk(xs)

        fun remove_env(target_pair : (int*string*string), env_stk : (int*string*string) list) =
            case env_stk of
                x::xs   => if x = target_pair then xs
                            else remove_env(target_pair, xs)

        fun string_to_bool(bool_str : string) =
            case bool_str of
                ":true:"    => true
              | ":false:"   => false

        fun and_op(bool1 : string, bool2 : string) = 
            if string_to_bool(bool1) then 
                if string_to_bool(bool2) then ":true:" else ":false:"
            else ":false:"

        fun or_op(bool1 : string, bool2 : string) = 
            if string_to_bool(bool1) orelse string_to_bool(bool2) then ":true:" else ":false:"

        fun not_op(bool1 : string) =
            if string_to_bool(bool1) then ":false:" else ":true:"

        fun equal_op(num1 : string, num2 : string, environments_stk : (int*string*string) list) =
            let
                val num1_opt = if is_name(num1) then Int.fromString(pair_value(get_pair(num1, environments_stk))) 
                                else Int.fromString(num1)
                val num1_int = Option.getOpt(num1_opt, 0)
                val num2_opt = if is_name(num2) then Int.fromString(pair_value(get_pair(num2, environments_stk))) 
                                else Int.fromString(num2)
                val num2_int = Option.getOpt(num2_opt, 0)
                val bool1 = if num1_int = num2_int then true else false
            in
                if bool1 then ":true:" else ":false:"
            end

        fun lessThan_op(num1 : string, num2 : string, environments_stk : (int*string*string) list) =
            let
                val num1_opt = if is_name(num1) then Int.fromString(pair_value(get_pair(num1, environments_stk))) 
                                else Int.fromString(num1)
                val num1_int = Option.getOpt(num1_opt, 0)
                val num2_opt = if is_name(num2) then Int.fromString(pair_value(get_pair(num2, environments_stk))) 
                                else Int.fromString(num2)
                val num2_int = Option.getOpt(num2_opt, 0)
                val bool1 = if num1_int > num2_int then true else false
            in
                if bool1 then ":true:" else ":false:"
            end

        fun if_op(element1 : string, element2 : string, bool1 : string) =
            if string_to_bool(bool1) then element1 else element2


(*************************** HW4 METHODS ***********************************************************************************************)

        fun check_for_function(fun_name : string, environments_left : (int*string*string) list) =
            case environments_left of
                []      => ((0, "#NOT_FOUND", "#NOT_FOUND"), environments_left)
              | x::xs   => (if get_env_id(environments_left) = (#1 x) andalso fun_name = (#2 x) then (x, environments_left) 
                            else check_for_function(fun_name, xs))

        fun funct_contains_return(tokens : string list) = 
            case tokens of
                []      => (false)
              | x::xs   => (if x="return\n" then true else funct_contains_return(xs))


        fun run_function(f : string*string list*(int*string*string) list -> string list*(int*string*string) list,
                            tokens : string list, stk : string list, funct_envs : (int*string*string) list) =
            case tokens of 
                x::xs   => (let
                                val top_line_output = f(x, stk, funct_envs)
                                val updated_stk = #1 top_line_output
                                val updated_funct_envs = #2 top_line_output
                                val top_element = (case stk of [] => ("#EMPTY") | x::xs => (x))
                                val is_top_name = is_name(top_element)
                            in
                                case xs of
                                    []      => (if is_top_name then let
                                                    val name_value = #3 (get_pair(top_element, funct_envs))
                                                    val is_not_function_name = String.isPrefix "IN_OUT_CLOSURE_START#" name_value
                                                                    orelse String.isPrefix "CLOSURE_START#" name_value
                                                in
                                                    if is_not_function_name then updated_stk else name_value::[] 
                                                end
                                                else updated_stk) (* should be only one value *)
                                  | y::ys   => run_function(f, xs, updated_stk, updated_funct_envs)
                            end)

        fun run_inOut_function(f : string*string list*(int*string*string) list -> string list*(int*string*string) list,
                            tokens : string list, stk : string list, funct_envs : (int*string*string) list, parameter_name : string) =
            case tokens of 
                x::xs   => (let
                                val top_line_output = f(x, stk, funct_envs)
                                val updated_stk = #1 top_line_output
                                val updated_funct_envs = #2 top_line_output
                                val updated_param_value = #3 (get_pair(parameter_name, funct_envs))
                                val top_element = (case stk of [] => ("#EMPTY") | x::xs => (x))
                                val is_top_name = is_name(top_element)
                            in
                                case xs of
                                    []      => (if is_top_name then let
                                                    val name_value = #3 (get_pair(top_element, funct_envs))
                                                    val is_not_function_name = String.isPrefix "IN_OUT_CLOSURE_START#" name_value
                                                                    orelse String.isPrefix "CLOSURE_START#" name_value
                                                in
                                                    if is_not_function_name then (updated_stk, updated_param_value)
                                                        else (name_value::[], updated_param_value)
                                                end 
                                                else (updated_stk, updated_param_value)) (* should be only one value *)
                                  | y::ys   => run_inOut_function(f, xs, updated_stk, updated_funct_envs, parameter_name)
                            end)

        fun evaluate_line(line : string, stk : string list, environments_stk : (int*string*string) list) = 
            let
                val tokens_list = String.tokens Char.isSpace line
                val line_type = get_element(0, tokens_list)
                val is_fun_closure = case environments_stk of [] => false | x::xs => if (#3 x)="#FUN_CLOSURE" then true else false
            in
                if is_fun_closure then 
                    let
                        val stack_top = case stk of x::xs => x
                        val stack_rest = case stk of x::xs => xs
                        val updated_top = stack_top^line^"#"
                    in
                        case line_type of 
                        "funEnd" => 
                            (let
                                val fun_closure = stack_top^"end"
                                val top_3tuple = case environments_stk of x::xs => x
                                val rest_of_envs = case environments_stk of x::xs => xs
                                val top_id = #1 top_3tuple
                                val fun_name = #2 top_3tuple
                                val updated_3tuple = (top_id, fun_name, fun_closure)
                            in
                                (stack_rest, updated_3tuple::rest_of_envs)
                            end)
                        | _ => (updated_top::stack_rest, environments_stk)
                    end
                    

                else case line_type of
                    "push"      => (if is_double(get_element(1, tokens_list))
                                        then (":error:"::stk, environments_stk)
                                    else if is_int(get_element(1, tokens_list)) 
                                        then (convert_neg_zero(get_element(1, tokens_list))::stk, environments_stk)
                                    else if is_name(get_element(1, tokens_list)) 
                                        then (get_element(1, tokens_list)::stk, environments_stk)
                                    else if is_literal(get_element(1, tokens_list)) 
                                        then (get_element(1, tokens_list)::stk, environments_stk)
                                    else (":error:"::stk, environments_stk))

                  | "pop"       => (case stk of
                                        []      => ((":error:"::stk), environments_stk)
                                      | x::xs   => ((xs), environments_stk))
                  | ":true:"    => ((":true:"::stk), environments_stk)
                  | ":false:"   => ((":false:"::stk), environments_stk)
                  | ":error:"   => ((":error:"::stk), environments_stk)
                  | "add"       => (case stk of
                                        []      => ((":error:"::stk), environments_stk)
                                      | x::xs   => case xs of
                                                        []      => ((":error:"::stk), environments_stk)
                                                      | y::ys   => (if error_chk(x,y) then (":error:"::stk, environments_stk)
                                                                    else if invalid_var_chk(x, environments_stk)
                                                                        then (":error:"::stk, environments_stk)
                                                                    else if invalid_var_chk(y, environments_stk)
                                                                        then (":error:"::stk, environments_stk)
                                                                    else (get_sum(x, y, environments_stk)::ys, environments_stk)))
                  | "sub"       => (case stk of
                                        []      => ((":error:"::stk), environments_stk)
                                      | x::xs   => case xs of
                                                        []      => ((":error:"::stk), environments_stk)
                                                      | y::ys   => (if error_chk(x,y) then (":error:"::stk, environments_stk)
                                                                    else if invalid_var_chk(x, environments_stk)
                                                                        then (":error:"::stk, environments_stk)
                                                                    else if invalid_var_chk(y, environments_stk)
                                                                        then (":error:"::stk, environments_stk)
                                                                    else (get_difference(x, y, environments_stk)::ys, environments_stk)))
                  | "mul"       => (case stk of
                                        []      => ((":error:"::stk), environments_stk)
                                      | x::xs   => case xs of
                                                        []      => ((":error:"::stk), environments_stk)
                                                      | y::ys   => (if error_chk(x,y) then (":error:"::stk, environments_stk)
                                                                    else if invalid_var_chk(x, environments_stk)
                                                                        then (":error:"::stk, environments_stk)
                                                                    else if invalid_var_chk(y, environments_stk)
                                                                        then (":error:"::stk, environments_stk)
                                                                    else (get_product(x, y, environments_stk)::ys, environments_stk)))
                  | "div"       => (case stk of
                                        []      => ((":error:"::stk), environments_stk)
                                      | x::xs   => case xs of
                                                        []      => ((":error:"::stk), environments_stk)
                                                      | y::ys   => (if error_chk(x,y) then (":error:"::stk, environments_stk)
                                                                    else if invalid_var_chk(x, environments_stk)
                                                                        then (":error:"::stk, environments_stk)
                                                                    else if invalid_var_chk(y, environments_stk)
                                                                        then (":error:"::stk, environments_stk)
                                                                    else let
                                                                        val div_value = (get_quotient(x, y, environments_stk))
                                                                    in
                                                                        if div_value = "#ZERO_ERROR" orelse x = "0" 
                                                                        then (":error:"::stk, environments_stk)
                                                                        else (div_value::ys, environments_stk)
                                                                    end))
                  | "rem"       => (case stk of
                                        []      => ((":error:"::stk), environments_stk)
                                      | x::xs   => case xs of
                                                        []      => ((":error:"::stk), environments_stk)
                                                      | y::ys   => (if error_chk(x,y) then (":error:"::stk, environments_stk)
                                                                    else if invalid_var_chk(x, environments_stk)
                                                                        then (":error:"::stk, environments_stk)
                                                                    else if invalid_var_chk(y, environments_stk)
                                                                        then (":error:"::stk, environments_stk)
                                                                    else (get_remainder(x, y, environments_stk)::ys, environments_stk)))
                  | "neg"       => (case stk of
                                        []      => ((":error:"::stk), environments_stk)
                                      | x::xs   => (if is_int(x) then (negate(x, environments_stk)::xs, environments_stk)
                                                    else if is_name(x) then if invalid_var_chk(x, environments_stk)
                                                    then (":error:"::stk, environments_stk) else (negate(x, environments_stk)::xs, environments_stk)
                                                    else (":error:"::stk, environments_stk)))
                  | "swap"      => (case stk of
                                        []      => ((":error:"::stk), environments_stk)
                                      | x::xs   => case xs of
                                                        []      => ((":error:"::stk), environments_stk)
                                                      | y::ys   => ((y::x::ys), environments_stk))
                  | "and"       => (case stk of
                                        []      => ((":error:"::stk), environments_stk)
                                      | x::xs   => (case xs of
                                                        []      => ((":error:"::stk), environments_stk)
                                                      | y::ys   => let  val x_val = if is_name(x) then pair_value(get_pair(x, environments_stk))
                                                                                    else if is_boolean(x) then x
                                                                                    else "#NOT_BOOLEAN"
                                                                        val y_val = if is_name(y) then pair_value(get_pair(y, environments_stk))
                                                                                    else if is_boolean(x) then y
                                                                                    else "#NOT_BOOLEAN"
                                                                        val valid_vals = if is_boolean(x_val) then
                                                                                         if is_boolean(y_val) then
                                                                                         true else false else false
                                                                    in  if valid_vals then (and_op(x_val,y_val)::ys, environments_stk)
                                                                        else ((":error:"::stk), environments_stk) end))

                  | "or"       => (case stk of
                                        []      => ((":error:"::stk), environments_stk)
                                      | x::xs   => (case xs of
                                                        []      => ((":error:"::stk), environments_stk)
                                                      | y::ys   => let  val x_val = if is_name(x) then pair_value(get_pair(x, environments_stk))
                                                                                    else if is_boolean(x) then x
                                                                                    else "#NOT_BOOLEAN"
                                                                        val y_val = if is_name(y) then pair_value(get_pair(y, environments_stk))
                                                                                    else if is_boolean(x) then y
                                                                                    else "#NOT_BOOLEAN"
                                                                        val valid_vals = if is_boolean(x_val) then
                                                                                         if is_boolean(y_val) then
                                                                                         true else false else false
                                                                    in  if valid_vals then (or_op(x_val,y_val)::ys, environments_stk)
                                                                        else ((":error:"::stk), environments_stk) end))

                  | "not"       => (case stk of
                                        []      => ((":error:"::stk), environments_stk)
                                      | x::xs   => let  val x_val = if is_name(x) then pair_value(get_pair(x, environments_stk))
                                                                    else if is_boolean(x) then x
                                                                    else "#NOT_BOOLEAN"
                                                        val valid_val = if is_boolean(x_val) then true else false
                                                    in
                                                        if valid_val then (not_op(x_val)::xs, environments_stk)
                                                        else ((":error:"::stk), environments_stk) end)

                  | "equal"     => (case stk of
                                        []      => ((":error:"::stk), environments_stk)
                                      | x::xs   => (case xs of
                                                        []      => ((":error:"::stk), environments_stk)
                                                      | y::ys   => (if error_chk(x,y) then (":error:"::stk, environments_stk)
                                                                    else if invalid_var_chk(x, environments_stk)
                                                                        then (":error:"::stk, environments_stk)
                                                                    else if invalid_var_chk(y, environments_stk)
                                                                        then (":error:"::stk, environments_stk)
                                                                    else (equal_op(x, y, environments_stk)::ys, environments_stk))))

                  | "lessThan"  => (case stk of
                                        []      => ((":error:"::stk), environments_stk)
                                      | x::xs   => (case xs of
                                                        []      => ((":error:"::stk), environments_stk)
                                                      | y::ys   => (if error_chk(x,y) then (":error:"::stk, environments_stk)
                                                                    else if invalid_var_chk(x, environments_stk)
                                                                        then (":error:"::stk, environments_stk)
                                                                    else if invalid_var_chk(y, environments_stk)
                                                                        then (":error:"::stk, environments_stk)
                                                                    else (lessThan_op(x, y, environments_stk)::ys, environments_stk))))

                  | "if"        => (case stk of
                                        []      => ((":error:"::stk), environments_stk)
                                      | x::xs   => case xs of
                                        []      => ((":error:"::stk), environments_stk)
                                      | y::ys   => case ys of
                                        []      => ((":error:"::stk), environments_stk)
                                      | z::zs   => (if is_name(z) then let
                                                        val z_3tuple = (get_pair(z, environments_stk))
                                                    in
                                                        if is_boolean(#3 z_3tuple) then (if_op(x,y,(#3 z_3tuple))::zs, environments_stk)
                                                            else ((":error:"::stk), environments_stk)
                                                    end 
                                                    else if is_boolean(z) then (if_op(x,y,z)::zs, environments_stk)
                                                        else ((":error:"::stk), environments_stk)))
                    
                  | "bind"      => (case stk of
                                        []      => ((":error:"::stk), environments_stk)
                                      | x::xs   => (case xs of
                                                        []      => ((":error:"::stk), environments_stk)
                                                      | y::ys   => (if is_name(y) then 
                                                                        if valid_binder(x) then bind(y, x, ys, environments_stk)
                                                                        else let
                                                                            val curr_pair = get_pair(x, environments_stk)
                                                                            val curr_pair_value = pair_value(curr_pair)
                                                                        in
                                                                            if curr_pair = (0, "#NOT_FOUND", "#NOT_FOUND") 
                                                                            then (":error:"::stk, environments_stk) 
                                                                            else bind(y, curr_pair_value, ys, environments_stk)
                                                                        end
                                                                    else (":error:"::stk, environments_stk))))
                  | "let"       => (let 
                                        val env_id = 1 + get_env_id(environments_stk)
                                        val temp_pair = (env_id, "#REMOVE", "#REMOVE")
                                    in 
                                        ("#LET_MARK"::stk, temp_pair::environments_stk)
                                    end)
                  | "end"       => (let 
                                        val latest_entry = case stk of x::xs => x
                                        val temp_pair = (get_env_id(environments_stk), "#REMOVE", "#REMOVE")
                                    in 
                                        (latest_entry::(update_stk(stk)), remove_env(temp_pair, environments_stk)) 
                                    end)
                  | "fun"       => (let
                                        val env_id = get_env_id(environments_stk)
                                        val fun_name = get_element(1, tokens_list)
                                        val fun_arg = get_element(2, tokens_list)
                                        val fun_pair = (env_id, fun_name, "#FUN_CLOSURE")
                                    in
                                        ("CLOSURE_START#"^fun_arg^"#let#"::":unit:"::stk, fun_pair::environments_stk)
                                    end)
                  | "inOutFun"  => (let
                                        val env_id = get_env_id(environments_stk)
                                        val fun_name = get_element(1, tokens_list)
                                        val fun_arg = get_element(2, tokens_list)
                                        val fun_pair = (env_id, fun_name, "#FUN_CLOSURE")
                                    in
                                        ("IN_OUT_CLOSURE_START#"^fun_arg^"#let#"::":unit:"::stk, fun_pair::environments_stk)
                                    end)
                  | "call"      => (* TO DO:
                                        1. ERROR CHECKS
                                            * DO 2 VALUES BEFORE EXIST?
                                            * ARE ANY VALUES ABOVE ":error:"?
                                            * IS THE FUNCTION BINDING IN CURR ENVIRONMENT?
                                            * IF ARG IS A NAME, DOES THE BINDING EXIST IN THE SCOPE?
                                        2. SET ARGUMENT TO VARIABLE
                                        3. SET CORRECT ENVIRONMENT TO BE USED
                                        4. CALL METHOD
                                            * TOKENIZE CLOSURE BY "#"
                                            * SOMEHOW MAKE PROGRAM RUN CLOSURE
                                            * CHECK IF THERE IS A RETURN STATEMENT
                                                * IF YES THEN ALLOW "LET...END" DO ITS MAGIC
                                                * ELSE REMOVE WHAT WAS RETURNED BY LET...END
                                    *)
                                    (case stk of 
                                        []      => (":error:"::stk, environments_stk)
                                      | x::xs   => (if x=":error:" then (":error:"::stk, environments_stk)
                                        else case xs of 
                                            []      => (":error:"::stk, environments_stk)
                                          | y::ys   => (if y=":error:" then (":error:"::stk, environments_stk)
                                                        else 
                                                        let
                                                            val function_data = check_for_function(x, environments_stk)
                                                            val function_is_defined = String.isPrefix "IN_OUT_CLOSURE_START#" (#3 (#1 function_data))
                                                                                    orelse String.isPrefix "CLOSURE_START#" (#3 (#1 function_data))
                                                            val env_for_function = #2 function_data
                                                        in
                                                            if function_is_defined then 
                                                            let
                                                                val is_arg_name = is_name(y)
                                                                val function_closure = #3 (#1 function_data)
                                                                val closure_tokens = String.tokens (fn a => a = #"#") function_closure
                                                                val arg_name = get_element(1, closure_tokens)
                                                                val evnv_id_for_arg_pair = get_env_id(env_for_function)
                                                                val is_return_function = funct_contains_return(closure_tokens)
                                                                val function_stk : string list = []
                                                                val closure_tokens_without_flags = case closure_tokens of a::b::rest => rest
                                                                val not_inOut_function = if get_element(0, closure_tokens) = "IN_OUT_CLOSURE_START" then false else true
                                                                val param_name = get_element(1, closure_tokens)
                                                            in
                                                                if not_inOut_function then (* Program goes here only if regular "fun" was called *)

                                                                    if is_arg_name then 
                                                                    let
                                                                        val curr_pair = get_pair(y, environments_stk)
                                                                    in
                                                                        if curr_pair = (0, "#NOT_FOUND", "#NOT_FOUND") 
                                                                            then (":error:"::stk, environments_stk)
                                                                        else let
                                                                            val arg_value = #3 curr_pair
                                                                            val arg_3tuple = (evnv_id_for_arg_pair, arg_name, arg_value)
                                                                            val function_output_data = run_function(evaluate_line, closure_tokens_without_flags, function_stk, arg_3tuple::env_for_function)
                                                                        in
                                                                            if is_return_function then (function_output_data@ys, environments_stk)
                                                                            else (ys, environments_stk)
                                                                        end  
                                                                    end 

                                                                    else let
                                                                        val arg_value = y
                                                                        val arg_3tuple = (evnv_id_for_arg_pair, arg_name, arg_value)
                                                                        val function_output_data = run_function(evaluate_line, closure_tokens_without_flags, function_stk, arg_3tuple::env_for_function)
                                                                    in
                                                                        if is_return_function then (function_output_data@ys, environments_stk)
                                                                        else (ys, environments_stk)
                                                                    end

                                                                else (* Program goes here only if a "inOutFun" was called 
                                                                        The argument at time of "call" must be set to the parameters new modified value
                                                                        at the end of the method.

                                                                        get latest function parameter and thus its value.
                                                                        set its value to y and save

                                                                        so run_function needs to communicate back the updated parameter value
                                                                     *)

                                                                    if is_arg_name then 
                                                                    let
                                                                        val curr_pair = get_pair(y, environments_stk)
                                                                    in
                                                                        if curr_pair = (0, "#NOT_FOUND", "#NOT_FOUND") 
                                                                            then (":error:"::stk, environments_stk)
                                                                        else let
                                                                            val arg_value = #3 curr_pair
                                                                            val arg_3tuple = (evnv_id_for_arg_pair, arg_name, arg_value)
                                                                            val function_output_data = run_inOut_function(evaluate_line, closure_tokens_without_flags, function_stk, arg_3tuple::env_for_function, param_name)
                                                                            val env_id = get_env_id(environments_stk)
                                                                            val updated_arg_3tuple = (env_id, y, (#2 function_output_data))
                                                                        in
                                                                            if is_return_function then ((#1 function_output_data)@ys, updated_arg_3tuple::environments_stk)
                                                                            else (ys, updated_arg_3tuple::environments_stk)
                                                                        end  
                                                                    end 

                                                                    else let
                                                                        val arg_value = y
                                                                        val arg_3tuple = (evnv_id_for_arg_pair, arg_name, arg_value)
                                                                        val function_output_data = run_inOut_function(evaluate_line, closure_tokens_without_flags, function_stk, arg_3tuple::env_for_function, param_name)
                                                                        val env_id = get_env_id(environments_stk)
                                                                        val updated_arg_3tuple = (env_id, y, (#2 function_output_data))
                                                                    in
                                                                        if is_return_function then ((#1 function_output_data)@ys, updated_arg_3tuple::environments_stk)
                                                                        else (ys, updated_arg_3tuple::environments_stk)
                                                                    end

                                                            end

                                                            else (":error:"::stk, environments_stk)
                                                        end
                                    )))
                  | "return"    => (stk, environments_stk) (* might not be required due to the _ case already present but better safe than sorry *)
                  | "quit"      => let val _ = print_list(stk) in (stk, environments_stk) end
                  | _           => (stk, environments_stk) (* This accounts for the keywords "return" as well *)
            end

	fun helper(readLine : string option, (stk, data_base) : (string list)*((int*string*string) list)) =
		case readLine of
			NONE         => ( TextIO.closeIn inStream; TextIO.closeOut outStream)
		  | SOME(c)      => ( helper(TextIO.inputLine inStream, evaluate_line(c, stk, data_base)))
in
        helper(readLine, (stk, data_base))
end