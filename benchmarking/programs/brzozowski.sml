signature BMARK =
  sig
    val doit : int -> unit
    val testit : unit -> unit
  end

structure Main : BMARK =
    struct

    datatype regex =
    EmptySet
    |EmptyString
    |Character of string
    |Union of regex * regex
    |Concat of regex * regex
    |Star of regex
    |And of regex * regex
    |Not of regex

    fun nullable r =
    case r of
    EmptySet => false
    |EmptyString => true
    |Character(_) => false
    |Union(r, s) => (nullable r) orelse (nullable s)
    |Concat(r, s) => (nullable r) andalso (nullable s)
    |Star(_) => true
    |And(r, s) => (nullable r) andalso (nullable s)
    |Not(r) => not (nullable r)

    fun derive r c =
    case r of
        EmptySet => EmptySet
        | EmptyString => EmptySet
        | Character(c2) => (if c = c2 then EmptyString
                        else EmptySet)
        | Union(r, s) => Union (derive r c, derive s c)
        | Concat(r, s) => (if nullable r then
                            Union (Concat (derive r c, s), derive s c)
                        else
                            Concat(derive r c, s)
                        )
        | Star(r) => Concat (derive r c,  Star(r))
        | And(r, s) => And(derive r c, derive r c)
        | Not(r) => Not(derive r c)

    fun fold_left f acc l =
        case l of
            [] => acc
            | x::xs => fold_left f (f acc x) xs

    fun map f l =
        case l of
            [] => []
            | x::xs => f x :: map f xs

    fun regexmatch r char_list = nullable (fold_left derive r char_list)

    fun union_list l = fold_left (fn acc => fn c => Union(acc, c)) EmptySet l
    fun concat_list l = fold_left (fn acc => fn c => Concat(acc, c)) EmptyString l

    fun to_characters l = map (fn c => Character c) l

    val lowercase_letter = union_list (to_characters ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"])
    val digit = union_list (to_characters ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"])

    fun one_or_more r = Concat(r, Star(r))

    val test_regex =
        Concat(
            one_or_more (union_list [lowercase_letter, digit, Character "-", Character "_", Character "."]),
            Concat(
                Character "@",
                Concat(
                    one_or_more (union_list [lowercase_letter, digit]),
                    Concat (
                        Character ".",
                        Union(
                            one_or_more lowercase_letter,
                            Character "."
                        )
                    )
                )
            )
        )

    fun doit_once () = regexmatch test_regex
        ["c", "s", "t", "-", "g", "r", "a", "d", "u", "a", "t", "e", "-", "a", "d", "m", "i", "s", "s", "i", "o", "n", "s", "@", "c", "s", "t", ".", "c", "a", "m", ".", "a", "c", ".", "u", "k"]
    val doit = fn size =>
        let
            fun loop n =
                if n = 0 then ()
                else (doit_once(); loop(n-1))
        in
            loop size
        end

    fun testit () = (
        print(Bool.toString (regexmatch test_regex ["a"]));
        print(Bool.toString (regexmatch test_regex ["a", "b"]));
        print(Bool.toString (regexmatch test_regex ["a", "c"]));
        print(Bool.toString (regexmatch test_regex ["a", "d"]));
        print(Bool.toString (regexmatch test_regex ["a", "b", "b"]))
    )

end