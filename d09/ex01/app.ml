module App : sig
    type project = string * string * int
    val zero : project
    val combine : project -> project -> project
    val fail : project -> project
    val success : project -> project
end = struct

    type project = string * string * int
    let zero = ("", "", 0)
    let combine (name1, status1, g1) (name2, status2, g2) =
        let avge = (g1 + g2) / 2 in
        let status = if avge >= 80 then "succeed" else "failed" in
        (name1 ^ name2, status, avge)
    let fail (n, _, _) = (n, "failed", 0)
    let success (n, _, _) = (n, "succeed", 80)

end