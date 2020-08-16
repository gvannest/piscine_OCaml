class virtual reaction (reactive:Molecule.molecule list) (products:Molecule.molecule list) =
object (this)
    method virtual get_start : (Molecule.molecule * int) list
    method virtual get_result : (Molecule.molecule * int) list
    method virtual balance : reaction
    method virtual is_balanced : bool
end