--Este paquete opera con el tipo LLU.End_Point_Type
with Lower_Layer_UDP;
with Ada.Strings.Unbounded;

package End_Points is

	package LLU renames Lower_Layer_UDP;
	package ASU renames Ada.Strings.Unbounded;
	use type ASU.Unbounded_String;
				  
	
	function IP (EP: LLU.End_Point_Type) return ASU.Unbounded_String;
	
	
	function Port (EP: LLU.End_Point_Type) return ASU.Unbounded_String;
	
	
	function "=" (EP1: LLU.End_Point_Type; 
				  EP2: LLU.End_Point_Type) return Boolean;
	
end End_Points;
