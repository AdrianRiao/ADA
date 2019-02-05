--Este paquete opera con el tipo Client_Type
with Ada.Strings.Unbounded;
with Lower_Layer_UDP;
with Ada.Text_IO;
with End_Points;

package Clients_Chat is

	package ASU renames Ada.Strings.Unbounded;
	package LLU renames Lower_Layer_UDP;
	package ATIO renames Ada.Text_IO;
	use type ASU.Unbounded_String;
	use type LLU.End_Point_Type;
	
	type Client_Type is private;

	function Nick (Client: Client_Type) return ASU.Unbounded_String;
	
	function EP (Client: Client_Type) return LLU.End_Point_Type;
	
	function "=" (C1: Client_Type; C2: Client_Type) return Boolean;
	
	--Lo que hay en C1 se copia a C2
	procedure Copy (C1: in Client_Type; C2: out Client_Type);
	
	function New_Client_Chat (Nick_A: ASU.Unbounded_String;
							  EP_A: LLU.End_Point_Type) 
							  return Client_Type;
							  
private

	type Client_Type is record
		Nick: ASU.Unbounded_String;
		EP: LLU.End_Point_Type;
	end record;
end Clients_Chat;
