with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Handlers;

procedure Chat_Server_2 is

	package ATIO renames Ada.Text_IO;
	package ACL renames Ada.Command_Line;
	package LLU renames Lower_Layer_UDP;
	package ASU renames Ada.Strings.Unbounded;
	use type ASU.Unbounded_String;
	
	Format_Error: exception;


	function Correct_Format return Boolean is
	begin
		return ACL.Argument_Count = 2 and then
			   (Natural'Value(ACL.Argument(2)) >= 2 and
			    Natural'Value(ACL.Argument(2)) <= 50);
	exception
		when Constraint_Error =>
			return False;
	end Correct_Format;


	procedure Get_Server_EP (Server_EP: out LLU.End_Point_Type) is
	
		IP: ASU.Unbounded_String;
		Port: Natural;
		Machine: ASU.Unbounded_String;
	begin
		Machine:= ASU.To_Unbounded_String (LLU.Get_Host_Name);
		IP:= ASU.To_Unbounded_String(LLU.To_IP(ASU.To_String(Machine)));
		Port:= Natural'Value (ACL.Argument(1));
		Server_EP:= LLU.Build (ASU.To_String(IP), Port);
	end Get_Server_EP;
	
	
	Server_EP: LLU.End_Point_Type;
	Char: Character;
begin

	if not Correct_Format then
		raise Format_Error;
	end if;

	Get_Server_EP (Server_EP);
	LLU.Bind (Server_EP, Handlers.Server_Handler'Access);
	
	loop
		Ada.Text_IO.Get_Immediate (Char);
		
		if Char = '1' or Char = 'l' then
			Handlers.Print_Active_Clients;
		elsif Char = '0' or Char = 'o' then
			Handlers.Print_Inactive_Clients;
		end if;
		
	end loop;
	
exception
	when Format_Error =>
		ATIO.Put_Line ("usage: chat_server_2 <Port_Server> <N_Users>");
		LLU.Finalize;
		
	when Except:others =>
		ATIO.Put_Line ("Excepción imprevista: " &
					   Ada.Exceptions.Exception_Name (Except) & " en: " &
					   Ada.Exceptions.Exception_Message (Except));
		LLU.Finalize;
end Chat_Server_2;
