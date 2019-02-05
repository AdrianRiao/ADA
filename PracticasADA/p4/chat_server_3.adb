with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Handlers_Server;

procedure Chat_Server_3 is

	package ATIO renames Ada.Text_IO;
	package ACL renames Ada.Command_Line;
	package LLU renames Lower_Layer_UDP;
	package ASU renames Ada.Strings.Unbounded;
	package HS renames Handlers_Server;
	use type ASU.Unbounded_String;
	
	Format_Error: exception;


	function Correct_Format return Boolean is
	begin
		return ACL.Argument_Count = 5 and then
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
	
	
	Char: Character;
	Min_Delay, Max_Delay, Fault_Pct: Natural;
begin

	if not Correct_Format then
		raise Format_Error;
	end if;

	Min_Delay := Natural'Value (ACL.Argument(3));
	Max_Delay := Natural'Value (ACL.Argument(4));
	Fault_Pct := Natural'Value (ACL.Argument(5));
	HS.Plazo_Retrans := 2 * Duration(Max_Delay) / 1000;
	HS.Max_Retrans := 10 + ((Fault_Pct/10) * (Fault_Pct/10));
	LLU.Set_Faults_Percent (Fault_Pct);
	LLU.Set_Random_Propagation_Delay (Min_Delay, Max_Delay);
	
	Get_Server_EP (HS.Server_EP);
	LLU.Bind (HS.Server_EP, HS.Server_Handler'Access);
	
	loop
		Ada.Text_IO.Get_Immediate (Char);
		
		if Char = '1' or Char = 'l' then
			HS.Print_Active_Clients;
		elsif Char = '0' or Char = 'o' then
			HS.Print_Inactive_Clients;
		end if;
		
	end loop;
	
exception
	when Format_Error =>
		ATIO.Put_Line ("usage: chat_server_3 <Port_Server> <N_Users>" &
					   " <Min_Delay> <Max_Delay> <Fault_Pct>");
		LLU.Finalize;
		
	when Except:others =>
		ATIO.Put_Line ("Excepción imprevista: " &
					   Ada.Exceptions.Exception_Name (Except) & " en: " &
					   Ada.Exceptions.Exception_Message (Except));
		LLU.Finalize;
end Chat_Server_3;
