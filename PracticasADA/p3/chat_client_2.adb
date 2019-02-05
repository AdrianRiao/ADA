with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Lower_Layer_UDP;
with Chat_Messages;
with Ada.Strings.Unbounded;
with Handlers;

procedure Chat_Client_2 is

	package ATIO renames Ada.Text_IO;
	package ACL renames Ada.Command_Line;
	package LLU renames Lower_Layer_UDP;
	package CM renames Chat_Messages;
	package ASU renames Ada.Strings.Unbounded;
	use type ASU.Unbounded_String;
	use type CM.Message_Type;
	
	Format_Error: exception;
	Accepted_Error: exception;
	Extract_Welcome_Error: exception;


	function Correct_Format return Boolean is
	begin
		return ACL.Argument_Count = 3 and then
			   ACL.Argument(3) /= "server";
	end Correct_Format;


	procedure Get_Server_EP (Server_EP: out LLU.End_Point_Type) is
	
		IP: ASU.Unbounded_String;
		Port: Natural;
	begin
		IP:= ASU.To_Unbounded_String (LLU.To_IP(ACL.Argument(1)));
		Port:= Natural'Value (ACL.Argument(2));
		Server_EP:= LLU.Build (ASU.To_String(IP), Port);
	end Get_Server_EP;
	
	
	procedure Receive_Welcome(Client_EP: in LLU.End_Point_Type;
							  Accepted: out Boolean) is
	
		Buffer: aliased LLU.Buffer_Type(1024);
		Expired: Boolean;
		Mess: CM.Message_Type;
	begin
		LLU.Reset(Buffer);
		LLU.Receive (Client_EP, Buffer'Access, 10.0, Expired);
		if Expired then
			raise Accepted_Error;
		else
			Mess := CM.Message_Type'Input (Buffer'Access);
			if Mess = CM.Welcome then
				CM.Extract_Welcome(Accepted, Buffer'Access);
			else
				raise Extract_Welcome_Error; --No debería ocurrir
			end if;
		end if;
	end Receive_Welcome;
	
	
	Client_EP_Receive, Client_EP_Handler, Server_EP: LLU.End_Point_Type;
	Nick, Text: ASU.Unbounded_String;
	Accepted: Boolean;
	Reply: String:= "hola";
begin
	
	if not Correct_Format then 
		raise Format_Error;
	end if;
	
	Get_Server_EP(Server_EP);
	LLU.Bind_Any(Client_EP_Receive);
	LLU.Bind_Any(Client_EP_Handler, Handlers.Client_Handler'Access);
	Nick:= ASU.To_Unbounded_String (ACL.Argument(3));
	CM.Send_Init(Client_EP_Receive, Client_EP_Handler,
				 Nick, Server_EP);
	Receive_Welcome(Client_EP_Receive, Accepted);
	
	if Accepted then
		ATIO.Put_Line("Mini-Chat v2.0: Welcome " &
					  ASU.To_String(Nick));
		
		loop
			ATIO.Put (">> ");
			Text:= ASU.To_Unbounded_String (ATIO.Get_Line);
			
			if Text = ".quit" then
				CM.Send_Logout(Client_EP_Handler, Nick, Server_EP);
			else
				CM.Send_Writer(Client_EP_Handler, Nick,
							   Text, Server_EP);
			end if;
			
			exit when Text = ".quit";
		end loop;

	else
		ATIO.Put_Line("Mini-Chat v2.0: IGNORED new user " &
					  ASU.To_String(Nick) &
					  ", nick already used");
	end if;
	
	LLU.Finalize;
exception
	when Format_Error =>
		ATIO.Put_Line ("usage: chat_client_2 <PC_Server>" &
					   " <Port_Server> <Nickname>");
		LLU.Finalize;
		
	when Accepted_Error =>
		ATIO.Put_Line ("Server unreachable");
		LLU.Finalize;
		
	when Except:others =>
		ATIO.Put_Line ("Excepción imprevista: " &
					   Ada.Exceptions.Exception_Name (Except) & " en: " &
					   Ada.Exceptions.Exception_Message (Except));
		LLU.Finalize;
end Chat_Client_2;
