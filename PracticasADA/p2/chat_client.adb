with Chat_Messages;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;
with Lower_Layer_UDP;
with Ada.Strings.Unbounded;

procedure Chat_Client is

	package ACL renames Ada.Command_Line;
	package CM renames Chat_Messages;
	package ATIO renames Ada.Text_IO;
	package LLU renames Lower_Layer_UDP;
	package ASU renames Ada.Strings.Unbounded;
	use type ASU.Unbounded_String;
	use type CM.Message_Type;
	
	Format_Error: exception;
	
	function Reader_Mode return Boolean is
	begin
		return ACL.Argument(3) = "reader";
	end Reader_Mode;
	
	
	function Formato_Correcto return Boolean is
	begin
		return ACL.Argument_Count = 3;
	end Formato_Correcto;
	
	
	procedure Get_Server_EP (Server_EP: out LLU.End_Point_Type) is
	
		IP: ASU.Unbounded_String;
		Port: Natural;
	begin
		IP:= ASU.To_Unbounded_String (LLU.To_IP(ACL.Argument(1)));
		Port:= Natural'Value (ACL.Argument(2));
		Server_EP:= LLU.Build (ASU.To_String(IP), Port);
	end Get_Server_EP;
	
	
	procedure Get_Reply (Buffer: in out LLU.Buffer_Type) is
	
		Mess: CM.Message_Type;
		Nick: ASU.Unbounded_String;
		Reply: ASU.Unbounded_String;
		Error_Message: exception;
	begin
		Mess := CM.Message_Type'Input (Buffer'Access);
		Nick := ASU.Unbounded_String'Input (Buffer'Access);
		Reply := ASU.Unbounded_String'Input (Buffer'Access);
		
		if Mess = CM.Server then
			ATIO.Put (ASU.To_String(Nick));
			ATIO.Put (": ");
			ATIO.Put_Line (ASU.To_String(Reply));
		else
			raise Error_Message; --No debería ocurrir
		end if;
		
	exception
		when Error_Message =>
			ATIO.Put_Line ("Error en el mensaje recibido");
	end Get_Reply;
	
	
	procedure Send_Writer (Server_EP: in LLU.End_Point_Type;
						   Client_EP: in LLU.End_Point_Type;
						   Message: in ASU.Unbounded_String) is
		
		Buffer: aliased LLU.Buffer_Type(1024);
		Mess: CM.Message_Type:= CM.Writer;
	begin
		LLU.Reset(Buffer);
		CM.Message_Type'Output(Buffer'Access, Mess);
		LLU.End_Point_Type'Output(Buffer'Access, Client_EP);
		ASU.Unbounded_String'Output(Buffer'Access, Message);
		LLU.Send(Server_EP, Buffer'Access);
	end Send_Writer;
	
	
	procedure Send_Logout (Server_EP: in LLU.End_Point_Type;
						   Client_EP: in LLU.End_Point_Type) is
		
		Buffer: aliased LLU.Buffer_Type(1024);
		Mess: CM.Message_Type:= CM.Logout;
	begin
		LLU.Reset(Buffer);
		CM.Message_Type'Output(Buffer'Access, Mess);
		LLU.End_Point_Type'Output(Buffer'Access, Client_EP);
		LLU.Send(Server_EP, Buffer'Access);
	end Send_Logout;
	
	
	procedure Send_Init (Server_EP: in LLU.End_Point_Type;
						 Client_EP: in LLU.End_Point_Type;
						 Nick: in ASU.Unbounded_String) is
		
		Buffer: aliased LLU.Buffer_Type(1024);
		Mess: CM.Message_Type:= CM.Init;
	begin
		LLU.Reset(Buffer);
		CM.Message_Type'Output(Buffer'Access, Mess);
		LLU.End_Point_Type'Output(Buffer'Access, Client_EP);
		ASU.Unbounded_String'Output(Buffer'Access, Nick);
		LLU.Send(Server_EP, Buffer'Access);
	end Send_Init;


	Server_EP: LLU.End_Point_Type;
	Client_EP: LLU.End_Point_Type;
	Buffer: aliased LLU.Buffer_Type(1024);
	Expired : Boolean;
	Client_Nick: ASU.Unbounded_String;
	Message: ASU.Unbounded_String;
begin

	if not Formato_Correcto then 
		raise Format_Error;
	end if;
	
	Get_Server_EP(Server_EP);
	LLU.Bind_Any(Client_EP);
	Client_Nick:= ASU.To_Unbounded_String (ACL.Argument(3));
	Send_Init(Server_EP, Client_EP, Client_Nick);
	
	if Reader_Mode then

		loop
			LLU.Reset(Buffer);
			LLU.Receive (Client_EP, Buffer'Access, 1000.0, Expired);
			if not Expired then
				Get_Reply(Buffer);
			end if;
		end loop;
	else
	
		loop
			ATIO.Put ("Message: ");
			Message:= ASU.To_Unbounded_String (ATIO.Get_Line);
			
			if Message = ".quit" then
				Send_Logout(Server_EP, Client_EP);
			else
				Send_Writer(Server_EP, Client_EP, Message);
			end if;
			
			exit when Message = ".quit";
		end loop;
	end if;
	
	LLU.Finalize;
exception
	when Format_Error =>
		ATIO.Put_Line ("usage: chat_client <PC_Server>" &
					   " <Port_Server> <Nickname>");
		LLU.Finalize;
		
	when Except:others =>
		ATIO.Put_Line ("Excepción imprevista: " &
					   Ada.Exceptions.Exception_Name (Except) & " en: " &
					   Ada.Exceptions.Exception_Message (Except));
		LLU.Finalize;
end Chat_Client;
