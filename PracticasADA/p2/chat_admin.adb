with Chat_Messages;
with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Exceptions;
with Lower_Layer_UDP;
with Ada.Strings.Unbounded;

procedure Chat_Admin is

	package ACL renames Ada.Command_Line;
	package CM renames Chat_Messages;
	package ATIO renames Ada.Text_IO;
	package LLU renames Lower_Layer_UDP;
	package ASU renames Ada.Strings.Unbounded;
	use type CM.Message_Type;
	
	Format_Error: exception;
	Pass_Error: exception;
	
	
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
	
	
	--Receive_CD = Receive_Collection_Data
	procedure Receive_CD (Buffer: Access LLU.Buffer_Type;
						  Data: out ASU.Unbounded_String) is
	
		Mess: CM.Message_Type;
		Error_Message: exception;
	begin
		Mess := CM.Message_Type'Input (Buffer);
		
		if Mess = CM.Collection_Data then
			Data:= ASU.Unbounded_String'Input(Buffer);
		else
			raise Error_Message; --No debería ocurrir
		end if;
	
	exception
		when Error_Message =>
			ATIO.Put_Line ("Error en el mensaje recibido");
	end Receive_CD;
	
	
	procedure Send_Shutdown (Server_EP: in LLU.End_Point_Type;
							 Pass: in ASU.Unbounded_String) is
		
		Buffer: aliased LLU.Buffer_Type(1024);
		Mess: CM.Message_Type:= CM.Shutdown;
	begin
		LLU.Reset(Buffer);
		CM.Message_Type'Output(Buffer'Access, Mess);
		ASU.Unbounded_String'Output(Buffer'Access, Pass);
		LLU.Send(Server_EP, Buffer'Access);
	end Send_Shutdown;
	
	
	procedure Send_Ban (Server_EP: in LLU.End_Point_Type;
					    Pass: in ASU.Unbounded_String;
					    Nick: in ASU.Unbounded_String) is
		
		Buffer: aliased LLU.Buffer_Type(1024);
		Mess: CM.Message_Type:= CM.Ban;
	begin
		LLU.Reset(Buffer);
		CM.Message_Type'Output(Buffer'Access, Mess);
		ASU.Unbounded_String'Output(Buffer'Access, Pass);
		ASU.Unbounded_String'Output(Buffer'Access, Nick);
		LLU.Send(Server_EP, Buffer'Access);
	end Send_Ban;
	
	
	--Send_CR = Send_Collection_Request
	procedure Send_CR (Server_EP: in LLU.End_Point_Type;
					   Admin_EP: in LLU.End_Point_Type;
					   Pass: in ASU.Unbounded_String) is
		
		Buffer: aliased LLU.Buffer_Type(1024);
		Mess: CM.Message_Type:= CM.Collection_Request;
	begin
		LLU.Reset(Buffer);
		CM.Message_Type'Output(Buffer'Access, Mess);
		LLU.End_Point_Type'Output(Buffer'Access, Admin_EP);
		ASU.Unbounded_String'Output(Buffer'Access, Pass);
		LLU.Send(Server_EP, Buffer'Access);
	end Send_CR;
	
	
	Server_EP: LLU.End_Point_Type;
	Admin_EP: LLU.End_Point_Type;
	Pass: ASU.Unbounded_String;
	Buffer: aliased LLU.Buffer_Type(1024);
	Expired : Boolean;
	Entrada: Natural;
	Done: Boolean:= False;
	Data: ASU.Unbounded_String;
	Nick: ASU.Unbounded_String;
begin

	if not Formato_Correcto then 
		raise Format_Error;
	end if;

	Get_Server_EP(Server_EP);
	LLU.Bind_Any(Admin_EP);
	Pass:= ASU.To_Unbounded_String (ACL.Argument(3));
	
	loop
		ATIO.New_Line;
		ATIO.Put_Line("Options");
		ATIO.Put_Line("1 Show writers collection");
		ATIO.Put_Line("2 Ban writer");
		ATIO.Put_Line("3 Shutdown server");
		ATIO.Put_Line("4 Quit");
		ATIO.New_Line;
		ATIO.Put("Your option? ");
			
		begin
			Entrada:= Natural'Value(ATIO.Get_Line);
				
			case Entrada is
				when 1 =>
					Send_CR(Server_EP, Admin_EP, Pass);
					LLU.Reset(Buffer);
					LLU.Receive (Admin_EP, Buffer'Access, 5.0, Expired);
					
					if Expired then
						raise Pass_Error;
					else
						Receive_CD(Buffer'Access, Data);
						ATIO.Put_Line(ASU.To_String(Data));
					end if;
				when 2 =>
					ATIO.Put("Nick to ban? ");
					Nick:= ASU.To_Unbounded_String(ATIO.Get_Line);
					Send_Ban(Server_EP, Pass, Nick);
				when 3 =>
					Send_Shutdown(Server_EP, Pass);
					ATIO.Put_Line("Server shutdown sent");
				when 4 =>
					Done:= True;
				when others =>
					ATIO.Put_Line("Entrada Inválida");
			end case;
				
		exception
			when Constraint_Error =>
				ATIO.Put_Line("Entrada Inválida");
		end;
		exit when Done;
	end loop;
		
	LLU.Finalize;
exception
	when Format_Error =>
		ATIO.Put_Line ("usage: chat_admin <PC_Server>" &
					   " <Port_Server> <Password>");
		LLU.Finalize;
		
	when Pass_Error =>
		ATIO.Put_Line ("Incorrect password.");
		LLU.Finalize;
		
	when Except:others =>
		ATIO.Put_Line ("Excepción imprevista: " &
					   Ada.Exceptions.Exception_Name (Except) & " en: " &
					   Ada.Exceptions.Exception_Message (Except));
		LLU.Finalize;
end Chat_Admin;
