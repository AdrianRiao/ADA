with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Exceptions;
with Lower_Layer_UDP;
with Chat_Messages;
with Ada.Strings.Unbounded;
with Client_Collections;

procedure Chat_Server is

	package ACL renames Ada.Command_Line;
	package ATIO renames Ada.Text_IO;
	package LLU renames Lower_Layer_UDP;
	package CM renames Chat_Messages;
	package ASU renames Ada.Strings.Unbounded;
	package CC renames Client_Collections;
	use type CM.Message_Type;
	use type ASU.Unbounded_String;
	
	Format_Error: exception;
	Error_Message: exception;


	function Correct_Pass (Pass: ASU.Unbounded_String) return Boolean is
	begin
		return Pass = ACL.Argument(2);
	end Correct_Pass;
	
	
	function Correct_Format return Boolean is
	begin
		return ACL.Argument_Count = 2;
	end Correct_Format;
	
	
	function Is_Reader (Nick: ASU.Unbounded_String) return Boolean is
	begin
		return Nick = "reader";
	end Is_Reader;
	
	
	--Extrae datos de un mensaje Logout
	procedure Get_Logout (Buffer: Access LLU.Buffer_Type;
						  Client_EP: out LLU.End_Point_Type) is
	begin
		Client_EP:= LLU.End_Point_Type'Input (Buffer);
	end Get_Logout;
	
	
	--Extrae datos de un mensaje Writer
	procedure Get_Writer (Buffer: Access LLU.Buffer_Type;
						  Client_EP: out LLU.End_Point_Type;
						  Text: out ASU.Unbounded_String) is
	begin
		Client_EP:= LLU.End_Point_Type'Input (Buffer);
		Text:= ASU.Unbounded_String'Input (Buffer);
	end Get_Writer;
	
	
	--Extrae datos de un mensaje INIT
	procedure Get_Init (Buffer: Access LLU.Buffer_Type;
						Client_EP: out LLU.End_Point_Type;
						Nick: out ASU.Unbounded_String) is
	begin
		Client_EP:= LLU.End_Point_Type'Input (Buffer);
		Nick:= ASU.Unbounded_String'Input (Buffer);
	end Get_Init;
	
	
	--Send_CD = Send_Collection_Data
	procedure Send_CD (Admin_EP: in LLU.End_Point_Type;
					   List_Writers: in CC.Collection_Type) is
	
		Mess: CM.Message_Type:= CM.Collection_Data;
		Data: ASU.Unbounded_String;
		Buffer: aliased LLU.Buffer_Type(1024);
	begin
		Data:= ASU.To_Unbounded_String
			   (CC.Collection_Image(List_Writers));
		LLU.Reset(Buffer);
		CM.Message_Type'Output(Buffer'Access, Mess);
		ASU.Unbounded_String'Output(Buffer'Access, Data);
		LLU.Send(Admin_EP, Buffer'Access);
	end Send_CD;
	
	
	--Es el mensaje que el servidor envía a los clientes, ya sea
	--porque alguien se añade al chat, o si alguien envía un mensaje,
	--o si alguien deja el chat. Who indica quién envía el mensaje
	--y Reply indica el mensaje.
	procedure Send_Reply (Who: in ASU.Unbounded_String;
						  Reply: in ASU.Unbounded_String;
						  List_Readers: in CC.Collection_Type) is
						  
		Mess: CM.Message_Type:= CM.Server;
		Buffer: aliased LLU.Buffer_Type(1024);
	begin
		LLU.Reset(Buffer);
		CM.Message_Type'Output(Buffer'Access, Mess);
		ASU.Unbounded_String'Output(Buffer'Access, Who);
		ASU.Unbounded_String'Output(Buffer'Access, Reply);
		CC.Send_To_All(List_Readers, Buffer'Access);
	end Send_Reply;
	
	
	procedure Save_Client (Client_EP: in LLU.End_Point_Type;
						   Nick: in ASU.Unbounded_String;
						   List_Readers: out CC.Collection_Type;
						   List_Writers: out CC.Collection_Type) is
		Unique: Boolean;
	begin		
		if Is_Reader(Nick) then
			Unique:= False;
			CC.Add_Client(List_Readers, Client_EP, Nick, Unique);
		else
			Unique:= True;
			CC.Add_Client(List_Writers, Client_EP, Nick, Unique);
		end if;
	end Save_Client;
	
	
	procedure Receive_Shutdown (Buffer: Access LLU.Buffer_Type;
								Done: out Boolean) is
	
		Pass: ASU.Unbounded_String;
	begin
		Pass:= ASU.Unbounded_String'Input(Buffer);
		
		if Correct_Pass(Pass) then
			ATIO.Put_Line("SHUTDOWN received");
			Done:= True;
		else
			ATIO.Put_Line("SHUTDOWN received" &
						  ". IGNORED, incorrect password");
		end if;
	end Receive_Shutdown;
	
	
	procedure Receive_Ban (Buffer: Access LLU.Buffer_Type;
						   List_Writers: in out CC.Collection_Type) is
	
		Pass: ASU.Unbounded_String;
		Nick: ASU.Unbounded_String;
	begin
		Pass:= ASU.Unbounded_String'Input(Buffer);
		Nick:= ASU.Unbounded_String'Input(Buffer);
		
		if Correct_Pass(Pass) then
			CC.Delete_Client(List_Writers, Nick);
			ATIO.Put_Line("BAN received for " & ASU.To_String(Nick));
		else
			ATIO.Put_Line("BAN received for " & ASU.To_String(Nick) &
						  ". IGNORED, incorrect password");
		end if;
		
	exception
		when CC.Client_Collection_Error =>
			ATIO.Put_Line("BAN received for " & ASU.To_String(Nick) &
						  ". IGNORED, nick not found");
	end Receive_Ban;
	
	
	--Receive_CR = Receive_Collection_Request
	procedure Receive_CR (Buffer: Access LLU.Buffer_Type;
						  List_Writers: in CC.Collection_Type) is
		
		Admin_EP: LLU.End_Point_Type;
		Pass: ASU.Unbounded_String;
	begin
		Admin_EP:= LLU.End_Point_Type'Input(Buffer);
		Pass:= ASU.Unbounded_String'Input(Buffer);
		
		if Correct_Pass(Pass) then
			ATIO.Put_Line("LIST_REQUEST received");
			Send_CD(Admin_EP, List_Writers);
		else
			ATIO.Put_Line("LIST_REQUEST received. " &
						  "IGNORED, incorrect password");
		end if;
	end Receive_CR;
	
	
	procedure Receive_Logout (Buffer: Access LLU.Buffer_Type;
							  List_Readers: in out CC.Collection_Type;
							  List_Writers: in out CC.Collection_Type) is
	
		Client_EP: LLU.End_Point_Type;
		Nick: ASU.Unbounded_String;
		Reply: ASU.Unbounded_String;
		Who: ASU.Unbounded_String:= ASU.To_Unbounded_String("server");
	begin
		Get_Logout (Buffer, Client_EP);
		Nick:= CC.Search_Client(List_Writers, Client_EP);
													
		ATIO.Put_Line("LOGOUT received from " & ASU.To_String(Nick));
		CC.Delete_Client(List_Writers, Nick);
						
		Reply:= ASU.To_Unbounded_String (ASU.To_String(Nick) &
										 " leaves the chat");
		Send_Reply(Who, Reply, List_Readers);
	exception
		when CC.Client_Collection_Error =>
			ATIO.Put_Line("LOGOUT received from "  &
						  "unknown client, IGNORED");
	end Receive_Logout;
	
	
	procedure Receive_Writer (Buffer: Access LLU.Buffer_Type;
							  List_Readers: in CC.Collection_Type;
							  List_Writers: in CC.Collection_Type) is
	
		Client_EP: LLU.End_Point_Type;
		Nick: ASU.Unbounded_String;
		Text: ASU.Unbounded_String;
		Reply: ASU.Unbounded_String;
		Who: ASU.Unbounded_String;
	begin
		Get_Writer (Buffer, Client_EP, Text);
		Nick:= CC.Search_Client(List_Writers, Client_EP);
		
		Reply:= Text;
		Who:= Nick;
		Send_Reply(Who, Reply, List_Readers);
											
		ATIO.Put_Line("WRITER received from " &
					  ASU.To_String(Nick) &
					  ": " & ASU.To_String(Text));
	exception
		when CC.Client_Collection_Error =>
			ATIO.Put_Line("WRITER received from "  &
						  "unknown client. IGNORED");
	end Receive_Writer;
	
	
	procedure Receive_Init (Buffer: Access LLU.Buffer_Type;
							List_Readers: out CC.Collection_Type;
							List_Writers: out CC.Collection_Type) is
	
		Client_EP: LLU.End_Point_Type;
		Nick: ASU.Unbounded_String;
		
		--Mensaje que el servidor envía a todos los clientes
		Reply: ASU.Unbounded_String;
		
		--Indica quién envía el mensaje Reply, en este caso server							
		Who: ASU.Unbounded_String:= ASU.To_Unbounded_String("server");
	begin
		Get_Init (Buffer, Client_EP, Nick);
		Save_Client(Client_EP, Nick, List_Readers, List_Writers);
		
		if not Is_Reader(Nick) then
			Reply:= ASU.To_Unbounded_String(ASU.To_String(Nick) &
										    " joins the chat");
			Send_Reply(Who, Reply, List_Readers);
		end if;
		
		ATIO.Put_Line("INIT received from " & ASU.To_String(Nick));
		
	exception
		when CC.Client_Collection_Error =>
			ATIO.Put_Line("INIT received from " & 
						  ASU.To_String(Nick) &
						  ". IGNORED, nick already used");
	end Receive_Init;
	
	
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
	Buffer: aliased LLU.Buffer_Type(1024);
	Mess: CM.Message_Type;
	Expired : Boolean;
	List_Readers: CC.Collection_Type;
	List_Writers: CC.Collection_Type;
	Done: Boolean:= False;
begin

	if not Correct_Format then 
		raise Format_Error;
	end if;

	Get_Server_EP (Server_EP);
	LLU.Bind (Server_EP);
	
	loop
		LLU.Reset(Buffer);
		LLU.Receive (Server_EP, Buffer'Access, 1000.0, Expired);
		
		if not Expired then
			Mess := CM.Message_Type'Input (Buffer'Access);
			
			case Mess is
				when CM.Init =>
					Receive_Init(Buffer'Access,
								 List_Readers,
								 List_Writers);
				when CM.Writer =>
					Receive_Writer(Buffer'Access,
								   List_Readers,
								   List_Writers);
				when CM.Logout =>
					Receive_Logout(Buffer'Access,
								   List_Readers,
								   List_Writers);
				when CM.Collection_Request =>
					Receive_CR(Buffer'Access,
							   List_Writers);
				when CM.Ban =>
					Receive_Ban(Buffer'Access,
								List_Writers);
				when CM.Shutdown =>
					Receive_Shutdown(Buffer'Access, Done);
				when others =>
					raise Error_Message; --No debería ocurrir
			end case;
			
		end if;
		exit when Done;
	end loop;

	LLU.Finalize;
exception
	when Format_Error =>
		ATIO.Put_Line ("usage: chat_server <Port> <Password>");
		LLU.Finalize;
		
	when Error_Message =>
		ATIO.Put_Line ("Error en el mensaje recibido");
		LLU.Finalize;
		
	when Except:others =>
		ATIO.Put_Line ("Excepción imprevista: " &
					   Ada.Exceptions.Exception_Name (Except) & " en: " &
					   Ada.Exceptions.Exception_Message (Except));
		LLU.Finalize;
end Chat_Server;
