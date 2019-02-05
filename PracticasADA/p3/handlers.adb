package body Handlers is
	
	procedure Client_Handler (From: in LLU.End_Point_Type;
							  To: in LLU.End_Point_Type;
							  P_Buffer: access LLU.Buffer_Type) is

		Nick, Text: ASU.Unbounded_String;
		Mess: CM.Message_Type;
	begin
		Mess := CM.Message_Type'Input (P_Buffer);
		if Mess = CM.Server then
			CM.Extract_Server(Nick, Text, P_Buffer);
			ATIO.New_Line;
			ATIO.Put_Line(ASU.To_String(Nick) & ": " &
						  ASU.To_String(Text));
			ATIO.Put (">> ");
		else
			raise Extract_Server_Error; --No debería ocurrir
		end if;
	end Client_Handler;
	

	package Users_List is new Maps_G(CC.Client_Type,
									 AC.Time,
									 Natural'Value(ACL.Argument(2)),
									 CC."=");
	
	package Old_Users_List is new Maps_G(ASU.Unbounded_String,
										 AC.Time,
										 150,
										 ASU."=");
	
	package UL renames Users_List;
	package OUL renames Old_Users_List;
										  
	Client_List: UL.Map;
	Old_Client_List: OUL.Map;
	
	
	function Time_Image (T: AC.Time) return String is
	begin
		return Gnat.Calendar.Time_IO.Image(T, "%d-%b-%y %T.%i");
	end Time_Image;
   
   
	procedure Print_Active_Clients is
	
		Point: UL.Cursor:= UL.First(Client_List);
		Time: AC.Time;
		Element: UL.Element_Type;
		Client: CC.Client_Type;
	begin
		ATIO.Put_Line("ACTIVE CLIENTS");
		ATIO.Put_Line("==============");
		while UL.Has_Element(Point) loop
			Element:= UL.Element(Point);
			Client:= Element.key;
			Time:= Element.Value;
			
			ATIO.Put_Line(ASU.To_String(CC.Nick(Client)) & 
						  " (" &
						  ASU.To_String(End_Points.IP(CC.EP(Client))) & 
						  ":" & 
						  ASU.To_String(End_Points.Port(CC.EP(Client)))
						  & "): " & Time_Image(Time));
			UL.Next(Point);
		end loop;
	end Print_Active_Clients;
	
	
	procedure Print_Inactive_Clients is
	
		Point: OUL.Cursor:= OUL.First(Old_Client_List);
		Element: OUL.Element_Type;
		Nick: ASU.Unbounded_String;
		Time: AC.Time;
	begin
		ATIO.Put_Line("OLD CLIENTS");
		ATIO.Put_Line("==============");
		while OUL.Has_Element(Point) loop
			Element:= OUL.Element(Point);
			Nick:= Element.key;
			Time:= Element.Value;
			
			ATIO.Put_Line(ASU.To_String(Nick) & ": " &
						  Time_Image(Time));

			OUL.Next(Point);
		end loop;
	end Print_Inactive_Clients;
	
	
	function Client_Repeated (Client_List: in UL.map;
						      Client: in CC.Client_Type) return boolean is
	
		Element: UL.Element_Type;
		Point: UL.Cursor:= UL.First(Client_List);
		Found: Boolean:= False;
	begin
		while UL.Has_Element(Point) and not Found loop
			Element:= UL.Element(Point);
			if Client = Element.key then
				Found:= True;
			else
				UL.Next(Point);
			end if;
		end loop;
		
		return Found;
	end Client_Repeated;
	
	
	function Nick_Repeated (Client_List: in UL.map;
						    Client: in CC.Client_Type) return boolean is
	
		Element: UL.Element_Type;
		Point: UL.Cursor:= UL.First(Client_List);
		Found: Boolean:= False;
	begin
		while UL.Has_Element(Point) and not Found loop
			Element:= UL.Element(Point);
			if CC.Nick(Client) = CC.Nick(Element.key) then
				Found:= True;
			else
				UL.Next(Point);
			end if;
		end loop;
		
		return Found;
	end Nick_Repeated;
	
	
	--Devuelve el elemento de la lista que lleva más tiempo inactivo
	function Low_Element (Client_List: in UL.Map) return 
												  UL.Element_Type is
	
		Point: UL.Cursor:= UL.First(Client_List);
		Time1, Time2: AC.Time;
		Element1, Element2: UL.Element_Type;
		Client1, Client2: CC.Client_Type;
	begin
		--Escojo el primer tiempo
		Element1:= UL.Element(Point);
		Client1:= Element1.key;
		Time1:= Element1.Value;
		
		--Lo comparo con los demás para elegir el menor
		UL.Next(Point);
		while UL.Has_Element(Point) loop
			Element2:= UL.Element(Point);
			Client2:= Element2.key;
			Time2:= Element2.Value;
			if Time2 < Time1 then
				CC.Copy(Client2, Client1);
				Time1:= Time2;
			else
				UL.Next(Point);
			end if;
		end loop;
		
		return (Key => Client1, Value => Time1);
	end Low_Element;
	
	
	procedure Send_Server_Logout (Client_List: in UL.Map;
								  Client: in CC.Client_Type) is

		Who: ASU.Unbounded_String; --Quién envía el mensaje server
		Nick, Text: ASU.Unbounded_String;
		Point: UL.Cursor:= UL.First(Client_List);
		Element: UL.Element_Type;
	begin
		Who:= ASU.To_Unbounded_String("server");
		Nick:= CC.Nick(Client);
		Text:= ASU.To_Unbounded_String
			   (ASU.To_String(Nick) & "leaves the chat");

		while UL.Has_Element(Point) loop
			Element:= UL.Element(Point);
			CM.Send_Server(Who, Text, CC.EP(Element.Key));
			UL.Next(Point);
		end loop;
	end Send_Server_Logout;
	
	
	procedure Send_Server_Init (Client_List: in UL.Map;
								Client: in CC.Client_Type) is

		Who: ASU.Unbounded_String; --Quién envía el mensaje server
		Nick, Text: ASU.Unbounded_String;
		Point: UL.Cursor:= UL.First(Client_List);
		Element: UL.Element_Type;
	begin
		Who:= ASU.To_Unbounded_String("server");
		Nick:= CC.Nick(Client);
		Text:= ASU.To_Unbounded_String
			   (ASU.To_String(Nick) & " joins the chat");

		while UL.Has_Element(Point) loop
			Element:= UL.Element(Point);
			if Element.Key /= Client then
				CM.Send_Server(Who, Text, CC.EP(Element.Key));
			end if;
			UL.Next(Point);
		end loop;
	end Send_Server_Init;
	
	
	procedure Send_Server_Writer (Client_List: in UL.Map;
								  Client: in CC.Client_Type;
								  Text: in ASU.Unbounded_String) is
								  
		Nick: ASU.Unbounded_String;
		Point: UL.Cursor:= UL.First(Client_List);
		Element: UL.Element_Type;
	begin
		Nick:= CC.Nick(Client);
			   
		while UL.Has_Element(Point) loop
			Element:= UL.Element(Point);
			if Element.Key /= Client then
				CM.Send_Server(Nick, Text, CC.EP(Element.Key));
			end if;
			UL.Next(Point);
		end loop;
	end Send_Server_Writer;
	
	
	procedure Send_Server_Eject (Client_List: in UL.Map;
								 Low_Client: in CC.Client_Type) is
	
		Who: ASU.Unbounded_String; --Quién envía el mensaje server
		Nick, Text: ASU.Unbounded_String;
		Point: UL.Cursor:= UL.First(Client_List);
		Element: UL.Element_Type;
	begin
		Who:= ASU.To_Unbounded_String("server");
		Nick:= CC.Nick(Low_Client);
		Text:= ASU.To_Unbounded_String
			   (ASU.To_String(Nick) & " banned for being idle too long");
			   
		while UL.Has_Element(Point) loop
			Element:= UL.Element(Point);
			CM.Send_Server(Who, Text, CC.EP(Element.Key));
			UL.Next(Point);
		end loop;
	end Send_Server_Eject;
	
	
	procedure Eject_Low_Client (Client_List: in out UL.map;
							    Old_Client_List: in out OUL.map) is
	
		Low_Elem: UL.Element_Type;
		Low_Client: CC.Client_Type;
		Success: Boolean;
		Time_Eject: AC.Time;
	begin
		Low_Elem:= Low_Element(Client_List);
		Low_Client:= Low_Elem.Key;
		Send_Server_Eject(Client_List, Low_Client);
		UL.Delete(Client_List, Low_Client, Success);
		Time_Eject:= Ada.Calendar.Clock; --Hora de expulsión
		OUL.Put(Old_Client_List, CC.Nick(Low_Client), Time_Eject);
	
	exception
		when OUL.Full_Map =>
			null;
	end Eject_Low_Client;
	
	
	procedure Save_Client (Client_List: in out UL.map;
						   Old_Client_List: in out OUL.map;
						   Client: in CC.Client_Type;
						   Time: in AC.Time;
						   Saved: out Boolean) is
	begin
		if Nick_Repeated(Client_List, Client) then
			Saved:= False;
		else
			UL.Put(Client_List, Client, Time);
			Saved:= True;
		end if;
	
	exception
		when UL.Full_Map =>
			Eject_Low_Client(Client_List, Old_Client_List);
			UL.Put(Client_List, Client, Time);
			Saved:= True;
	end Save_Client;
	
	
	procedure Receive_Logout (P_Buffer: access LLU.Buffer_Type;
							  Client_List: in out UL.map;
							  Old_Client_List: in out OUL.map) is
	
		Client_EP: LLU.End_Point_Type;
		Client: CC.Client_Type;
		Nick, Text: ASU.Unbounded_String;
		Success: Boolean;
		Time: AC.Time;
	begin
		CM.Extract_Logout(Client_EP, Nick, P_Buffer);
		Client:= CC.New_Client_Chat(Nick, Client_EP);
		UL.Delete(Client_List, Client, Success);

		if Success then
			Send_Server_Logout(Client_List, Client);
			Time:= Ada.Calendar.Clock; --Hora en la que se va
			OUL.Put(Old_Client_List, Nick, Time);
			ATIO.Put_Line("LOGOUT received from " & 
						  ASU.To_String(Nick));
		else
			ATIO.Put_Line("LOGOUT received from "  &
						  "unknown client, IGNORED");
		end if;
	end Receive_Logout;
	
	
	procedure Receive_Writer(P_Buffer: access LLU.Buffer_Type;
							 Client_List: in out UL.map) is
	
		Client_EP: LLU.End_Point_Type;
		Client: CC.Client_Type;
		Nick, Text: ASU.Unbounded_String;
		Time: AC.Time;
	begin
		CM.Extract_Writer(Client_EP, Nick, Text, P_Buffer);
		Client:= CC.New_Client_Chat(Nick, Client_EP);
		
		if Client_Repeated(Client_List, Client) then		
			Send_Server_Writer(Client_List, Client, Text);
			Time:= Ada.Calendar.Clock; --Hora en la que se envía mensaje
			UL.Put(Client_List, Client, Time);
			ATIO.Put_Line("WRITER received from "  &
						  ASU.To_String(Nick) & ": " &
						  ASU.To_String(Text));
		else
			ATIO.Put_Line("WRITER received from "  &
						  "unknown client. IGNORED");
		end if;
	end Receive_Writer;
	
	
	procedure Receive_Init (P_Buffer: access LLU.Buffer_Type;
							Client_List: in out UL.map;
							Old_Client_List: in out OUL.map) is
							
		Time: AC.Time:= AC.Clock;
		Client_EP_Receive, Client_EP_Handler: LLU.End_Point_Type;
		Nick: ASU.Unbounded_String;
		Client: CC.Client_Type;
		Saved: Boolean;
	begin
		CM.Extract_Init(Client_EP_Receive, Client_EP_Handler,
						Nick, P_Buffer);

		Client:= CC.New_Client_Chat(Nick, Client_EP_Handler);
		Save_Client(Client_List, Old_Client_List, Client, Time, Saved);
		
		CM.Send_Welcome(Client_EP_Receive, Saved);
		if Saved then
			Send_Server_Init (Client_List, Client);
			ATIO.Put_Line ("INIT received from " & ASU.To_String(Nick)
						   & ": ACCEPTED");
		else
			ATIO.Put_Line("INIT received from " & 
						  ASU.To_String(Nick) &
						  ": IGNORED. nick already used");
		end if;
	end Receive_Init;
	
	
	procedure Server_Handler (From: in LLU.End_Point_Type;
							  To: in LLU.End_Point_Type;
							  P_Buffer: access LLU.Buffer_Type) is

		Mess: CM.Message_Type;
	begin
		Mess := CM.Message_Type'Input (P_Buffer);
		case Mess is
			when CM.Init =>
				Receive_Init(P_Buffer, Client_List, Old_Client_List);
			when CM.Writer =>
				Receive_Writer(P_Buffer, Client_List);
			when CM.Logout =>
				Receive_Logout(P_Buffer, Client_List, Old_Client_List);
			when Others =>
				raise Error_Message; --No deberír ocurrir
		end case;
	end Server_Handler;

end Handlers;
