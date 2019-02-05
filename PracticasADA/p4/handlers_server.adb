package body Handlers_Server is

	HASH_SIZE : constant := 10;
   
	type Hash_Range is mod HASH_SIZE;
	
	function Client_Hash (Client_A: CC.Client_Type) return Hash_Range is
	
		Nick : ASU.Unbounded_String := CC.Nick (Client_A);
		EP : LLU.End_Point_Type := CC.EP (Client_A);
		IP : ASU.Unbounded_String := End_Points.IP (EP);
		Port : ASU.Unbounded_String := End_Points.Port (EP);
		Client, R : ASU.Unbounded_String;
		P : String(1..1);
		C : Character;
		Cont : Natural := 0;
	begin
		Client := ASU.To_Unbounded_String (ASU.To_String(Nick) &
										   ASU.To_String(IP) &
										   ASU.To_String(Port));
		
		while ASU.Length (Client) /= 0 loop
			R := ASU.Head (Client, 1);
			P := ASU.To_String(R);
			C := P(1);
			Cont := Cont + Character'Pos (C);
			ASU.Tail (Client, ASU.Length(Client)-1);
		end loop;
		
		return Hash_Range'Mod(Cont);
	end Client_Hash;
	
	package Users_List is new Hash_Maps_G(CC.Client_Type,
										  AC.Time,
										  CC."=",
										  Hash_Range,
										  Client_Hash,
										  Natural'Value(ACL.Argument(2)));
	
	package Old_Users_List is new Ordered_Maps_G(ASU.Unbounded_String,
												 AC.Time,
												 ASU."=",
												 ASU."<",
												 150);
	
	package Map_Pending_Msgs is new Maps_G(CM.Pend_Msgs_Ident,
										   CM.Pend_Msgs_Data,
										   100,
										   CM."=");
										   
	package Retransmission_Times_List is new Ordered_List_G(ART.Time,
															CM.Pend_Msgs_Ident,
															ART."=",
															ART."<",
															100);
	
	-- Establece los números de secuencia que debo enviar o recibir											
	package Seq_N_Users is new Maps_G(CC.Client_Type,
									   SN.Seq_N_T,
								       Natural'Value(ACL.Argument(2)),
							     	   CC."=");
										   
	package MPM renames Map_Pending_Msgs;
	package RTL renames Retransmission_Times_List;
	package UL renames Users_List;
	package OUL renames Old_Users_List;
	package SNU renames Seq_N_Users;
										  
	Client_List: UL.Map;
	Old_Client_List: OUL.Map;
	Pending_Msgs: MPM.Map;
	Retransmission_Times: RTL.Map;
	
	--guarda el próximo número de secuencia
	-- que me tiene que llegar de cada cliente
	Seq_N_Client: SNU.Map;
	
	--guarda el próximo número de secuencia que
	--tiene que enviar el servidor a cada cliente
	Seq_N_Server: SNU.Map;
	
	
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
	
	
	function No_Pending_Msgs return Boolean is
		Point : MPM.Cursor:= MPM.First(Pending_Msgs);
	begin
		return not MPM.Has_Element(Point);
	end No_Pending_Msgs;


	function Client_In_Old_List (Old_Client_List: in OUL.map;
								 Client: in CC.Client_Type) return boolean is
	
		Element: OUL.Element_Type;
		Point: OUL.Cursor:= OUL.First(Old_Client_List);
		Found: Boolean:= False;
		Nick: ASU.Unbounded_String;
	begin
		Nick:= CC.Nick(Client);
		while OUL.Has_Element(Point) and not Found loop
			Element:= OUL.Element(Point);
			if Nick = Element.key then
				Found:= True;
			else
				OUL.Next(Point);
			end if;
		end loop;
		
		return Found;
	end Client_In_Old_List;
	
	
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
	

	procedure Save_Server_MPM is
	
		Key: CM.Pend_Msgs_Ident;
		Value: CM.Pend_Msgs_Data;
		Mess: CM.Message_Type:= CM.Server;
		Retrans: Natural:= 0;
	begin
		Key:= CM.New_Pend_Msgs_Ident(Server_EP,
									 Client_EP_Handler,
									 My_Seq_N,
									 Retrans);
		
		Value:= CM.New_Pend_Msgs_Data(Mess, Who, Text);		
		MPM.Put(Pending_Msgs, Key, Value);
	end Save_Server_MPM;


	procedure Save_Msg_RTL is
		Key: ART.Time;
		Value: CM.Pend_Msgs_Ident;
		Retrans_Time: ART.Time;
		Retrans: Natural:= 0;
	begin
		Retrans_Time:= ART.Clock + ART.Seconds(Integer(Plazo_Retrans));
		Key:= Retrans_Time;
		
		Value:= CM.New_Pend_Msgs_Ident(Server_Ep,
									   Client_EP_Handler,
									   My_Seq_N,
									   Retrans);
									   
		RTL.Put (Retransmission_Times, Key, Value);
	end Save_Msg_RTL;


	procedure Delete_In_Retransmission_Times is
	
		Point: RTL.Cursor:= RTL.First(Retransmission_Times);
		Done: Boolean:= False;
		Element: RTL.Element_Type;
		Key: ART.Time;
		Value: CM.Pend_Msgs_Ident;
	begin
		while RTL.Has_Element(Point) and not Done loop
			Element:= RTL.Element(Point);
			Key:= Element.Key;
			Value:= Element.Value;
			if Value.Seq_N = Seq_N_Ack and
			   Value.EP_Destination = Client_EP_Handler then
				RTL.Delete(Retransmission_Times, Key, Done);
			end if;
			RTL.Next(Point);
		end loop;
	end Delete_In_Retransmission_Times;
	
	
	procedure Delete_In_Pending_Msgs is
	
		Point: MPM.Cursor:= MPM.First(Pending_Msgs);
		Done: Boolean:= False;
		Element: MPM.Element_Type;
		Key: CM.Pend_Msgs_Ident;
	begin
		while MPM.Has_Element(Point) and not Done loop
			Element:= MPM.Element(Point);
			Key:= Element.Key;
			if Key.Seq_N = Seq_N_Ack and
			   Key.EP_Destination = Client_EP_Handler then
				MPM.Delete(Pending_Msgs, Key, Done);
			end if;
			MPM.Next(Point);
		end loop;
	end Delete_In_Pending_Msgs;


	procedure Delete_Pending_Msg is
	begin
		Delete_In_Pending_Msgs;
		Delete_In_Retransmission_Times;
	end Delete_Pending_Msg;


	procedure Retrans_Msg (Identifier: CM.Pend_Msgs_Ident) is
	
		Data: CM.Pend_Msgs_Data;
		EP_Source: LLU.End_Point_Type;
		EP_Destination: LLU.End_Point_Type;
		Seq_N: SN.Seq_N_T;
		Retrans: Natural;
		Mess: CM.Message_Type;
		Who: ASU.Unbounded_String;
		Text: ASU.Unbounded_String;
		Success: Boolean;
		Key, Retrans_Time: ART.Time;
		Value: CM.Pend_Msgs_Ident;
	begin
		MPM.Get (Pending_Msgs, Identifier, Data, Success);
		EP_Source := Identifier.EP_Source;
		EP_Destination := Identifier.EP_Destination;
		Seq_N := Identifier.Seq_N;
		Retrans := Identifier.Retrans;
		Mess := Data.Mess;
		Who := Data.Nick;
		Text := Data.Text;
		
		if Mess = CM.Server then
			CM.Send_Server (EP_Source, Seq_N, Who,
							Text, EP_Destination);	
		end if;
		
		--Guardo otra vez el mensaje en Retransmission_Times
		Retrans_Time:= ART.Clock + ART.Seconds(Integer(Plazo_Retrans));
		Key:= Retrans_Time;
		
		Value:= CM.New_Pend_Msgs_Ident(EP_Source,
									   EP_Destination,
									   Seq_N,
									   Retrans);
									   
		RTL.Put (Retransmission_Times, Key, Value);
	end Retrans_Msg;


	--Aumenta en 1 el contador de Retrans de un mensaje pendiente
	procedure Change_Retrans_MPM (Identifier: CM.Pend_Msgs_Ident) is

		Point: MPM.Cursor:= MPM.First(Pending_Msgs);
		Done: Boolean:= False;
		Element: MPM.Element_Type;
		Key: CM.Pend_Msgs_Ident;
	begin
		while MPM.Has_Element(Point) and not Done loop
			Element:= MPM.Element(Point);
			Key:= Element.Key;
			if Key = Identifier then
				Key.Retrans := Key.Retrans + 1;
				Done:= True;
			end if;
			MPM.Next(Point);
		end loop;
	end Change_Retrans_MPM;


	procedure Timed_Procedure is

		Point : RTL.Cursor := RTL.First(Retransmission_Times);
		Element : RTL.Element_Type;
		Key : ART.Time;
		Value: CM.Pend_Msgs_Ident;
		Time_Now : ART.Time := ART.Clock;
		Done : Boolean := False;
		Success : Boolean;
	begin
		while RTL.Has_Element(Point) and not Done loop
			Element := RTL.Element(Point);
			Key := Element.Key;
			Value := Element.Value;
			
			if Key <= Time_Now then
				if Value.Retrans < Max_Retrans then
					Change_Retrans_MPM (Value);
					Value.Retrans := Value.Retrans + 1;
					RTL.Delete (Retransmission_Times, Key, Success);
					Retrans_Msg (Value);
				
				--Llega al Máximo de Retransmisiones
				else
					RTL.Delete (Retransmission_Times, Key, Success);
					MPM.Delete (Pending_Msgs, Value, Success);
				end if;
			else
				Done := True;
			end if;
		end loop;
		
		--Caso en el que haya más mensajes pendientes vuelvo a programar
		if RTL.Has_Element(Point) then
			Element := RTL.Element(Point);
			Key := Element.Key;
			PO.Program_Timer_Procedure (Timed_Procedure'Access, 
										Key);
		end if;
	end Timed_Procedure;


	procedure Send_Server_Logout (Client_List: in UL.Map;
								  Client: in CC.Client_Type) is

		Nick: ASU.Unbounded_String;
		Point: UL.Cursor:= UL.First(Client_List);
		Element: UL.Element_Type;
		Success: Boolean;
	begin
		Who:= ASU.To_Unbounded_String("server");
		Nick:= CC.Nick(Client);
		Text:= ASU.To_Unbounded_String
			   (ASU.To_String(Nick) & " leaves the chat");

		while UL.Has_Element(Point) loop
			Element:= UL.Element(Point);		
			if No_Pending_Msgs then
				PO.Program_Timer_Procedure
				(Timed_Procedure'Access,
				 ART.Clock + ART.Seconds(Integer(Plazo_Retrans)));
			end if;
			
			Client_EP_Handler:= CC.EP(Element.Key);
			SNU.Get(Seq_N_Server, Element.Key, 
					My_Seq_N, Success);
			SNU.Put(Seq_N_Server, Element.Key, My_Seq_N+1);
				
			CM.Send_Server(Server_EP, My_Seq_N,
						   Who, Text, Client_EP_Handler);
						   
			PO.Protected_Call(Save_Server_MPM'Access);
				
			PO.Protected_Call(Save_Msg_RTL'Access);
				
			UL.Next(Point);
		end loop;
	end Send_Server_Logout;
	
	
	procedure Send_Server_Init (Client_List: in UL.Map;
								Client: in CC.Client_Type) is

		Nick: ASU.Unbounded_String;
		Point: UL.Cursor:= UL.First(Client_List);
		Element: UL.Element_Type;
		Success: Boolean;
	begin
		Who:= ASU.To_Unbounded_String("server");
		Nick:= CC.Nick(Client);
		Text:= ASU.To_Unbounded_String
			   (ASU.To_String(Nick) & " joins the chat");

		while UL.Has_Element(Point) loop
			Element:= UL.Element(Point);
			if Element.Key /= Client then
				if No_Pending_Msgs then
					PO.Program_Timer_Procedure
					(Timed_Procedure'Access,
					 ART.Clock + ART.Seconds(Integer(Plazo_Retrans)));
				end if;
				
				Client_EP_Handler:= CC.EP(Element.Key);
				SNU.Get(Seq_N_Server, Element.Key, 
						My_Seq_N, Success);
				SNU.Put(Seq_N_Server, Element.Key, My_Seq_N+1);
				
				CM.Send_Server(Server_EP, My_Seq_N, 
							   Who, Text, Client_EP_Handler);
				
				PO.Protected_Call(Save_Server_MPM'Access);
				
				PO.Protected_Call(Save_Msg_RTL'Access);
			end if;
			UL.Next(Point);
		end loop;
	end Send_Server_Init;
	
	
	procedure Send_Server_Writer (Client_List: in UL.Map;
								  Client: in CC.Client_Type;
								  Text_A: in ASU.Unbounded_String) is
								  
		Nick: ASU.Unbounded_String;
		Point: UL.Cursor:= UL.First(Client_List);
		Element: UL.Element_Type;
		Success: Boolean;
	begin
		Nick:= CC.Nick(Client);
		Who:= Nick;
		Text:= Text_A;
			   
		while UL.Has_Element(Point) loop
			Element:= UL.Element(Point);
			if Element.Key /= Client then
				if No_Pending_Msgs then
					PO.Program_Timer_Procedure
					(Timed_Procedure'Access,
					 ART.Clock + ART.Seconds(Integer(Plazo_Retrans)));
				end if;
				
				Client_EP_Handler:= CC.EP(Element.Key);
				SNU.Get(Seq_N_Server, Element.Key, 
						My_Seq_N, Success);
				SNU.Put(Seq_N_Server, Element.Key, My_Seq_N+1);
				
				CM.Send_Server(Server_EP, My_Seq_N, 
							   Who, Text, Client_EP_Handler);
							   
				PO.Protected_Call(Save_Server_MPM'Access);
				
				PO.Protected_Call(Save_Msg_RTL'Access);
			end if;
			UL.Next(Point);
		end loop;
	end Send_Server_Writer;
	
	
	procedure Send_Server_Eject (Client_List: in UL.Map;
								 Low_Client: in CC.Client_Type) is
	
		Nick: ASU.Unbounded_String;
		Point: UL.Cursor:= UL.First(Client_List);
		Element: UL.Element_Type;
		Success: Boolean;
	begin
		Who:= ASU.To_Unbounded_String("server");
		Nick:= CC.Nick(Low_Client);
		Text:= ASU.To_Unbounded_String
			   (ASU.To_String(Nick) & " banned for being idle too long");
			   
		while UL.Has_Element(Point) loop
			Element:= UL.Element(Point);
			if No_Pending_Msgs then
				PO.Program_Timer_Procedure
				(Timed_Procedure'Access,
				 ART.Clock + ART.Seconds(Integer(Plazo_Retrans)));
			end if;
			
			Client_EP_Handler:= CC.EP(Element.Key);
			SNU.Get(Seq_N_Server, Element.Key, 
					My_Seq_N, Success);
			SNU.Put(Seq_N_Server, Element.Key, My_Seq_N+1);
				
			CM.Send_Server(Server_EP, My_Seq_N, 
						   Who, Text, Client_EP_Handler);
			
			PO.Protected_Call(Save_Server_MPM'Access);
				
			PO.Protected_Call(Save_Msg_RTL'Access);
				
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
		SNU.Delete(Seq_N_Client, Low_Client, Success);
		SNU.Delete(Seq_N_Server, Low_Client, Success);
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
						   Saved: out Boolean;
						   First_Time_init: in out Boolean) is
	begin
		if Client_Repeated(Client_List, Client) then
			Saved:= True;
			First_Time_init:= False;
		elsif Nick_Repeated(Client_List, Client) then
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
		Seq_N: SN.Seq_N_T; --N.Secuencia que recibo
		Seq_N_A: SN.Seq_N_T; --N.Secuencia que espero recibir
	begin
		CM.Extract_Logout(Client_EP, Seq_N, Nick, P_Buffer);
		Client:= CC.New_Client_Chat(Nick, Client_EP);

		if Client_Repeated(Client_List, Client) then
			SNU.Get (Seq_N_Client, Client, Seq_N_A, Success);
			
			if Seq_N < Seq_N_A then
				CM.Send_Ack (Server_EP, Seq_N, Client_EP);
			elsif Seq_N = Seq_N_A then
			
				CM.Send_Ack (Server_EP, Seq_N, Client_EP);
				SNU.Delete (Seq_N_Client, Client, Success);
				SNU.Delete(Seq_N_Server, Client, Success);
				UL.Delete(Client_List, Client, Success);
				Send_Server_Logout(Client_List, Client);
				Time:= Ada.Calendar.Clock; --Hora en la que se va
				OUL.Put(Old_Client_List, Nick, Time);
				ATIO.Put_Line("LOGOUT received from " & 
								ASU.To_String(Nick));
			end if;
		
		--Si ya hemos eliminado al cliente y no le llega el ACK
		elsif Client_In_Old_List(Old_Client_List, Client) then
			CM.Send_Ack (Server_EP, Seq_N, Client_EP);
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
		Seq_N: SN.Seq_N_T; --N.Secuencia que recibo
		Seq_N_A: SN.Seq_N_T; --N.Secuencia que espero recibir
		Success: Boolean;
	begin
		CM.Extract_Writer(Client_EP, Seq_N, Nick, Text, P_Buffer);
		Client:= CC.New_Client_Chat(Nick, Client_EP);
		
		if Client_Repeated(Client_List, Client) then
			SNU.Get (Seq_N_Client, Client, Seq_N_A, Success);
			
			if Seq_N < Seq_N_A then
				CM.Send_Ack (Server_EP, Seq_N, Client_EP);
			elsif Seq_N = Seq_N_A then
	
				SNU.Put (Seq_N_Client, Client, Seq_N_A+1);
				CM.Send_Ack (Server_EP, Seq_N, Client_EP);		
				Send_Server_Writer(Client_List, Client, Text);
				Time:= Ada.Calendar.Clock; --Hora en la que se envía mensaje
				UL.Put(Client_List, Client, Time);
				ATIO.Put_Line("WRITER received from "  &
								ASU.To_String(Nick) & ": " &
								ASU.To_String(Text));
			end if;
			
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
		
		--Indica si el cliente es la primera vez que envía el Init
		First_Time_Init: Boolean:= True;
		
		--Es el número de secuencia que espero
		--recibir de este cliente
		Seq_N: SN.Seq_N_T:= SN.Seq_N_T'First + 1;
		
		--Es el próximo número de secuencia
		--que debo enviar a este cliente
		Seq_N_A: SN.Seq_N_T:= SN.Seq_N_T'First + 1;
	begin
		CM.Extract_Init(Client_EP_Receive, Client_EP_Handler,
						Nick, P_Buffer);

		Client:= CC.New_Client_Chat(Nick, Client_EP_Handler);
		Save_Client(Client_List, Old_Client_List, 
					Client, Time, Saved, First_Time_Init);
		
		CM.Send_Welcome(Client_EP_Receive, Saved);
		
		if First_Time_Init then
			
			if Saved then
				SNU.Put(Seq_N_Client, Client, Seq_N);
				SNU.Put(Seq_N_Server, Client, Seq_N_A);
				Send_Server_Init (Client_List, Client);
				ATIO.Put_Line ("INIT received from " & ASU.To_String(Nick)
								& ": ACCEPTED");
			else
				ATIO.Put_Line("INIT received from " & 
								ASU.To_String(Nick) &
								": IGNORED. nick already used");
			end if;
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
			when CM.Ack =>
				CM.Extract_Ack (Client_EP_Handler, Seq_N_Ack, P_Buffer);
				PO.Protected_Call(Delete_Pending_Msg'Access);
			when Others =>
				raise Error_Message; --No deberír ocurrir
		end case;
	end Server_Handler;
	
end Handlers_Server;
