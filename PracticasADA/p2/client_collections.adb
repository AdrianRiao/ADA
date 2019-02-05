with Ada.Unchecked_Deallocation;

package body Client_Collections is

	use type ASU.Unbounded_String;
	use type LLU.End_Point_Type;
	
	Empty_String: constant String:= "";

	procedure Free is new
		Ada.Unchecked_Deallocation
			(Cell, Cell_A);

	function Last_Cell(Ptr: Cell_A) return Boolean is
	begin
		return Ptr.Next = Null;
	end Last_Cell;


	function Empty_Collection (Collection: Collection_Type) 
							   return Boolean is
	begin
		return Collection.Total = 0;
	end Empty_Collection;
	
	
	--Sabiendo que la funcion Image devuelve un String así:
	--LOWER_LAYER.INET.UDP.UNI.ADDRESS IP: 193.147.49.72, Port:  1025
	function IP (EP: LLU.End_Point_Type) return String is
							   
		S: ASU.Unbounded_String;
		N: Natural:= 0;
	begin
		S:= ASU.To_Unbounded_String(LLU.Image(EP));
		
		for A in 1..2 loop
			N:= ASU.Index (S, " ");
			ASU.Tail (S, ASU.Length(S)-N);
		end loop;
		
		N:= ASU.Index (S, ",");
		ASU.Head (S, N-1);
		
		return ASU.To_String(S);
	end IP;
	
	
	--Sabiendo que la funcion Image devuelve un String así:
	--LOWER_LAYER.INET.UDP.UNI.ADDRESS IP: 193.147.49.72, Port:  1025
	function Port (EP: LLU.End_Point_Type) return String is
	
		S: ASU.Unbounded_String;
		N: Natural:= 0;
	begin
		S:= ASU.To_Unbounded_String(LLU.Image(EP));
	
		for A in 1..2 loop
			N:= ASU.Index (S, ":");
			ASU.Tail (S, ASU.Length(S)-N);
		end loop;
		
		--Elimino los blancos del puerto
		loop
			N:= ASU.Index (S, " ");
			if N /= 0 then
				ASU.Tail (S, ASU.Length(S)-N);
			end if;
			exit when N = 0;
		end loop;
		
		return ASU.To_String(S);
	end Port;
	
	
	function Repeated_Nick(Collection: Collection_Type;
						   Nick: ASU.Unbounded_String) 
						   return Boolean is
	
		P_Aux: Cell_A;
		Found: Boolean:= False;
		Done: Boolean:= False;
	begin
		P_Aux:= Collection.P_First;
		
		loop
			if P_Aux.Nick = Nick then
				Found:= True;
				Done:= True;
			elsif Last_Cell(P_Aux) then
				Done:= True;
			else
				P_Aux:= P_Aux.Next;
			end if;
			
			exit when Done;
		end loop;
		
		return Found;
	end Repeated_Nick;
	
	
	function Total (Collection: in Collection_Type) return Natural is
	begin
		return Collection.Total;
	end Total;
	
	
	procedure Add_Client (Collection: in out Collection_Type;
						  EP: in LLU.End_Point_Type;
						  Nick: in ASU.Unbounded_String;
						  Unique: in Boolean) is
	
		P_Aux: Cell_A;
	begin
	
		if Empty_Collection(Collection) then
			Collection.P_First:= new Cell;
			Collection.P_First.Client_EP:= EP;
			Collection.P_First.Nick:= Nick;
			Collection.Total:= 1;
		else
		
			if not Repeated_Nick(Collection, Nick) then
				P_Aux:= new Cell;
				P_Aux.Client_EP:= EP;
				P_Aux.Nick:= Nick;
				P_Aux.Next:= Collection.P_First;
				Collection.P_First:= P_Aux;
				Collection.Total:= Collection.Total + 1;
			else
			
				if Unique then
					raise Client_Collection_Error;
				else
					P_Aux:= new Cell;
					P_Aux.Client_EP:= EP;
					P_Aux.Nick:= Nick;
					P_Aux.Next:= Collection.P_First;
					Collection.P_First:= P_Aux;
					Collection.Total:= Collection.Total + 1;
				end if;
				
			end if;	
		end if;	
	end Add_Client;
	
	
	procedure Delete_Client (Collection: in out Collection_Type;
                             Nick: in ASU.Unbounded_String) is
	
		P_Aux1: Cell_A; --Ptr que elimina la celda
		P_Aux2: Cell_A; --Va detrás de P_Aux1
		P_Aux3: Cell_A; --Va delante de P_Aux1
		Done: Boolean:= False;
	begin
		if Empty_Collection(Collection) then
			raise Client_Collection_Error;
		elsif not Repeated_Nick(Collection, Nick) then 
			raise Client_Collection_Error;		
		else
			P_Aux1:= Collection.P_First;
			P_Aux2:= Collection.P_First;
			
			loop
			
				if P_Aux1.Nick = Nick then
				
					--caso en el que solo hay una palabra en la lista
					if Last_Cell(P_Aux1) and P_Aux1 = P_Aux2 then 
						P_Aux2:= Null;
						Collection.P_First:= Null;
						Free(P_Aux1);
						
					elsif not Last_Cell(P_Aux1) and P_Aux1 = P_Aux2 then
						P_Aux3:= P_Aux1.Next;
						Collection.P_First:= P_Aux3; --Uno la cadena
						P_Aux2:= Null;
						Free(P_Aux1);
						
					elsif Last_Cell(P_Aux1) then
						P_Aux2.Next:= Null;
						Free(P_Aux1);
						
					else
						P_Aux3:= P_Aux1.Next; 
						P_Aux2.Next:= P_Aux3; --Uno la cadena
						Free(P_Aux1);
					end if;
					Collection.Total:= Collection.Total - 1;
					Done:=True;
					
				else
					P_Aux2:= P_Aux1;
					P_Aux1:= P_Aux1.Next;
				end if;
				
				exit when done;
			end loop;	
		end if;
	end Delete_Client;
	
	
	function Search_Client (Collection: Collection_Type;
							EP: LLU.End_Point_Type)
							return ASU.Unbounded_String is
		P_Aux: Cell_A;
		Done: Boolean:= False;
		Nick: ASU.Unbounded_String;
	begin
	
		if Empty_Collection(Collection) then
			raise Client_Collection_Error;
		else
			P_Aux:= Collection.P_First;
			
			loop
				if P_Aux.Client_EP = EP then
					Nick:= P_Aux.Nick;
					Done:= True;
				elsif Last_Cell(P_Aux) then
					raise Client_Collection_Error;
				else
					P_Aux:= P_Aux.Next;
				end if;
				
				exit when Done;
			end loop;
		end if;
		
		return Nick;
	end Search_Client;


	procedure Send_To_All (Collection: in Collection_Type;
						   P_Buffer: access LLU.Buffer_Type) is
	
		P_Aux: Cell_A;
		Done: Boolean:= False;
	begin
		if not Empty_Collection(Collection) then
			P_Aux:= Collection.P_First;
			loop
				LLU.Send(P_Aux.Client_EP, P_Buffer);
				
				if Last_Cell(P_Aux) then
					Done:= True;
				else
					P_Aux:= P_Aux.Next;
				end if;
				
				exit when Done;
			end loop;
		end if;
	end Send_To_All;
	

	function Collection_Image (Collection: in Collection_Type) 
							   return String is
	
		P_Aux: Cell_A;
		Done: Boolean:= False;
		Data: ASU.Unbounded_String:= 
			  ASU.To_Unbounded_String(Empty_String);
	begin
		if Empty_Collection(Collection) then
			return ASU.To_String(Data);
		else
			P_Aux:= Collection.P_First;
			
			loop
				Data:= ASU.To_Unbounded_String (ASU.To_String(Data) & 
					   ASCII.LF & IP(P_Aux.Client_EP) & ":" & 
					   Port(P_Aux.Client_EP) & " " &
					   ASU.To_String(P_Aux.Nick));

				if Last_Cell(P_Aux) then
					Done:= True;
				else
					P_Aux:= P_Aux.Next;
				end if;
				
				exit when Done;
			end loop;
		end if;
		
		return ASU.To_String(Data);
	end Collection_Image;

end Client_Collections;
