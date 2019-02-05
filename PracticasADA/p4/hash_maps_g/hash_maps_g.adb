with Ada.Unchecked_Deallocation;
with ADA.Text_IO;

package body Hash_Maps_G is

	procedure Free is new Ada.Unchecked_Deallocation (Cell, Cell_A);
	

	procedure Get (M: Map;
				   Key: in  Key_Type;
                   Value: out Value_Type;
                   Success: out Boolean) is
    
		Pos : Natural;
		P_Aux : Cell_A;
	begin
		Pos := Natural(Hash(Key));
	
		P_Aux := M.P_Array(Pos);
		Success := False;
		while not Success and P_Aux /= null Loop
			if P_Aux.Key = Key then
				Value := P_Aux.Value;
				Success := True;
			end if;
			P_Aux := P_Aux.Next;
		end loop;
	end Get;
                   
                  
	procedure Put (M: in out Map;
                   Key: Key_Type;
                   Value: Value_Type) is
                   
		Pos: Natural;
		P_Aux: Cell_A;
		Found : Boolean;
	begin
		Pos:= Natural(Hash(Key));
		
		-- Si ya existe Key, cambiamos su Value
		P_Aux := M.P_Array(Pos);
		Found := False;
		while not Found and P_Aux /= null loop
			if P_Aux.Key = Key then
				P_Aux.Value := Value;
				Found := True;
			end if;
			P_Aux := P_Aux.Next;
		end loop;
		
		-- Si no hemos encontrado Key añadimos al principio
		if not Found then
			if M.Length < Max then
				M.P_Array(Pos):= new Cell'(Key, Value, M.P_Array(Pos));
				M.Length := M.Length + 1;
			else
				raise Full_Map;
			end if;
		end if;
	end Put;


	procedure Delete (M: in out Map;
                      Key: in  Key_Type;
                      Success: out Boolean) is
	
		Pos: Natural;
		P_Current  : Cell_A;
		P_Previous : Cell_A;
	begin
		Pos:= Natural(Hash(Key));
	
		Success := False;
		P_Previous := null;
		P_Current  := M.P_Array(Pos);
		while not Success and P_Current /= null  loop
			if P_Current.Key = Key then
				Success := True;
				M.Length := M.Length - 1;
				if P_Previous /= null then
					P_Previous.Next := P_Current.Next;
				end if;
				if M.P_Array(Pos) = P_Current then
					M.P_Array(Pos) := M.P_Array(Pos).Next;
				end if;
				Free (P_Current);
			else
				P_Previous := P_Current;
				P_Current := P_Current.Next;
			end if;
		end loop;
	end Delete;
	
	
	function Map_Length (M : Map) return Natural is
	begin
		return M.Length;
	end Map_Length;
	
	
	function First (M: Map) return Cursor is
		
		Found: Boolean := False;
		Cont : Natural;
		P_Aux: Cell_A;
	begin
		Cont := M.P_Array'First;
		
		loop
			P_Aux := M.P_Array(Cont);
			if P_Aux /= null then
				Found := True;
			else
				Cont := Cont + 1;
			end if;
			exit when Found or Cont > M.P_Array'Last;
		end loop;
		
		if Found then
			return (M => M, Pos => Cont, Element_A => P_Aux);
		else
			return (M => M, Pos => Cont, Element_A => null);
		end if;
	end First;
	
	
	procedure Next (C: in out Cursor) is
	
		Found : Boolean;
		P_Aux : Cell_A;
		Cont : Natural;
	begin
		if C.Element_A.Next /= null then
			C.Element_A := C.Element_A.Next;
		else
			Found := False;
			Cont := C.Pos + 1;
			while Cont <= C.M.P_Array'Last and not Found loop
				P_Aux := C.M.P_Array(Cont);
				if P_Aux /= null then
					Found := True;
				else
					Cont := Cont + 1;
				end if;
			end loop;
			
			if Found then
				C.Pos := Cont;
				C.Element_A := P_Aux;
			else
				C.Element_A := null;
			end if;
		end if;
	end Next;
	
	
	function Has_Element (C: Cursor) return Boolean is
	begin
		if C.Element_A /= null then
			return True;
		else
			return False;
		end if;
	end Has_Element;
	
	
	function Element (C: Cursor) return Element_Type is
	begin
		if Has_Element (C) then
			return (Key   => C.Element_A.Key,
					Value => C.Element_A.Value);
		else
			raise No_Element;
		end if;
	end Element;

end Hash_Maps_G;
