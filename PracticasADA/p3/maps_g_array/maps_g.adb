with Ada.Text_IO;
package body Maps_G is


	function Empty_Cell (Cell_A: Cell) return Boolean is
	begin
		return Cell_A.Full = False;
	end Empty_Cell;
	
	
	function Keys_Match (K1: in Key_Type; K2: in Key_Type) return 
														   Boolean is
	begin
		return K1 = K2;
	end Keys_Match;
	
	
	-- Comprueba si en nuestra lista hay una celda vacía
	function Empty_Cell_In_List(M: Map) return Boolean is
		Found: Boolean:= False;
		Cont: Natural:= 0;
	begin
		while not Found and Cont <= Max_Length-1 loop
		
			if Empty_Cell(M.P_Array(Cont)) then
				Found:= True;
			end if;
			
			Cont:= Cont + 1;
		end loop;
		return Found;
	end Empty_Cell_In_List;
	
	
	-- Comprueba si en nuestra lista existe Key
	function Repeated_Key_In_List(M: Map; Key: Key_Type) return 
														 Boolean is
		Found: Boolean:= False;
		Cont: Natural:= 0;
	begin
		while not Found and Cont <= Max_Length-1 loop
		
			if not Empty_Cell(M.P_Array(Cont)) and then
				   Keys_Match(M.P_Array(Cont).Key, key) then
				Found:= True;
			end if;
			
			Cont:= Cont + 1;
		end loop;
		return Found;
	end Repeated_Key_In_List;
	
	
	procedure Search_Empty_Cell(M: Map; Pos: out Natural) is
		Found: Boolean:= False;
		Cont: Natural:= 0;
		Error_Search_Empty_Cell: exception;
	begin
		while not Found and Cont <= Max_Length-1 loop
			if Empty_Cell(M.P_Array(Cont)) then
				Pos:= Cont;
				Found:= True;
			end if;
			Cont:= Cont + 1;
		end loop;
		
		if not Found then
			raise Error_Search_Empty_Cell;
		end if;
	end Search_Empty_Cell;
	
	
	procedure Search_Repeated_Key(M: Map;
								  Key: Key_Type;
								  Pos: out Natural) is
		Found: Boolean:= False;
		Cont: Natural:= 0;
		Error_Search_Repeated_Key: exception;
	begin
		while not Found and Cont <= Max_Length-1 loop
			if not Empty_Cell(M.P_Array(Cont)) and then
				   Keys_Match(M.P_Array(Cont).Key, key) then
				Pos:= Cont;
				Found:= True;
			end if;
			Cont:= Cont + 1;
		end loop;
		
		if not Found then
			raise Error_Search_Repeated_Key;
		end if;
	end Search_Repeated_Key;
	
	
	procedure Get (M       : Map;
                   Key     : in  Key_Type;
                   Value   : out Value_Type;
                   Success : out Boolean) is
		Pos: Natural;
	begin
		if Repeated_Key_In_List(M, Key) then
			Search_Repeated_Key(M, Key, Pos);
			Value:= M.P_array(Pos).Value;
			Success:= True;
		else
			Success:= False;
		end if;
	end Get;
	
	
	procedure Put (M     : in out Map;
                   Key   : Key_Type;
                   Value : Value_Type) is
		Pos: Natural;
	begin	
		if Repeated_Key_In_List(M, Key) then
			Search_Repeated_Key(M, Key, Pos);
			M.P_array(Pos).Value:= Value;
		elsif Empty_Cell_In_List(M) then
			Search_Empty_Cell(M, Pos);
			M.P_array(Pos).Key:= Key;
			M.P_array(Pos).Value:= Value;
			M.P_array(Pos).Full:= True;
			M.Length:= M.Length + 1;
		else
			raise Full_Map;
		end if;
	end Put;
	
	
	procedure Delete (M      : in out Map;
                      Key     : in  Key_Type;
                      Success : out Boolean) is
		Pos: Natural;
	begin
		if Repeated_Key_In_List(M, Key) then
			Search_Repeated_Key(M, Key, Pos);
			M.P_array(Pos).Full:= False;
			M.Length:= M.Length - 1;
			Success:= True;
		else
			Success:= False;
		end if;
	end Delete;

	
	function Map_Length (M : Map) return Natural is
	begin
		return M.Length;
	end Map_Length;
	
	
	--Te dice si el cursor apunta a tu array o no
	function Out_of_Array (C: Cursor) return Boolean is
	begin
		if C.Element_A <= Max_Length-1 then
			return False;
		else
			return True;
		end if;
	end Out_of_Array;
	
	
	function First (M: Map) return Cursor is
		Cont: Natural:= 0;
		Pos: Natural:= 0;
		Found: Boolean:= False;
	begin
		while not Found and Cont <= Max_Length-1 loop
			if not Empty_Cell(M.P_Array(Cont)) then
				Pos:= Cont;
				Found:= True;
			end if;
			Cont:= Cont + 1;
		end loop;
		
		if not Found then
			Pos:= Max_Length; --El cursor apuntará fuera del array
		end if;
		
		return (M => M, Element_A => Pos);
	end First;


	function Has_Element (C: Cursor) return Boolean is
		Pos: Natural;
	begin
		if not Out_Of_Array(C) then
			Pos:= C.Element_A;
			
			if not Empty_Cell(C.M.P_Array(Pos)) then
				return True;
			else
				return False;
			end if;
			
		else
			return False;
		end if;
	end Has_Element;
	
	
	procedure Next (C: in out Cursor) is
		Found: Boolean:= False;
	begin
		while not Out_Of_Array(C) and not Found loop
			C.Element_A:= C.Element_A +1;
			if Has_Element(C) then
				Found:= True;
			end if;
		end loop;
	end Next;


	function Element (C: Cursor) return Element_Type is
		Pos: Natural;
	begin	
		if Has_Element(C) then
			Pos:= C.Element_A;
			return (Key   => C.M.P_Array(Pos).Key,
                    Value => C.M.P_Array(Pos).Value);
		else
			raise No_Element;
		end if;
	end Element;
end Maps_G;
