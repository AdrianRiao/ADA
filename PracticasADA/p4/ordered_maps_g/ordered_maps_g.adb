package body Ordered_Maps_G is
	
	
	function Empty_Map (M: Map) return Boolean is
	begin
		return M.Length = 0;
	end Empty_Map;
	

	function Empty_Cell (Cell_A: Cell) return Boolean is
	begin
		return Cell_A.Full = False;
	end Empty_Cell;
	
	
	function Full_List (M: Map) return Boolean is
	begin
		return M.Length = Max;
	end Full_List;
	

	function Num_Elements (M: Map) return Natural is
	begin
		return M.Length;
	end Num_Elements;
	
	
	function Last_Element (M: Map) return Natural is
	begin
		return M.Length-1;
	end Last_Element;


	function Pos_In_Map (M: Map; Pos: Natural) return Boolean is
	begin
		return Pos <= M.P_array'Last;
	end Pos_In_Map;
	
	
	function Key_In_Pos (M: Map;
						 Key: Key_Type;
						 Pos: Natural) return Boolean is
	begin
		return Pos_In_Map (M, Pos) and then
			   not Empty_Cell (M.P_Array(Pos)) and then
			   M.P_Array(Pos).Key = Key;
	end Key_In_Pos;
	
		
	--Copia Cell1 en Cell2
	procedure Copy_Cell (M: in out Map; 
						 Cell1: Natural; 
						 Cell2: Natural) is
	begin
		M.P_array(Cell2).Key:= M.P_array(Cell1).Key;
		M.P_array(Cell2).Value:= M.P_array(Cell1).Value;
		M.P_array(Cell2).Full:= M.P_array(Cell1).Full;
	end Copy_Cell;
	
	
	--Busca Key en map de una forma binaria(válido cuando en map
	--hay más de 1 elemento). Si no la encuentra devuelve
	--la pocición idonea donde debería estar.
	procedure Binary_Search (M: Map; 
							 Key: Key_Type; 
							 Pos: out Natural) is

		Middle, First, Last, Long: Natural;
		Found: Boolean;
	begin
		--Son las características del intervalo en donde buscaré
		First:= M.P_array'First;
		Last:= Last_Element (M);
		Long:= Last - First +1;
		
		--Compruebo que no esté en los extremos del intervalo
		if M.P_Array(First).Key = Key then
			Pos:= First;
		elsif M.P_Array(Last).Key = Key then
			Pos:= Last;
		else --Inicio la búsqueda binaria
			Found:= False;
			
			while not Found and Long > 2 loop
				Middle:= (First + Last)/2;

				if M.P_Array(Middle).Key = Key then
					Pos:= Middle;
					Found:= True;
				elsif Key < M.P_Array(Middle).Key then
					Last:= Middle;
					Long:= Last - First +1;
				else
					First:= Middle;
					Long:= Last - First +1;
				end if;
			end loop;
			
			--Si no se ha encontrado Key tenemos 3 opciones
			if not Found then
				if Key < M.P_Array(First).Key then
					Pos:= First;
				elsif Key < M.P_Array(Last).Key then
					Pos:= Last;
				else
					Pos:= Last + 1;
				end if;
			end if;
		end if;
	end Binary_Search;
	
	
	--Devuelve la posición idonea en la que debería estar
	--Key(suponiendo que tenemos un array infinito). 
	--Si Key ya existe devuelve su posición.
	function Key_Pos (M: Map; Key: Key_Type) return Natural is
		Pos: Natural;
	begin
		if Empty_Map(M) then
			return M.P_array'First;
		elsif Num_Elements(M) = 1 then
			if Key = M.P_Array(M.P_array'First).Key or
			   Key < M.P_Array(M.P_array'First).Key then
				return M.P_array'First;
			else
				return M.P_array'First + 1;
			end if;
		else
			Binary_Search(M, Key, Pos);
			return Pos;
		end if;
	end Key_Pos;
	
	
	--Mueve la "rodaja" que va desde First hasta
	--Last una posición hacia abajo
	procedure Move_Slice_Down (M: in out Map; First: Natural) is

		Last: Natural;
	begin
		Last:= Last_Element (M);
		
		for K in reverse First..Last loop
			Copy_Cell(M, K, K+1);
		end loop;
		
		M.P_array(First).Full:= False;
	end Move_Slice_Down;
	
	
	--Mueve la "rodaja" que va desde First hasta
	--Last una posición hacia arriba
	procedure Move_Slice_Up (M: in out Map; First: Natural) is
	
		Last: Natural;
	begin
		Last:= M.Length-1;
		
		for K in First..Last loop
			Copy_Cell(M, K, K-1);
		end loop;
		
		M.P_array(Last).Full:= False;
	end Move_Slice_Up;


	procedure Get (M: Map;
                   Key: in  Key_Type;
                   Value: out Value_Type;
                   Success: out Boolean) is

		Pos: Natural; --Posición donde debería estar Key
	begin
		Pos:= Key_Pos (M, Key);
				
		if Pos_In_Map(M, Pos) and then Key_In_Pos (M, Key, Pos) then
			Value:= M.P_array(Pos).Value;
			Success:= True;
		else
			Success:= False;
		end if;
	end Get;
	
	
	procedure Put (M: in out Map;
                   Key: Key_Type;
                   Value: Value_Type) is
                   
		Pos: Natural; --Posición idonea para Key
	begin
		Pos:= Key_Pos (M, Key);
		
		if Pos_In_Map(M, Pos) then
			
			if Key_In_Pos (M, Key, Pos) then
				M.P_array(Pos).Value:= Value;
			elsif Full_List (M) then
				raise Full_Map;
			else
				if not Empty_Map(M) and then 
				   Pos <= Last_Element(M) then --Debo hacer un hueco
					Move_Slice_Down(M, Pos);
				end if;
				M.P_array(Pos).Key:= Key;
				M.P_array(Pos).Value:= Value;
				M.P_array(Pos).Full:= True;
				M.Length:= M.Length + 1;
			end if;
			
		else
			raise Full_Map;
		end if;
	end Put;
	

	procedure Delete (M: in out Map;
                      Key: in  Key_Type;
                      Success: out Boolean) is
		
		Pos: Natural; --Posición donde debería estar Key
    begin
		Pos:= Key_Pos (M, Key);
    
		if Pos_In_Map(M, Pos) and then Key_In_Pos (M, Key, Pos) then
			M.P_array(Pos).Full:= False;
			if Pos /= Last_Element(M) then --Debo tapar 1 hueco
				Move_Slice_Up(M, Pos+1);
			end if;
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
		if C.Element_A <= Max-1 then
			return False;
		else
			return True;
		end if;
	end Out_of_Array;
	
	
	function First (M: Map) return Cursor is
		Pos: Natural;
	begin
		if Empty_Map(M) then
			Pos:= M.P_array'Last + 1; --El cursor apuntará fuera del array
		else
			Pos:= M.P_array'First;
		end if;
		
		return (M => M, Element_A => Pos);
	end First;
	
	
	procedure Next (C: in out Cursor) is
	begin
		C.Element_A:= C.Element_A + 1;
	end Next;
	
	
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
	
end Ordered_Maps_G;
