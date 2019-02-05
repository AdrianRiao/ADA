with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Operar_Enteros is
	
	function Producto (X: Integer; 
					   Y: Integer; 
					   Z: Integer) return Integer is
	--Es el producto de 3 números--
	begin
		
		return X * Y * Z;
		
	end Producto;
	
	function Suma (X: Integer; 
				   Y: Integer; 
				   Z: Integer) return Integer is
	--Es la suma de 3 números--
	begin
		
		return X + Y + Z;
		
	end Suma;
	
	procedure Leer_Entero (Numero: out Integer) is
	begin
	
		Ada.Integer_Text_IO.Get(Numero);
		
	end Leer_Entero;
	
	X: Integer;
	Y: Integer;
	Z: Integer;
	
begin

	Leer_Entero (X);
	Leer_Entero (Y);
	Leer_Entero (Z);
	Ada.Text_IO.Put_line(Integer'Image(Producto(X,Y,Z)));
	Ada.Text_IO.Put_line(Integer'Image(Suma(X,Y,Z)));

end Operar_Enteros;
