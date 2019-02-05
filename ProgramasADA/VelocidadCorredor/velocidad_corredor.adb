with Ada.Text_IO;
with Ada.Integer_Text_IO;

--Calcula la velocidad en pies por segundo
--y km por segundo teniendo el tiempo
--que tarda en recorrer 1 milla
procedure Velocidad_Corredor is

	type Tiempo is record
		Minutos: Integer;
		Segundos: Integer;
	end record;
	
	SegEnMin: constant Integer:= 60; --Número de segundos en 1 minuto--
	
	--Devuelve el número de segundos totales
	--dado un determinado tiempo en minutos y segundos
	function Segundos_Totales (T: Tiempo) return Integer is
	begin
	
		return SegEnMin * T.Minutos + T.Segundos;
		
	end Segundos_Totales;
	procedure Leer_Segundos (Segundos: out Integer) is
	begin
	
		Ada.Text_IO.Put("Introducir Segundos: ");
		Ada.Integer_Text_IO.Get(Segundos);
		
	end Leer_Segundos;
	
	procedure Imprimir_Velocidad2 (Segundos: Integer) is
	begin
	

	
	end Imprimir_Velocidad2;
	
	procedure Imprimir_Velocidad1 (Segundos: Integer) is
	begin
	

	
	end Imprimir_Velocidad1;
	
	procedure Leer_Minutos (Minutos: out Integer) is
	begin
	
		Ada.Text_IO.Put("Introducir Minutos: ");
		Ada.Integer_Text_IO.Get(Minutos);
	
	end Leer_Minutos;
	
	procedure Leer_Tiempo (T: out Tiempo) is
	begin
	
		Leer_Minutos (T.Minutos);
		Leer_Segundos (T.Segundos);
		
	end Leer_Tiempo;
	
	T: Tiempo;
	STotales: Integer; --Segundos totales--
	
begin
	Leer_Tiempo (T);
	STotales:= Segundos_Totales(T);
	Imprimir_Velocidad1 (STotales);
	Imprimir_Velocidad2 (STotales);
end Velocidad_Corredor;
