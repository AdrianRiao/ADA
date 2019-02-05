with Ada.Text_IO;

-- Programa que devuelve el número
-- exacto de euros y céntimos de euro

procedure Cuenta_Monedas is
	
	type Monedas is array (1..8) of Natural;

	procedure Calcular_Cent (Mon: Monedas;Cent: out Integer) is
	begin
	Cent:= 0;
		for Contador in 1..8 loop
		
			case Contador is
				when 1 =>
					Cent:= Cent + Mon(Contador);
				when 2 =>
					Cent:= Cent + Mon(Contador) * 2;
				when 3 =>
					Cent:= Cent + Mon(Contador) * 5;
				when 4 =>
					Cent:= Cent + Mon(Contador) * 10;
				when 5 =>
					Cent:= Cent + Mon(Contador) * 20;
				when 6 =>
					Cent:= Cent + Mon(Contador) * 50;
				when 7 =>
					Cent:= Cent + Mon(Contador) * 100;
				when 8 =>
					Cent:= Cent + Mon(Contador) * 200;
			end case;
			
		end loop;
	
	end Calcular_Cent;
	
	Mon: Monedas:= (1,2,0,10,0,5,100,0);
	CentTotal: Integer; --Es el número total de céntimos que tenemos--
	Cent: Integer; --Son los céntimos de euro--
	Eur: Integer;

begin

	Calcular_Cent (Mon, CentTotal);
	Eur:= CentTotal / 100;
	Cent:= CentTotal mod 100;
	Ada.Text_IO.Put_line("Esas monedas son:" & Integer'Image(Eur)
						 & " euros y" & Integer'Image(Cent) &
						 " céntimos de euro");
	
end Cuenta_Monedas;
	
