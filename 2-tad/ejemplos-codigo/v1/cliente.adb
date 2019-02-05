with Ada.Text_IO;
With Ada.Strings.Unbounded;

with Lists;

procedure Cliente is
  package ASU renames Ada.Strings.Unbounded;

  Mi_Lista: Lists.Cell_A;
begin

  Lists.Add (Mi_Lista, ASU.To_Unbounded_String("Pepe"), 39);
  Lists.Add (Mi_Lista, ASU.To_Unbounded_String("Juan"), 22);
  Lists.Add (Mi_Lista, ASU.To_Unbounded_String("Lola"), 19);


  Ada.Text_IO.Put_Line ("Contenidos de la lista:");
  Lists.Print_All (Mi_Lista);

  Mi_Lista.Next := null; -- ¡ El programador del cliente
                         --   puede escribir esta línea, que compila, 
                         --   lo que destroza la implementación de la lista!

  Ada.Text_IO.New_Line;

  Ada.Text_IO.Put_Line ("Contenidos de la lista:");
  Lists.Print_All (Mi_Lista);

end Cliente;

