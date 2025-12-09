with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line; use Ada.Command_Line;

with FASTA; use FASTA;

procedure NW_Align is
   
begin
   
   if Argument_Count >= 1 then
      Put_Line (To_String (Load_FASTA (Argument (1))));
   end if;
   
end;
