with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with FASTA; use FASTA;

procedure NW_Align is
   
begin
   Put_Line (To_String (Load_FASTA ("seq1.fasta")));
end;
