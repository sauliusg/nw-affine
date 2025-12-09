with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line; use Ada.Command_Line;

with FASTA; use FASTA;

procedure NW_Align is
   
   Default_Match_Score        : constant Integer := 1;
   Default_Mismatch_Penalty   : constant Integer := -1;
   Default_Gap_Open_Penalty   : constant Integer := -4;
   Default_Gap_Extend_Penalty : constant Integer := -2;
   
   procedure Align
     (
      Seq1, Seq2 : in Unbounded_String;
      Aln1, Aln2 : out Unbounded_String;
      Score      : out Long_Integer;
      Match_Score        : Integer := Default_Match_Score;
      Mismatch_Penalty   : Integer := Default_Mismatch_Penalty;
      Gap_Open_Penalty   : Integer := Default_Gap_Open_Penalty; 
      Gap_Extend_Penalty : Integer := Default_Gap_Extend_Penalty
     ) is
   begin
      null;
   end;
   
begin
   
   if Argument_Count >= 1 then
      Put_Line (To_String (Load_FASTA (Argument (1))));
   end if;
   
end;
