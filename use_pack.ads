-- Copyright (C) 2017  Tony Häger,  tony7@tele2.se

with Ada.Text_Io;
with Interfaces;
with Unchecked_Deallocation;
with Ada.Calendar;
with Generics; pragma Elaborate_All(Generics);

package Use_Pack is

   subtype Integer_16 is Interfaces.Integer_16;
   type Array_Of_Naturals is array(Natural range <>) of Natural;

   type String_P is access String;
   procedure Free is new Unchecked_Deallocation(String, String_P);
   function "="(L, R : String_P) return Boolean;
   function Hash(Sp : String_P) return Natural;


   procedure Assert(B : in Boolean; Text : in String := "");

   procedure Inc is new Generics.Inc(Integer);
   procedure Dec is new Generics.Dec(Integer);
   procedure Add is new Generics.Add(Integer, Integer);
   procedure Swap is new Generics.Swap(Integer_16);
   function Min(X1, X2 : Integer) return Integer;

   type Character_Set is private;
   Letter, Lc_Letter, Uc_Letter, Digit, Space, Tab_C, White_Space : constant Character_Set;
   function "not" (Right       : in Character_Set) return Character_Set;
   function "or"  (Left, Right : in Character_Set) return Character_Set;

   function Is_A(C : Character_Set; Ch : Character) return Boolean;
   function Find(C : Character_Set; S : String) return Natural;

   function Hash(S : String) return Natural;

   procedure Wait_Key(C : Character := Ascii.Nul);
   type Bstring(Max_Length : Natural) is private;

   function "&" is new Generics.Cat(Integer);
   function "&" (Left : in Integer; Right : in String) return String;
   function "&" (Left : in Integer; Right : in Integer) return String;


   subtype File_Type is Ada.Text_Io.File_Type;


   subtype File_Mode is Ada.Text_Io.File_Mode;
   In_File : constant File_Mode := Ada.Text_Io.In_File;

   procedure Put_Line(Item : in String) renames Ada.Text_Io.Put_Line;
   function End_Of_File (File : in File_Type) return Boolean renames
      Ada.Text_Io.End_Of_File;
   function End_Of_File return Boolean renames Ada.Text_Io.End_Of_File;

   procedure Get_Line
      (File : in File_Type;
      Item : out String;
      Last : out Natural) renames Ada.Text_Io.Get_Line;
   procedure Get_Line
      (Item : out String;
      Last : out Natural) renames Ada.Text_Io.Get_Line;

   procedure Open
      (File : in out File_Type;
      Mode : in File_Mode;
      Name : in String;
      Form : in String := "") renames Ada.Text_Io.Open;
   procedure Close (File : in out File_Type) renames Ada.Text_Io.Close;

   ---------------------------------------------------------
   function To_String(B : Bstring) return String;
   function Last_Char(B : Bstring) return Character;
   procedure Add(B : in out Bstring; S : in String);
   ---------------------------------------------------------
   type Timer is private;

   procedure Start(T : in out Timer);
   procedure Stop(T : in out Timer);
   procedure Put_Line(T : in Timer);

private
   type Character_Set_Internal is mod 64;
   for Character_Set_Internal'Size use 6;

   type Character_Set is new Character_Set_Internal;

   Lc_Letter   : constant Character_Set := 1;
   Uc_Letter   : constant Character_Set := 2;
   Letter      : constant Character_Set := Lc_Letter + Uc_Letter;
   Digit       : constant Character_Set := 4;
   Space       : constant Character_Set := 8;
   Tab_C       : constant Character_Set := 16;
   White_Space : constant Character_Set := Space + Tab_C;
   Other       : constant Character_Set := 32;

   type Bstring(Max_Length : Natural) is record
      Length : Natural := 0;
      Data : String (1..Max_Length);
   end record;

   type Timer is record
      Start_Time, Stop_Time : Ada.Calendar.Time;
   end record;

   pragma Inline(Add);
   pragma Inline(Min);
   pragma Inline(Start);
   pragma Inline(Stop);
end;