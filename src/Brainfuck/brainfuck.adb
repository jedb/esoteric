

with Ada.Text_IO;
with Ada.Direct_IO;
with Ada.Command_Line;


procedure Brainfuck is


    package CL renames Ada.Command_Line;
    package Text_IO renames Ada.Text_IO;


    package Direct_IO is new Ada.Direct_IO(Character);
    use type Direct_IO.Count;


    type Cell is mod 256;
    type Tape_Position is mod 5000;
    type Program_Tape is array (Tape_Position'Range) of Cell;
    pragma Pack (Program_Tape);


    Input_File : Direct_IO.File_Type;
    File_Position : Direct_IO.Positive_Count;
    Counter : Integer;
    Current_Char : Character;
    Tape : Program_Tape;
    Current_Cell : Tape_Position;


begin


    if CL.Argument_Count /= 1 then
        Text_IO.Put_Line(Text_IO.Standard_Error, "Please supply the filename of a brainfuck program");
        CL.Set_Exit_Status(CL.Failure);
        return;
    end if;


    begin
        Direct_IO.Open( File => Input_File,
        	            Mode => Direct_IO.In_File,
        	            Name => CL.Argument(1) );
    exception
        when others =>
            Text_IO.Put_Line(Text_IO.Standard_Error, "Can not open the file '" & CL.Argument(1) & "'. Does it exist?");
            CL.Set_Exit_Status(CL.Failure);
            return;
    end;


    Tape := (others => 0);
    Current_Cell := Tape_Position'First;

    while not Direct_IO.End_Of_File(Input_File) loop
        Direct_IO.Read(Input_File, Current_Char);
        
        case Current_Char is

            when '>' =>
                Current_Cell := Current_Cell + 1;

            when '<' =>
                Current_Cell := Current_Cell - 1;
            
            when '+' =>
                Tape(Current_Cell) := Tape(Current_Cell) + 1;
            
            when '-' =>
                Tape(Current_Cell) := Tape(Current_Cell) - 1;
            
            when '.' =>
                Text_IO.Put(Character'Val(Tape(Current_Cell)));
            
            when ',' =>
                Text_IO.Get(Current_Char);
                Tape(Current_Cell) := Character'Pos(Current_Char);

            when '[' =>
                if Tape(Current_Cell) = 0 then
                    Counter := 1;
                    while Counter > 0 and not Direct_IO.End_Of_File(Input_File) loop
                        Direct_IO.Read(Input_File, Current_Char);
                        if Current_Char = '[' then
                            Counter := Counter + 1;
                        elsif Current_Char = ']' then
                            Counter := Counter - 1;
                        end if;
                    end loop;
                end if;

            when ']' =>
                if Tape(Current_Cell) /= 0 then
                    Counter := 0;
                    File_Position := Direct_IO.Index(Input_File) - 1;
                    loop
                        Direct_IO.Set_Index(Input_File, File_Position);
                        Direct_IO.Read(Input_File, Current_Char);
                        if Current_Char = ']' then
                            Counter := Counter + 1;
                        elsif Current_Char = '[' then
                            Counter := Counter - 1;
                        end if;
                        File_Position := File_Position - 1;
                        exit when Counter < 1;
                    end loop;
                end if;

            when others =>
                null;

        end case;

    end loop;


    Direct_IO.Close(Input_File);


end Brainfuck;

