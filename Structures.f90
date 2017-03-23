submodule (struct_util) get_data_func
use LinkedList
!include custom types here
contains
    pure module subroutine get_data(dest,source)
        class(*), dimension(:), intent(out) :: dest
        class(*), dimension(:), intent(in) :: source
        if(same_type_as(dest,source)) then
            select type(s => source)
            type is (integer)
                select type(d => dest)
                    type is (integer)
                        d = s
                    endselect
                type is (real)
                    select type(d => dest)
                    type is (real)
                        d = s
                    endselect
                type is (character(*))
                    select type(d => dest)
                    type is (character(*))
                        d = s
                    endselect
                type is (complex)
                    select type(d => dest)
                    type is (complex)
                        d = s
                    endselect
                type is (logical)
                    select type(d => dest)
                    type is (logical)
                        d = s 
                    endselect
                type is (list)
                    select type(d => dest)
                    type is (list)
                        d = s
                    endselect
                !add custom type selects here
            endselect
        endif
    endsubroutine
    
    pure module subroutine get_data_single(dest,source)
        class(*), intent(out) :: dest
        class(*), dimension(:), intent(in) :: source
        if(same_type_as(dest,source)) then
            select type(s => source(1))
            type is (integer)
                select type(d => dest)
                    type is (integer)
                        d = s
                    endselect
                type is (real)
                    select type(d => dest)
                    type is (real)
                        d = s
                    endselect
                type is (character(*))
                    select type(d => dest)
                    type is (character(*))
                        d = s
                    endselect
                type is (complex)
                    select type(d => dest)
                    type is (complex)
                        d = s
                    endselect
                type is (logical)
                    select type(d => dest)
                    type is (logical)
                        d = s 
                    endselect
                type is (list)
                    select type(d => dest)
                    type is (list)
                        d = s
                    endselect
                !add custom type selects here
            endselect
        endif
    endsubroutine
    
    pure module function compare_data(data1,data2) result(res)
        class(*), dimension(:), intent(in) :: data1, data2
        logical :: res
        res = .false.
        if(same_type_as(data1,data2)) then
            select type(d1=>data1)
            type is (integer)
                select type(d2 => data2)
                    type is (integer)
                        res = all(d1 .eq. d2) 
                    endselect
                type is (real)
                    select type(d2 => data2)
                    type is (real)
                        res = all(d1 .eq. d2) 
                    endselect
                type is (character(*))
                    select type(d2 => data2)
                    type is (character(*))
                        res = all(d1 .eq. d2) 
                    endselect
                type is (complex)
                    select type(d2 => data2)
                    type is (complex)
                        res = all(d1 .eq. d2) 
                    endselect
                type is (logical)
                    select type(d2 => data2)
                    type is (logical)
                        res = all(d1 .eqv. d2) 
                    endselect
                type is (list)
                    select type(d2 => data2)
                    type is (list)
                        !res = all(d1 .eq. d2) 
                    endselect
                !add custom type selects here
            endselect
        endif
    endfunction
    
endsubmodule    