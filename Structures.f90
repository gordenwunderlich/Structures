submodule (struct_util) get_data_func
use LinkedList
!include custom types here
contains
    pure module subroutine get_data(dest,source)
        class(*), dimension(:), intent(out) :: dest
        class(*), dimension(:), allocatable, intent(in) :: source
        select type(s=>source)
        class default
            select type(d => dest)
                type is (integer)
                    d=transfer(s,d)
                type is (real)
                    d=transfer(s,d)
                type is (character(*))
                    d=transfer(s,d)
                type is (complex)
                    d=transfer(s,d)
                type is (logical)
                    d=transfer(s,d)
                type is (list)
                    d=transfer(s,d)
                !add custom type selects here
            endselect
        endselect
    endsubroutine
    
    pure module subroutine get_data_single(dest,source)
        class(*), intent(out) :: dest
        class(*), dimension(:), allocatable, intent(in) :: source
        select type(s=>source(1))
        class default
            select type(d => dest)
                type is (integer)
                    d=transfer(s,d)
                type is (real)
                    d=transfer(s,d)
                type is (character(*))
                    d=transfer(s,d)
                type is (complex)
                    d=transfer(s,d)
                type is (logical)
                    d=transfer(s,d)
                type is (list)
                    d=transfer(s,d)
                !add custom type selects here
            endselect
        endselect
    endsubroutine
endsubmodule    