module LinkedList
    private
    public :: list
    type node
        class(*),dimension(:),allocatable  :: data
        type(node),pointer :: next => null()
    contains
        procedure, private :: node_cpy
        generic :: assignment(=) => node_cpy
        final :: node_destroy
    endtype
    
    type :: list
        private
        integer :: len
        type(node),pointer :: first => null()
    contains
        procedure :: add => list_add
        procedure,private :: list_get_no_alloc
        procedure,private :: list_get_alloc
        generic :: get => list_get_no_alloc, list_get_alloc
        procedure :: getf => list_getf
        procedure, private :: list_remove_by_data
        procedure, private :: list_remove_by_num
        generic :: remove => list_remove_by_data, list_remove_by_num
        procedure :: length => list_length
        procedure, private, pass(dest) :: lst_cpy => list_cpy
        generic :: assignment(=) => lst_cpy
        final :: list_destroy
    endtype
    interface
        pure module subroutine list_add(lst,dat,num)
            class(list), intent(inout) :: lst
            class(*), dimension(:), intent(in) :: dat
            integer, intent(in), optional :: num
        endsubroutine
    
        pure module subroutine list_remove_by_data(lst,dat)
            class(list), intent(inout) :: lst
            class(*), dimension(:), intent(in) :: dat
        endsubroutine
    
    
        elemental module subroutine list_remove_by_num(lst,num)
            class(list), intent(inout) :: lst
            integer,optional, intent(in) :: num
            endsubroutine
    
    
        pure module subroutine list_get_no_alloc(lst,res,num)
            class(list),intent(in) :: lst
            integer,optional,intent(in) :: num
            class(*),dimension(:), intent(out) :: res
            endsubroutine
    
        pure module subroutine list_get_alloc(lst,num, resstar)
            class(list),intent(in) :: lst
            integer,optional,intent(in) :: num
            class(*),dimension(:), allocatable, intent(out) :: resstar
        endsubroutine
    
    
        pure module function list_getf(lst,num) result(res)
            class(list),intent(in) :: lst
            integer,optional,intent(in) :: num
            class(*),dimension(:),pointer :: res
        endfunction
    
        elemental module subroutine node_destroy(nde)
            type(node), intent(inout) :: nde
        endsubroutine
    
        elemental module subroutine list_destroy(lst)
        type(list), intent(inout) :: lst
        endsubroutine
    
        elemental module function list_length(lst) result(res)
            class(list), intent(in) :: lst
            integer :: res
        endfunction
    
        elemental module subroutine list_cpy(dest, source)
            class(list), intent(in) :: source
            class(list), intent(out) :: dest
        endsubroutine
    
        pure recursive module subroutine node_cpy(dest, source)
            class(node),  intent(in) :: source
            class(node),  intent(out) :: dest
        endsubroutine
    endinterface
endmodule
