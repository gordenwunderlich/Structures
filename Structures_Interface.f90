module struct_util
    interface
        pure module subroutine get_data(dest,source)
            class(*), dimension(:), intent(out) :: dest
            class(*), dimension(:), allocatable, intent(in) :: source
        endsubroutine
    
        pure module subroutine get_data_single(dest,source)
            class(*), intent(out) :: dest
            class(*), dimension(:), allocatable, intent(in) :: source
        endsubroutine
    
    endinterface
endmodule
