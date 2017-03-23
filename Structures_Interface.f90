module struct_util
    interface
        pure module subroutine get_data(dest,source)
            class(*), dimension(:), intent(out) :: dest
            class(*), dimension(:), intent(in) :: source
        endsubroutine
    
        pure module subroutine get_data_single(dest,source)
            class(*), intent(out) :: dest
            class(*), dimension(:), intent(in) :: source
        endsubroutine

        pure module function compare_data(data1,data2) result(res)
            class(*), dimension(:), intent(in) :: data1, data2
            logical :: res
        endfunction
    
    endinterface
endmodule
