!module containing helper functions for all structures
	
module struct_util
    interface
	
		!helper function to copy data from one class(*) array to another
        pure module subroutine get_data(dest,source)
            class(*), dimension(:), intent(out) :: dest
            class(*), dimension(:), intent(in) :: source
        endsubroutine
    
		!helper function to copy data from one class(*) array to a class(*) scalar	
        pure module subroutine get_data_single(dest,source)
            class(*), intent(out) :: dest
            class(*), dimension(:), intent(in) :: source
        endsubroutine

		!helper function to compare to class(*) arrays
        pure module function compare_data(data1,data2) result(res)
            class(*), dimension(:), intent(in) :: data1, data2
            logical :: res
        endfunction
    
    endinterface
endmodule
