submodule (struct_util) get_data_func
	use LinkedList
	!include custom types here
	contains
		pure module subroutine get_data(dest,source)
			class(*), dimension(:), intent(out) :: dest
			class(*), dimension(:), intent(in) :: source
			if(same_type_as(dest,source)) then
				select type(s => source)
#					define __operation__ d = s
#					define __logoperation__ __operation__
#					include "Type_selects.fi"
#					undef __operation__
#					undef __logoperation__
				endselect
			endif
		endsubroutine
    
		pure module subroutine get_data_single(dest,source)
			class(*), intent(out) :: dest
			class(*), dimension(:), intent(in) :: source
			if(same_type_as(dest,source)) then
				select type(s => source(1))
#					define __operation__ d = s
#					define __logoperation__ __operation__
#					include "Type_selects.fi"
#					undef __operation__
#					undef __logoperation__
				endselect
			endif
		endsubroutine
    
		pure module function compare_data(dest,source) result(res)
			class(*), dimension(:), intent(in) :: dest, source
			logical :: res
			res = .false.
			if(same_type_as(dest,source)) then
				select type(s => source)
#					define __operation__ res = all(s .eq. d)
#					define __logoperation__ res = all(s .eqv. d)
#					include "Type_selects.fi"
#					undef __operation__
#					undef __logoperation__
				endselect
			endif
		endfunction
    
endsubmodule    