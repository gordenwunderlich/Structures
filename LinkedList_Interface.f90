module LinkedList
	!general notes
		!compatible data must be of any type listed in structures.f90 you can easily add your own types in the same mannner as list was added as long as they are assignable and comparable
		!all procedures of this module are private you have to use the type-bound procedures
		!remember that this list (like fortran arrays) starts with position 1 not 0
	private
	public :: list
	type node
		class(*),dimension(:),allocatable  :: data
		type(node),pointer :: next => null()
	contains
		procedure, private :: node_cpy !copys a node
		generic :: assignment(=) => node_cpy !overwrites intrinsic assignment
		final :: node_destroy !deallocates the nodes data on node deallocation
	endtype
	
	type :: list
		private
		integer :: len !length
		type(node),pointer :: first => null()
	contains
		procedure, private :: list_add !adds an array to the list
		procedure, private :: list_add_single !wraps a scalar element in an array and adds it to the list
		generic :: add => list_add, list_add_single !allows you to call add with scalar and array valued elements
		procedure,private :: list_get_no_alloc !returns an element from the list into a non-allocatable array
		procedure,private :: list_get_alloc !returns an element from the list into a allocatable array
		procedure,private :: list_get_no_alloc_single !returns an element from the list into a non-allocatable scalar
		generic :: get => list_get_no_alloc, list_get_alloc, list_get_no_alloc_single !allows you to call add with scalar and array valued elements and also with allocatable arrays
		procedure :: getf => list_getf !a function which returns a class(*) array (broken right now due to a compiler bug)
		procedure, private :: list_remove_by_data !removes a node from the list with the supplied data array
		procedure, private :: list_remove_by_data_single !wraps a scalar in an array and call remove_by_data with that array
		procedure, private :: list_remove_by_num !removes a node from the list at the specified position
		generic :: remove => list_remove_by_data, list_remove_by_data_single !lets you call remove with array and scalar valued elements
		generic :: remove_at => list_remove_by_num !renames the remove_by_num procedure
		procedure :: length => list_length !returns the length of the list
		procedure, private, pass(dest) :: lst_cpy => list_cpy !list copy function
		procedure, private :: list_compare !list compare function
		generic :: operator(.eq.) => list_compare !allows comparing of lists with the .eq. operator
		generic :: assignment(=) => lst_cpy !overwrites intrinsic assignment
		final :: list_destroy !correctly deallocates all components of a list when it is deallocated
	endtype
	interface
	
		!adds an array to the list
		pure module subroutine list_add(lst,dat,num)
			class(list),						intent(inout) :: lst	!the passed list (this is passed automatically when lst%add(dat,num) is called and you can't explicitly call lst%add(lst,dat,num)
			class(*), dimension(:),	intent(in) :: dat		!the data array to be added
			integer, optional,			intent(in) :: num	!the position where the element is to be inserted. if this is not supplied the element will be added to the end of the list
		endsubroutine
		
		!wraps a scalar element in an array and adds it to the list
		pure module subroutine list_add_single(lst,dat,num)
			class(list),				intent(inout) :: lst	!the passed list (this is passed automatically when lst%add(dat,num) is called and you can't explicitly call lst%add(lst,dat,num)
			class(*),				intent(in) :: dat		!the data scalar to be added
			integer, optional,	intent(in) :: num	!the position where the element is to be inserted. if this is not supplied the element will be added to the end of the list
		endsubroutine

		!removes a node from the list with the supplied data array
		pure module subroutine list_remove_by_data(lst,dat)
			class(list),						intent(inout) :: lst	!the passed list (this is passed automatically when lst%remove(dat) is called and you can't explicitly call lst%remove(lst,dat)
			class(*), dimension(:),	intent(in) :: dat		!the data array of the element to be removed
		endsubroutine
	
		!wraps a scalar in an array and call remove_by_data with that array
		pure module subroutine list_remove_by_data_single(lst,dat)
			class(list),	intent(inout) :: lst	!the passed list (this is passed automatically when lst%remove(dat) is called and you can't explicitly call lst%remove(lst,dat)
			class(*),	intent(in) :: dat		!the data scalar of the element to be removed
		endsubroutine
		
		!removes a node from the list at the specified position
		elemental module subroutine list_remove_by_num(lst,num)
			class(list),				intent(inout) :: lst	!the passed list (this is passed automatically when lst%remove(num) is called and you can't explicitly call lst%remove(lst,num)
			integer,optional,		intent(in) :: num	!the number(position) of the element you want to remove
			endsubroutine
	
		!returns an element from the list into a non-allocatable array
		pure module subroutine list_get_no_alloc(lst,res,num)
			class(list),						intent(in) :: lst		!the passed list (this is passed automatically when lst%get(res,num) is called and you can't explicitly call lst%get(lst,res,num)
			integer,optional,				intent(in) :: num	!the number(position) of the element you want to extract
			class(*),dimension(:),      	intent(out) :: res	!data array of the same type as the data you want to extract from the list
		endsubroutine
		
		!returns an element from the list into a non-allocatable scalar
		pure module subroutine list_get_no_alloc_single(lst,res,num)
			class(list),				intent(in) :: lst		!the passed list (this is passed automatically when lst%get(res,num) is called and you can't explicitly call lst%get(lst,res,num)
			integer,optional,		intent(in) :: num	!the number(position) of the element you want to extract
			class(*),				intent(out) :: res	!data array of the same type as the data you want to extract from the list
		endsubroutine
	
		!returns an element from the list into a allocatable array
		pure module subroutine list_get_alloc(lst,num, resstar)
			class(list),										intent(in) :: lst			!the passed list (this is passed automatically when lst%get(num, resstar) is called and you can't explicitly call lst%get(lst,num,resstar)
			integer,optional,								intent(in) :: num		!the number(position) of the element you want to extract
			class(*),dimension(:), allocatable,	intent(out) :: resstar	!allocatable data array of the same type as the data you want to extract from the list
		endsubroutine
	
		!a function which returns a class(*) array (broken right now due to a compiler bug)
		pure module function list_getf(lst,num) result(res)
			class(list),									intent(in) :: lst		!the passed list (this is passed automatically when lst%getf(num) is called and you can't explicitly call lst%getf(lst,num)
			integer,optional,							intent(in) :: num	!the number(position) of the element you want to extract
			class(*),dimension(:),allocatable					:: res		!returns a copy of the elements data
		endfunction
	
		!deallocates the nodes data on node deallocation
		elemental module subroutine node_destroy(nde)
			type(node), intent(inout) :: nde
		endsubroutine
		
		!correctly deallocates all components of a list when it is deallocated
		elemental module subroutine list_destroy(lst)
		type(list), intent(inout) :: lst
		endsubroutine
	
		!returns the length of the list
		elemental module function list_length(lst) result(res)
			class(list),	intent(in) :: lst	!the passed list (this is passed automatically when lst%length() is called and you can't explicitly call lst%length(lst)
			integer					:: res		!returns the len attribute of the list
		endfunction
	
		!list copy function
		elemental module subroutine list_cpy(dest, source)
			class(list), intent(in) :: source
			class(list), intent(out) :: dest
		endsubroutine
	
		!list compare function
		elemental module function list_compare(lst1, lst2) result(res)
			class(list), intent(in) :: lst1, lst2
			logical :: res
		endfunction
	
		!copys a node
		!since fortran assignment(=) is not a ponter assignment like => we can't just let one node point to another so we have to copy the data into a new node and also recursively copy all following nodes
		pure recursive module subroutine node_cpy(dest, source)
			class(node),  intent(in) :: source
			class(node),  intent(out) :: dest
		endsubroutine
	endinterface
endmodule
