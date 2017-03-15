submodule (linkedlist) linkedlist_funcs
use struct_util
contains
    pure module subroutine list_add(lst,dat)
        class(list), intent(inout) :: lst
        type(node),pointer :: tmp
        class(*), dimension(:),intent(in) :: dat
        if(.not.associated(lst%first)) then
            allocate(lst%first, source=node(data = dat))
        else
            tmp=>lst%first
            do while(associated(tmp%next))
                tmp=>tmp%next
            enddo
            allocate(tmp%next,source=node(data = dat))
        endif
        lst%len=lst%len+1
    endsubroutine
    
    
    pure module subroutine list_remove_by_data(lst,dat)
        class(list), intent(inout) :: lst
        class(*), dimension(:), intent(in) :: dat
        type(node),pointer :: tmp,last
        integer(1), dimension(:),allocatable :: bytetest
        if(.not. ASSOCIATED(lst%first)) return
        tmp => lst%first
        if(same_type_as(lst%first%data, dat) .and. all(transfer(lst%first%data,bytetest) .eq. transfer(dat,bytetest))) then 
                if(associated(lst%first%next)) then 
                    lst%first => tmp%next
                else
                    lst%first => null()
                endif
                deallocate(tmp)
                lst%len = lst%len-1
                return
        endif
        do i=2,lst%len
            last => tmp
            if(associated(tmp%next))tmp => tmp%next
            if(same_type_as(tmp%data, dat) .and. all(transfer(tmp%data,bytetest) .eq. transfer(dat,bytetest))) then 
                if(associated(tmp%next)) then 
                    last%next => tmp%next
                else
                    last%next => null()
                endif
                deallocate(tmp)
                lst%len = lst%len-1
                return
            endif
        enddo
    endsubroutine
    
        
    
    elemental module subroutine list_remove_by_num(lst,num)
        class(list), intent(inout) :: lst
        integer,optional, intent(in) :: num
        type(node),pointer :: tmp,last
        integer(1), dimension(:),allocatable :: bytetest
        if(.not. ASSOCIATED(lst%first)) return
        tmp => lst%first
        if(num .eq. 1) then 
                if(associated(tmp%next)) then 
                    lst%first => tmp%next
                else
                    lst%first => null()
                endif
                deallocate(tmp)
                lst%len = lst%len-1
                return
            endif
        do i=2,lst%len
            last => tmp
            if(associated(tmp%next))tmp => tmp%next
            if(num .eq. i) then 
                if(associated(tmp%next)) then 
                    last%next => tmp%next
                else
                    last%next => null()
                endif
                deallocate(tmp)
                lst%len = lst%len-1
                return
            endif
        enddo
    endsubroutine
    
    
    pure module subroutine list_get_no_alloc(lst,res,num)
        class(list),intent(in) :: lst
        integer,optional,intent(in) :: num
        integer :: tnum
        class(*),dimension(:), intent(out) :: res
        type(node), pointer :: tmp,tmptmp 
        tnum =1
        if(present(num)) tnum=num
        allocate(tmptmp ,source=lst%first)
        tmp => tmptmp
        do i=1,tnum-1
            tmp => tmp%next
        enddo
        call get_data(res,tmp%data)
        deallocate(tmptmp)
    endsubroutine
    
    pure module subroutine list_get_alloc(lst,num, resstar)
        class(list),intent(in) :: lst
        integer,optional,intent(in) :: num
        integer :: tnum
        class(*),dimension(:), allocatable, intent(out) :: resstar
        type(node), pointer :: tmp,tmptmp 
        tnum =1
        if(present(num)) tnum=num
        allocate(tmptmp ,source=lst%first)
        tmp => tmptmp
        do i=1,tnum-1
            tmp => tmp%next
        enddo
        select type(s => tmp%data)
            class default    
                allocate(resstar,source=s)
        endselect
        deallocate(tmptmp)
    endsubroutine
    
    pure module function list_getf(lst,num) result(res)
        class(list),intent(in) :: lst
        integer,optional,intent(in) :: num
        integer :: tnum
        class(*),dimension(:),pointer :: res
        type(node), pointer :: tmp,tmptmp 
        tnum =1
        if(present(num)) tnum=num
        allocate(tmptmp ,source=lst%first)
        tmp => tmptmp
        do i=1,tnum-1
            tmp => tmp%next
        enddo
        select type(s => tmp%data)
            class default
            allocate(res, source=s)
        endselect
        !print *, transfer(res,(/"asd"/))
        deallocate(tmptmp)
    endfunction
    
    elemental module subroutine node_destroy(nde)
        type(node), intent(inout) :: nde
        if(allocated(nde%data)) deallocate(nde%data)
    endsubroutine
    
    elemental module subroutine list_destroy(lst)
    type(list), intent(inout) :: lst
    type(node), dimension(lst%len) :: lstnodes
    if(associated(lst%first)) then
        lstnodes(1) = lst%first
        deallocate(lstnodes(1)%data)
        deallocate(lst%first%data)
        do i=2,lst%len
            lstnodes(i)=lstnodes(i-1)%next
            deallocate(lstnodes(i-1)%next%data)
            deallocate(lstnodes(i-1)%next)
            deallocate(lstnodes(i)%data)
        enddo
        deallocate(lst%first)
    endif
    endsubroutine
    
    elemental module function list_length(lst) result(res)
        class(list), intent(in) :: lst
        integer :: res
        res = lst%len
    endfunction
    
    elemental module subroutine list_cpy(dest, source)
        class(list), intent(in) :: source
        class(list), intent(out) :: dest
        class(*), dimension(:), allocatable :: tmp
        call list_destroy(dest)
        select type(source)
            class is(list)
                allocate(dest%first)
                dest%first=source%first
                dest%len = source%len
        endselect
    endsubroutine
    
    pure recursive module subroutine node_cpy(dest, source)
        class(node),  intent(in) :: source
        class(node),  intent(out) :: dest
        allocate(dest%data, source=source%data)
        if(associated(source%next)) then 
            allocate(dest%next)
            dest%next = source%next
        endif
    endsubroutine
    

endsubmodule