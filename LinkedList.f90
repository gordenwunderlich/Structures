submodule (linkedlist) linkedlist_funcs
use struct_util
contains
    pure module subroutine list_add(lst,dat, num)
        class(list), intent(inout) :: lst
        type(node),pointer :: tmp, lasttmp
        type(node),target :: toadd
        integer,intent(in), optional :: num
        integer :: count
        class(*), dimension(:),intent(in) :: dat
        count = 1
        if(.not.associated(lst%first)) then
            allocate(lst%first, source=node(data = dat))
        else
            tmp => lst%first
            lasttmp => null()
            do while((.not. present(num) .or. count .lt. num) .and. associated(tmp%next))
                lasttmp => tmp
                tmp => tmp%next
                count = count + 1
            enddo
            if(.not. present(num)) then
                allocate(tmp%next,source=node(data = dat))
            else
                if(.not. associated(lasttmp)) then
                    allocate(toadd%data, source=dat)
                    toadd%next => lst%first
                    lst%first => toadd
                else
                    allocate(lasttmp%next,source=node(data = dat))
                    lasttmp%next%next => tmp
                endif
            endif
        endif
        lst%len=lst%len+1
    endsubroutine
    
     pure module subroutine list_add_single(lst,dat, num)
        class(list), intent(inout) :: lst
        integer,intent(in), optional :: num
        class(*), intent(in) :: dat
        class(*), dimension(:), pointer :: datnew
        allocate(datnew(1), source = dat)
        call lst%add(datnew,num)
        deallocate(datnew)
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
    
    pure module subroutine list_remove_by_data_single(lst,dat)
        class(list), intent(inout) :: lst
        class(*), intent(in) :: dat
        class(*), dimension(:), allocatable :: tmpdat
        allocate(tmpdat(1), source = dat)
        call lst%remove(tmpdat)
        deallocate(tmpdat)
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
    
    pure module subroutine list_get_no_alloc_single(lst,res,num)
        class(list),intent(in) :: lst
        integer,optional,intent(in) :: num
        class(*), intent(out) :: res
        class(*), dimension(:), allocatable :: tmpres
        allocate(tmpres(1), mold=res)
        call lst%get(tmpres, num)
        call get_data_single(res,tmpres)
        deallocate(tmpres)
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
        class(*),dimension(:),allocatable :: res
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
        deallocate(tmptmp)
    endfunction
    
    elemental module subroutine node_destroy(nde)
        type(node), intent(inout) :: nde
        if(allocated(nde%data)) deallocate(nde%data)
    endsubroutine
    
    elemental module subroutine list_destroy(lst)
    type(list), intent(inout) :: lst
    if(associated(lst%first)) then
       call list_destroy_node(lst%first)
       deallocate(lst%first)
    endif
    endsubroutine
    
    pure recursive subroutine list_destroy_node(nde)
        type(node), intent(inout) :: nde
        if(allocated(nde%data)) deallocate(nde%data)
        if(associated(nde%next)) then
            call list_destroy_node(nde%next)
            deallocate(nde%next)
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
        !call list_destroy(dest)
        allocate(dest%first)
        dest%first=source%first
        dest%len = source%len
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