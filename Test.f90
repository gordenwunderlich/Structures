program asdasfvcxy    
    use LinkedList
    call test()
contains
    subroutine test()
        type(list) :: a
        type(list) :: b
        type(list), dimension(1) :: bb
        integer,dimension(3) :: y
        real,dimension(2) :: zz
        character(3):: qq
        real,dimension(2) :: z
        character(3) :: q
        integer, dimension(3) :: x
        x=5
        z=2.5
        q="asd"
        call a%add(x)
        call a%add(z)
        call a%add(q)
        call b%add(q)
        call a%add("yxc")
        b=a
        call a%remove("asd")
        call a%add((/b/))
        call a%get(y)
        call a%get(zz,2)
        call a%get(qq,3)
        call a%get(bb,4)
        print *,"x=", y
        print *, "z=", zz
        print *,"q=", qq
        print *, "length:", b%length()
        call b%get(y)
        call b%get(zz,2)
        call b%get(qq,3)
        print *,"x=", y
        print *, "z=", zz
        print *,"q=", qq
        print *, "length:", bb(1)%length()
        call bb(1)%get(y)
        call bb(1)%get(zz,2)
        call bb(1)%get(qq,3)
        print *,"x=", y
        print *, "z=", zz
        print *,"q=", qq
        call a%remove(x)
        call a%get(zz,1)
        print *, "z=", zz
        call a%remove_at(1)
        call a%get(qq,1)
        print *,"q=", qq
        call b%add(x,3)
        y = 0
        call b%get(y)
        print *,"x=", y
        print *, a%length()
        call b%remove_at(3)
        call a%remove(b)
        print *, a%length()
    endsubroutine
endprogram