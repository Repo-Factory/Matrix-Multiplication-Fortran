module matrices
    implicit none
    contains

    subroutine populate_matrix(filename, matrix, rows, columns)
        character(*), intent(in) :: filename
        integer, intent(in) :: rows, columns
        real, intent(out) :: matrix(:,:)
        integer :: i, j
        open(10, file=filename)
        do i=1, rows
            do j=1, columns
                read(10, *) matrix(i,j)
            end do
        end do
        close(10)
    end subroutine populate_matrix

    subroutine prompt_dimensions(rows, columns)
        integer, intent(out) :: rows, columns
        print *, "Please type number of rows"
        read *, rows
        print *, "Please type number of columns"
        read *, columns
    end subroutine prompt_dimensions
end module matrices



program multMatrix
    use matrices
    implicit none

    real, allocatable :: matrix(:,:)
    integer :: rows, columns
    integer :: i, j

    call prompt_dimensions(rows, columns)
    allocate(matrix(rows, columns))
    
    call populate_matrix("matrix1.txt", matrix, rows, columns)

    do i=1, rows
        do j=1, columns
            if (j == columns) then
                write(*, fmt="(1x,f7.2)", advance="yes") matrix(i,j)
            else
                write(*, fmt="(1x,f7.2)", advance="no") matrix(i,j)
            endif
        end do
    end do
    
end program multMatrix