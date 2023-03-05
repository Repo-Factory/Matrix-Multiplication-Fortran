function read_matrix(filename) result(matrix)
    implicit none
    
    ! input/output
    character(len=*), intent(in)        :: filename
    real, dimension(:,:)   :: matrix
    
    integer :: x                     ! temp storage
    integer :: i, j                  ! loop variables
    integer :: rows, columns
    
    ! open file, read first line with rows/columns, allocate matrix
    open(10, file=filename)
    read(10, *) rows, columns
    allocate(matrix(rows,columns))
    
    ! nested loop to fill matrix
    do i = 1, rows
        do j = 1, columns
            read(10, *) x
            matrix(i,j) = x
        end do
    end do
    
    close(10)
end function read_matrix
    
    
program multMatrix
    implicit none
    
    real, dimension(:,:) :: matrix1, matrix2
    integer :: i, j
    integer :: rows1, columns1, rows2, columns2
    
    matrix1 = read_matrix("file1.txt")
    matrix2 = read_matrix("file2.txt")
    
    rows1 = size(matrix1, 1)
    columns1 = size(matrix1, 2)
    rows2 = size(matrix2, 1)
    columns2 = size(matrix2, 2)
    
    do i = 1, rows1
        do j = 1, columns1
            print *, matrix1(i, j)
        end do
    end do
    
    do i = 1, rows2
        do j = 1, columns2
            print *, matrix2(i, j)
        end do
    end do

end program multMatrix