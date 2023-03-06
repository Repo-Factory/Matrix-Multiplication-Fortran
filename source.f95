! @author Conner Sommerfield
! Program for CS420 class that multiplies two matrices in Fortran
! Reads from text files matrix1.txt and matrix2.txt to populate matrices
! User must give dimensions of matrices to multiply

! module defines helper functions for working with matrices
module matrices
    implicit none
    contains
    
    ! Before populating array, we need to get its dimension by prompting user input
    subroutine prompt_dimensions(rows, columns)
        integer, intent(out)        :: rows, columns

        print *, "Please type number of rows"
        read *, rows
        print *, "Please type number of columns"
        read *, columns
    end subroutine prompt_dimensions

    ! Pass in an allocated array and name of the text file to populate numbers from
    ! Use this along with dimensions given from prompt_dimensions to fill matrix
    subroutine populate_matrix(filename, matrix, rows, columns)
        character(*), intent(in)    :: filename
        integer, intent(in)         :: rows, columns
        real, intent(out)           :: matrix(:,:)
        integer                     :: i, j                     ! Loop variables
        
        ! File handle chosen at random
        open(10, file=filename)
        do i=1, rows
            do j=1, columns
                read(10, *) matrix(i,j)     ! next array element set to next number in file
            end do
        end do
        close(10)
    end subroutine populate_matrix

    ! Uses formatted write statement to write an array in matrix format
    subroutine print_matrix(rows, columns, matrix)
        integer, intent(in)         :: rows, columns
        real, intent(in)            :: matrix(:,:)
        integer                     :: i, j

        do i=1, rows
            do j=1, columns
                if (j == columns) then          ! When reaching the last column, print with new line
                    write(*, fmt="(1x,f7.2)", advance="yes") matrix(i,j)
                else
                    write(*, fmt="(1x,f7.2)", advance="no") matrix(i,j)
                endif
            end do
        end do

        ! Extra white space
        print *, ""
        print *, ""
    end subroutine print_matrix


    ! Uses matrix multiplication algorithm to populate output matrix array
    subroutine multMatrices(outMatrix, matrix1, matrix2, rows1, rows2, columns1, columns2)
        integer, intent(in)         :: rows1, rows2, columns1, columns2
        real, intent(in)            :: matrix1(:,:), matrix2(:,:)
        real, intent(out)           :: outMatrix(:,:)
        integer                     :: i, j, k
      
        do i=1, rows1
            do j=1, columns2
                do k=1, rows2
                    outMatrix(i, j) = outMatrix(i, j) + (matrix1(i, k) * matrix2(k, j))
                end do
            end do
        end do

    end subroutine multMatrices

end module matrices


! Populate two matrices from two text files, the user is prompted to give the dimensions of the matrices
! In the text files and the program will print the matrices.
! After validation the matrices can be multiplied, the calculation is performed and displayed to user

program multMatrix
    use matrices
    implicit none

                                          !!!! POPULATE MATRICES !!!!

    ! Declarations !
    real, allocatable :: matrix1(:,:), matrix2(:,:), outMatrix(:,:)
    integer :: rows1, columns1, rows2, columns2

    
    ! Matrix 1 !
74  call prompt_dimensions(rows1, columns1)                         ! Get dimensions to allocate array
    allocate(matrix1(rows1, columns1))
    call populate_matrix("matrix1.txt", matrix1, rows1, columns1)   ! Fill array from matrix1.txt
    call print_matrix(rows1, columns1, matrix1)                     ! Print array in matrix format

    ! Matrix 2 !
    call prompt_dimensions(rows2, columns2)                           
    allocate(matrix2(rows2, columns2))
    call populate_matrix("matrix2.txt", matrix2, rows2, columns2)
    call print_matrix(rows2, columns2, matrix2)

                                        
                                        !!!! PERFORM MULTIPLICATION !!!!

    ! Dimensions Check !
    if (rows2 .ne. columns1) then
        print *, "Invalid Matrix Dimension, Cannot Multiply"
        deallocate(matrix1)
        deallocate(matrix2)
        goto 74                                                     ! Prompt user again for useable dimensions
    endif

    ! Allocate Output Matrix, Multiply And Display Result !
    allocate(outMatrix(rows1, columns2))
    outMatrix = 0.0                                                 ! Set all matrix elements to zero
    call multMatrices(outMatrix, matrix1, matrix2, rows1, rows2, columns1, columns2)
    call print_matrix(rows1, columns2, outMatrix)

    ! Cleanup !
    deallocate(matrix1)
    deallocate(matrix2)
    deallocate(outMatrix)

end program multMatrix