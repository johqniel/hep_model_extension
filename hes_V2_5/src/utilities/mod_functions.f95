module mod_functions
    use mod_rnorm
    implicit none
    
    contains

    subroutine strt_distr_gaussian(n, xic, yic, sip, wkx, wky, siu, wku, wkv)
        integer, intent(in) :: n
        real(8), intent(in) :: xic, yic, sip, siu
        real(8), dimension(n), intent(out) :: wkx, wky, wku, wkv
        
        integer :: i
        
        do i = 1, n
            wkx(i) = rnorm_single(xic, sip)
            wky(i) = rnorm_single(yic, sip)
            wku(i) = rnorm_single(0.0d0, siu)
            wkv(i) = rnorm_single(0.0d0, siu)
        end do
        
    end subroutine strt_distr_gaussian

end module mod_functions
