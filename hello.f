        double precision function f (n,x)  
            integer n
            double precision x(n)
            integer i
            double precision sum
            sum = 0
            do 10 i = 1,n
            sum = sum + x(i)**2
10      continue
            f = sum
            RETURN
            end
        
        program hello
            integer n, maxnfe,nfe,iwork(2*2),iflag
            double precision tol,scale(2),x(2),fx,work(17)
            external f
            
            n = 2
            ! call alloca_fortran(iwork, n, TYPE_INT) ! Allocate 20 integers for a

            ! call alloca_fortran(b, 14, TYPE_DBL) ! Allocate 14 doubles for b

            tol = 0.00000000000001
            x = (/11 , -33/)
            scale = -1
            maxnfe = 10000
            call subplx(f,n,tol,maxnfe,scale,x,fx,nfe,work,iwork,iflag)
            print *, "Hello World!"
        end program hello