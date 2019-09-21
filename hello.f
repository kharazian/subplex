        double precision function f (n,x)  
            integer n
            double precision x(n)
            integer i
            double precision sum
            sum = x(1)**2 + x(2)**2
            f = sum
            RETURN
            end
        
        program hello
            integer n,maxnfe,nfe,iwork(4),iflag
            double precision tol,scale(2),x(2),fx,work(17)
            external f
            n = 2
            tol = 0.00000000000001
            x = (/11 , -33/)
            scale = -1
            maxnfe = 10000
            call subplx(f,n,tol,maxnfe,scale,x,fx,nfe,work,iwork,iflag)
            print *, "Hello World!"
        end program hello