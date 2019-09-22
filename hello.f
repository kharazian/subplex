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
            integer n
            parameter( n = 18)
            integer maxnfe,nfe,iwork(2*n),iflag
            double precision tol,scale(n),x(n),fx,work(n*(n+6)+1)
            external f
                        
            tol = 0.00000000000001
            x = (/1,-1,2,-2,3,3,-1,-15,70,1,-1,2,-2,
     *   3,3,-1,-15,70/)
            scale = -1
            maxnfe = 10000
            call subplx(f,n,tol,maxnfe,scale,x,fx,nfe,work,iwork,iflag)
            print *, "Hello World!"
        end program hello