program maxwell  ! Code developed by Rosana Gomes (September/2015)
! Last update 02.09.2015
implicit none
integer :: i, j, k1, k2, nH, nQ, qf, fm, icom
double precision, DIMENSION(:), allocatable :: rhobQ, eQ, pQ,munQ
double precision, DIMENSION(:), allocatable :: rhobH, eH, pH, munH
double precision :: a1, a2, a3, a4, a5, a6, a7, a8, trash
character(len=45) :: arg
allocate(rhobH(50000),eH(50000),pH(50000),munH(50000))
allocate(rhobQ(50000),eQ(50000),pQ(50000),munQ(50000))



!***************
!     FILES    !
!***************

! Get arguments for command line: 
! for icom=1, get filename (eos-hadrons)
! for icom=2, get filename (eos-quarks)
! for icom=3, get filename (eos-maxwell)

icom = 0
            DO
              CALL get_command_argument(icom, arg)
              IF (LEN_TRIM(arg) == 0) EXIT
             
		if(icom.eq.1) then
                ! reads argument as string and adds to the EOS (HADRON) file's name
		open(10, file=arg)

		WRITE (*,*) 'arg=', 'ARGUMENT 1, 10' !TRIM(arg), 'icom=', icom
                endif 

		if(icom.eq.2) then
                ! reads argument as string and adds to the EOS(QUARK) file's name
		open(20, file=arg)

		WRITE (*,*) 'arg=', 'ARGUMENT 2, 20'! , TRIM(arg), 'icom=', icom
                endif 


		if(icom.eq.3) then
                ! reads argument as string and adds to the EOS(MAXWELL) file's name
		open(35, file=arg)

		WRITE (*,*) 'arg=', 'ARGUMENT 3, 35'!, TRIM(arg), 'icom=', icom
                endif 

              icom = icom+1
            END DO

nH= 0
nQ = 0


! Reading quark phase data (converts eos to MeV/fm^3)
! Quarks: de, p + pmeson, rhob, mu_n, mu_e, a0, B14

     do 10 i = 1,2000000,1
        read(20,*,end=111) a1, a2, a3, a4, trash, trash, trash
        nQ = nQ + 1  
	eQ(i)   = a1*197.327d0       ! fm^-4- > MeV/fm^3
	pQ(i)   = a2*197.327d0       ! fm^-4- > MeV/fm^3
        rhobQ(i) = a3                 ! fm^-3
        munQ(i) = a4

 10   continue
111   continue 



! Reading hadronic phase data (converts from fm^-4 to MeV/fm^3)
!Hadrons: lambdaw,cte, a4, L0, de, p+pmeson, rhob, fs, mu_n, mu_e

     do 20 j = 1,2000000,1
        read(10,*,end=222) trash, trash, trash, trash, a5, a6, a7,trash, a8,trash
        nH = nH + 1  
	eH(j)   = a5*197.327d0       ! fm^-4- > MeV/fm^3
	pH(j)   = a6*197.327d0       ! fm^-4- > MeV/fm^3
        rhobH(j) = a7                ! fm^-3
        munH(j) = a8
 20   continue
222   continue 

 !     write(*,*) 'Maxwell'

! Maxwell transition: munH = munQ, pH = pQ (pressure always grows)

k1 = 1 ! line index in hadronic file
k2 = 1 ! line index in quark file


do while(k1<nH) ! nH = total number of lines in hadrons file

qf = 0
k2 = 1

    do while(k2<nQ) !nQ = total number of lines in hadrons file

!write(3,*) 'QUARKS', k2, munH(k1), munQ(k2), abs(pH(k1)-pQ(k2))
      if ((abs(munH(k1)-munQ(k2)).lt.0.1d0).and.(pH(k1).lt.pQ(k2))) then

     !       write(*,*) 'Quark Phase', munQ(k2),eQ(k2)- eH(k1) 
            do while(k2<nQ)
             qf = 1
            write(35,*) eQ(k2), pQ(k2),rhobQ(k2), munQ(k2), qf, eQ(k2)- eH(k1) 
             
             k2 = k2 + 1

            enddo
             k1 = nH + 1000
      endif 
      
      k2 = k2 + 1 
    enddo ! end of quarks loop

     if(qf.ne.1) then
  !    write(*,*) 'hadronic Phase', munH(k1)
     write(35,*) eH(k1), pH(k1),rhobH(k1), munH(k1), qf, '0.d0'
     endif

      k1 = k1 + 1
enddo !end of hadrons loop

     if(qf.ne.1) then
     write(*,*) 'No Phase Transition'
     endif

 close(10)
 close(20)
 close(35)

end program

