
default:

clean:
	rm *.mod || echo 'no mod files found'
	rm -rf build/gfortran_*
