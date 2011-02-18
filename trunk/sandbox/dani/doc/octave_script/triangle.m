function retval = triangle(x, centre)
  retval = 0;

  if ( x < 0)
    retval = 0;
  endif

  if (x >= 0 && x <= centre)
    retval = x / centre;
  endif
  
  if (x >= centre)
    retval = - (x - 2*centre) / centre;
  endif

endfunction
