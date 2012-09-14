/*
 * -----------------------------------------------------------------
 * $Revision$
 * $Date$
 * ----------------------------------------------------------------- 
 * Programmers: Alan Hindmarsh and Radu Serban @ LLNL
 * -----------------------------------------------------------------
 * Copyright (c) 2002, The Regents of the University of California  
 * Produced at the Lawrence Livermore National Laboratory
 * All rights reserved
 * For details, see the LICENSE file
 * -----------------------------------------------------------------
 * This is the header file for the IDAS Scaled Preconditioned GMRES     
 * linear solver module, IDASPGMR.                                 
 *
 * Part I contains function prototypes for using IDASPGMR on forward 
 * problems (DAE integration and/or FSA)
 *
 * Part II contains function prototypes for using IDASPGMR on adjoint 
 * (backward) problems
 * -----------------------------------------------------------------
 */

#ifndef _IDASSPGMR_H
#define _IDASSPGMR_H

#ifdef __cplusplus     /* wrapper to enable C++ usage */
extern "C" {
#endif

#include <idas/idas_spils.h>
#include <sundials/sundials_spgmr.h>

/* 
 * -----------------------------------------------------------------
 * PART I - forward problems
 * -----------------------------------------------------------------
 */

/*
 * -----------------------------------------------------------------
 *                                                                
 * Function : IDASpgmr                                            
 * -----------------------------------------------------------------
 * A call to the IDASpgmr function links the main integrator with 
 * the IDASPGMR linear solver module.  Its parameters are as      
 * follows:                                                       
 *                                                                
 * IDA_mem   is the pointer to memory block returned by IDACreate.
 *                                                                
 * maxl      is the maximum Krylov subspace dimension, an         
 *           optional input.  Pass 0 to use the default value,    
 *           MIN(Neq, 5).  Otherwise pass a positive integer.     
 *                                                                
 * The return values of IDASpgmr are:                             
 *    IDASPILS_SUCCESS    if successful                            
 *    IDASPILS_MEM_NULL   if the IDAS memory was NULL
 *    IDASPILS_MEM_FAIL   if there was a memory allocation failure 
 *    IDASPILS_ILL_INPUT  if there was illegal input.              
 * The above constants are defined in idas_spils.h
 *                                                                
 * -----------------------------------------------------------------
 */                                                                

SUNDIALS_EXPORT int IDASpgmr(void *ida_mem, int maxl);


/* 
 * -----------------------------------------------------------------
 * PART II - backward problems
 * -----------------------------------------------------------------
 */

SUNDIALS_EXPORT int IDASpgmrB(void *ida_mem, int which, int maxlB);

#ifdef __cplusplus
}
#endif

#endif
