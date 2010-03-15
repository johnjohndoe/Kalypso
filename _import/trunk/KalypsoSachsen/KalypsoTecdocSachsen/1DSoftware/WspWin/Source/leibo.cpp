/*--------------------------------------------------------------------

  Modul: Massenberechnung
  02.07.1996  Dr.Leibo
  --------------------------------------------------------------------
  In der Struktur << LAENGSPROFIL >> sind
  
  Abtragsflächen  mit ( - ) und  Auftragsflächen mit ( + ) gespeichert.
    
  summe ( Auftrag - Abtrag)      ist  mit Vorzeichen gespeichert.
     
  sum_auftrag                    ist ohne Vorzeichen gespeichert.
        
  sum_abtrag                     ist ohne Vorzeichen gespeichert.
          
            
------------------------------    ---------------------------------------
|                              |  |                                       |
| typedef struct GELAENDE      |  |    typedef struct LAENGSPROFIL        |
| {                            |  |      { double station;                |
|  short index;                |  |        struct GELAENDE *querprof1,    |
|  double y;                   |  |                        *querprof2,    |
|  double z;                   |  |                        *flaeche;      |
|  struct GELAENDE *next_ds;   |  |        double           summe,        |
|  struct GELAENDE *pre_ds;    |  |                         sum_auftrag,  |
| }                            |  |                         sum_abtrag;   |
|  GELAENDE;                   |  |      }                                |
|                              |  |        LAENGSPROFIL;                  |
|                              |  |                                       |
------------------------------    ---------------------------------------
              
  Achtung : Suchaktion ist im Programm <<LAENGSPROFIL.mc>> beschrieben :
                
  ---> int FlaechenImLAENGSPROFILSuchen(LAENGSPROFIL *LP),
                  
  und zwar --> [[ if(Fl->next_ds==NULL) break; ]]
                    
*/


#include <windows.h>
#include <math.h>
#include "xvt.h"

#include "global_types.h"

#include "leibo.h"

/******************************************************************************/
int FlaechenImLaengsprofilBerechnen(LAENGSPROFIL *LP)
{ /* Rückgabe:  True = alles OK / False= Fehlerfall
  */
  
  GELAENDE *P1,*P2,*Fl;
  int m1,
    m2, 
    in,p1[2],p2[2],I,i,j;
  double *y1,*z1,*y2,*z2,Y[2],Z[2],F,F1,F2,F12,TOL=1.0e-05,AU=0.,AB=0.;
  
  P1 = LP->querprof1;
  P2 = LP->querprof2;
  if( !P1 || !P2 )
    return 0;
  
  y1=(double*)malloc(sizeof(double));
  z1=(double*)malloc(sizeof(double));
  y2=(double*)malloc(sizeof(double));
  z2=(double*)malloc(sizeof(double));
  
  m1 = 0;
  while( TRUE )
  {
    y1[m1] = P1->y;
    z1[m1] = P1->z;
    m1++;
    y1 = (double*)realloc( y1,sizeof(double)*(m1+1) );
    z1 = (double*)realloc( z1,sizeof(double)*(m1+1) );
    if( P1->next_ds == NULL )
      break;
    P1 = P1->next_ds;
  }
  
  m2 = 0;
  while( TRUE )
  {
    y2[m2] = P2->y;
    z2[m2] = P2->z;
    m2++;
    y2=(double*)realloc(y2,sizeof(double)*(m2+1));
    z2=(double*)realloc(z2,sizeof(double)*(m2+1));
    if(P2->next_ds == NULL)
      break;
    P2=P2->next_ds;
  }
  
  F12=0.;
  LP->flaeche=Fl=(GELAENDE*)malloc(sizeof(GELAENDE));
  I=0;
  
  Y[I  ]=y1[0   ];
  Z[I  ]=z1[0   ];
  p1[I  ]=0;
  p2[I  ]=0;
  I++;
  Y[I%2]=y1[m1-1];
  Z[I%2]=z1[m1-1];
  p1[I%2]=m1-2;
  p2[I%2]=m2-2;
  
  
  for(i=0; i<m1-1; i++) for(j=0; j<m2-1; j++)
	 {
    in=SchnittPunktzweierStrecken(TOL,y1[i],z1[i],y1[i+1],z1[i+1],
      y2[j],z2[j],y2[j+1],z2[j+1],&Y[I%2],&Z[I%2]);
    p1[I%2]=i; p2[I%2]=j;
    
    if(in == 10000 || in == 10110)
    {
      RiemannschesIntegral(I,p1,y1,z1,Y,Z,&F1);
      RiemannschesIntegral(I,p2,y2,z2,Y,Z,&F2);
      F=F1-F2;
      F12=F12+F;
      I++;
      if(F<=0.)
        AU=AU-F;
      else
        AB=AB+F;
      Fl->y = Y[(I)%2]; //		 Fl->y = Y[(I-1)%2];
      Fl->z = F;
      
      Fl->next_ds=(GELAENDE*)malloc(sizeof(GELAENDE));
      Fl=Fl->next_ds;
    }
	 }
  RiemannschesIntegral(I,p1,y1,z1,Y,Z,&F1);
  RiemannschesIntegral(I,p2,y2,z2,Y,Z,&F2);
  F=F1-F2;
  F12=F12+F;
  I++;
  if(F<=0.)
		  AU=AU-F;
  else
		  AB=AB+F;
  Fl->y = Y[(I)%2]; //	  Fl->y = Y[(I-1)%2];
  Fl->z = F;
  
  Fl->next_ds=(GELAENDE*)malloc(sizeof(GELAENDE));
  Fl->next_ds=NULL;
  
  LP->summe = AB-AU;
  LP->sum_auftrag = AB;
  LP->sum_abtrag = AU;
  
  free(y1); free(z1); free(y2); free(z2);
  return(TRUE);
}
/******************************************************************************/
void RiemannschesIntegral(int I,int *p,double *x,double *y,double *X,double *Y,double *F)
{
  int i,i1,i2;
  *F=0.;
  i1=p[(I-1)%2];
  i2=p[(I-0)%2];
  if(i1 == i2)
  {
    *F=(X[(I-0)%2]-X[(I-1)%2])*(Y[(I-0)%2]+Y[(I-1)%2]);
    *F = *F/2.;
    return;
  }
  else
  {
    *F=*F+(x[i1+1]-X[(I-1)%2])*(y[i1+1]+Y[(I-1)%2]);
    for(i=i1+1; i<i2; i++)
      *F=*F+(x[i+1]-x[i])*(y[i+1]+y[i]);
    *F=*F+(X[(I-0)%2]-x[i2])*(Y[(I-0)%2]+y[i2]);
    *F=*F/2.;
    return;
  }
}
/******************************************************************************/
int SchnittPunktzweierStrecken(double TOL,double x1,double y1,double x2,double y2,
                               double x3,double y3,double x4,double y4,double *X,double *Y)
{
  double s21,s43,un,ob,r,s10,s20,s30,s40;
  int in=0;
  
  if((s21=sqrt(pow(x2-x1,2.)+pow(y2-y1,2.)))<TOL)
  {
    //	 printf("\nIN = -1  Darf nie sein\n");
    return(in-1 );
  }
  if((s43=sqrt(pow(x4-x3,2.)+pow(y4-y3,2.)))<TOL)
		{
    //		 printf("\nIN = -10 Darf nie sein\n");
    return(in-10);
		}
  
  if(fabs(un=(x4-x3)*(y1-y2)-(y4-y3)*(x1-x2))<TOL)
    return(in);
  
  ob=(x1-x3)*(y1-y2)-(y1-y3)*(x1-x2);
  
  r=ob/un;
  *X=x3+r*(x4-x3);
  *Y=y3+r*(y4-y3);
  
  s10=sqrt(pow(x1-*X,2.)+pow(y1-*Y,2.));
  s20=sqrt(pow(x2-*X,2.)+pow(y2-*Y,2.));
  s30=sqrt(pow(x3-*X,2.)+pow(y3-*Y,2.));
  s40=sqrt(pow(x4-*X,2.)+pow(y4-*Y,2.));
  
  if((s10)<TOL)  { *X=x1; *Y=y1; in=in+1   ; s10=0.0; }
  if((s20)<TOL)  { *X=x2; *Y=y2; in=in+10  ; s20=0.0; }
  if((s30)<TOL)  { *X=x3; *Y=y3; in=in+100 ; s30=0.0; }
  if((s40)<TOL)  { *X=x4; *Y=y4; in=in+1000; s40=0.0; }
  
  if( (s10<s21 || fabs(s10-s21)<TOL) && (s20<s21 || fabs(s20-s21)<TOL) &&
    (s30<s43 || fabs(s30-s43)<TOL) && (s40<s43 || fabs(s40-s43)<TOL) )
    in=in+10000;
  
  return(in);
}
