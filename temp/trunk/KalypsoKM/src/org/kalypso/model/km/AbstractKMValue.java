package org.kalypso.model.km;

import java.util.Formatter;

public abstract class AbstractKMValue
{
  public abstract double getLength( );

  public abstract double getAlpha( );

  public abstract double getK( );

  public abstract double getN( );

  public abstract double getKForeland( );

  public abstract double getNForeland( );

  public abstract double getQ( );

  public abstract double getQForeland( );

  public double getQSum( )
  {
    return getQ() + getQForeland();
  }

  public String toString( )
  {
    return "\n Qges:        " + new Formatter().format( "%8.3f", getQSum() ) + "\t Laenge:       " + new Formatter().format( "%7.2f", getLength() ) + "\n Q(Fluss):    " + new Formatter().format( "%8.3f", getQ() )
        + "\t k(Fluss):      " + new Formatter().format( "%8.4f", getK() ) + "\t    n(Fluss):    " + new Formatter().format( "%7.2f", getN() ) + "\n Q(Vorland): " + new Formatter().format( "%8.3f", getQForeland() )
        + "\t k (Vorland):  " + new Formatter().format( "%8.4f", getKForeland() ) + "\t n(Vorland): " + new Formatter().format( "%7.2f", getNForeland() );
  }
}
