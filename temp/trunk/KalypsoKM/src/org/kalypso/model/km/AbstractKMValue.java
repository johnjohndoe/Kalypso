package org.kalypso.model.km;

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
    return "\n Qges:" + getQSum() + "\t Laenge:" + getLength() + "\n Q(Fluss):" + getQ() + "\t k(Fluss):" + getK() + "\t n(Fluss):" + getN() + "\n Q(Vorland):" + getQForeland() + "\t k (Vorland):"
        + getKForeland() + "\t n(Vorland):" + getNForeland();
  }
}
