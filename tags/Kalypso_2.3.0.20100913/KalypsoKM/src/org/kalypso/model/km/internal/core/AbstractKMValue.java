package org.kalypso.model.km.internal.core;

import java.util.Formatter;

abstract class AbstractKMValue implements IKMValue
{
  @Override
  public double getQSum( )
  {
    return getQ() + getQForeland();
  }

  @Override
  public final String toString( )
  {
    final Formatter formatter = new Formatter();
    formatter.format( "\n Qges:        %8.3f", getQSum() ); //$NON-NLS-1$
    formatter.format( "\t Laenge:      %8.2f", getLength() ); //$NON-NLS-1$
    formatter.format( "\n Q(Fluss):    %8.3f", getQ() ); //$NON-NLS-1$
    formatter.format( "\t k(Fluss):    %8.4f", getK() ); //$NON-NLS-1$
    formatter.format( "\t n(Fluss):    %8.2f", getN() ); //$NON-NLS-1$
    formatter.format( "\n Q(Vorland):  %8.3f", getQForeland() ); //$NON-NLS-1$
    formatter.format( "\t k (Vorland): %8.4f", getKForeland() ); //$NON-NLS-1$
    formatter.format( "\t n(Vorland):  %8.2f", getNForeland() ); //$NON-NLS-1$

    return formatter.toString();
  }
}
