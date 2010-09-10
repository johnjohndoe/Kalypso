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
    return "\n Qges:        " + new Formatter().format( "%8.3f", getQSum() ) + "\t Laenge:       " + new Formatter().format( "%7.2f", getLength() ) + "\n Q(Fluss):    " + new Formatter().format( "%8.3f", getQ() ) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
    + "\t k(Fluss):      " + new Formatter().format( "%8.4f", getK() ) + "\t    n(Fluss):    " + new Formatter().format( "%7.2f", getN() ) + "\n Q(Vorland): " + new Formatter().format( "%8.3f", getQForeland() ) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
    + "\t k (Vorland):  " + new Formatter().format( "%8.4f", getKForeland() ) + "\t n(Vorland): " + new Formatter().format( "%7.2f", getNForeland() ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
  }
}
