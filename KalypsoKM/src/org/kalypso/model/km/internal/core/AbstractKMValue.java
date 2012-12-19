package org.kalypso.model.km.internal.core;

import java.util.Formatter;

abstract class AbstractKMValue implements IKMValue
{
  @Override
  public final double getLowerQ( )
  {
    return getLowerQchannel() + getLowerQforeland();
  }

  @Override
  public final double getUpperQ( )
  {
    return getUpperQchannel() + getUpperQforeland();
  }

  @Override
  public final String toString( )
  {
    try( final Formatter formatter = new Formatter() )
    {
      formatter.format( "\n Q (Gesamt):  %8.3f", getLowerQ() ); //$NON-NLS-1$
      formatter.format( "\t k (Fluss):   %8.4f", getK() ); //$NON-NLS-1$
      formatter.format( "\t n (Fluss):   %8.2f", getN() ); //$NON-NLS-1$
      formatter.format( "\n Q (Vorland): %8.3f", getLowerQforeland() ); //$NON-NLS-1$
      formatter.format( "\t k (Vorland): %8.4f", getKForeland() ); //$NON-NLS-1$
      formatter.format( "\t n(Vorland):  %8.2f", getNForeland() ); //$NON-NLS-1$
      return formatter.toString();
    }
  }
}
