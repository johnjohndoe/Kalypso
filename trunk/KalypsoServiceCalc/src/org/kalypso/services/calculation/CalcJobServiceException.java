package org.kalypso.services.calculation;

import java.rmi.RemoteException;

/**
 * @author Belger
 */
public class CalcJobServiceException extends RemoteException
{
  public CalcJobServiceException( final String s, final Throwable ex )
  {
    super( s, ex );
  }
}