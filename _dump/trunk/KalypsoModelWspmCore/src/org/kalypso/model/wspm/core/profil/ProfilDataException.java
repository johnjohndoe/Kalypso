/*
 * Created on 28.02.2005
 */
package org.kalypso.model.wspm.core.profil;

/**
 * @author kimwerner
  */
public class ProfilDataException extends Exception
{
  public ProfilDataException( final String msg )
  {
    super( msg );
  }

  public ProfilDataException( final String msg, final Throwable t )
  {
    super( msg, t );
  }
}
