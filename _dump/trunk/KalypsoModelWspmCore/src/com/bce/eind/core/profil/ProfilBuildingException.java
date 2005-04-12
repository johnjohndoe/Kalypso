/*
 * Created on 28.02.2005
 */
package com.bce.eind.core.profil;

/**
 * @author kimwerner
  */
public class ProfilBuildingException extends Exception
{
  public ProfilBuildingException( final String msg )
  {
    super( msg );
  }

  public ProfilBuildingException( final String msg, final Throwable t )
  {
    super( msg, t );
  }
}
