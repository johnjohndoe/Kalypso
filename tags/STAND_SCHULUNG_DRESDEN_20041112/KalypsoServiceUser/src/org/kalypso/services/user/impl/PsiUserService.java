package org.kalypso.services.user.impl;

import java.rmi.RemoteException;
import java.util.logging.FileHandler;
import java.util.logging.Logger;

import org.kalypso.psiadapter.PSICompactFactory;
import org.kalypso.services.common.ServiceConfig;
import org.kalypso.services.user.IUserService;

import de.psi.go.lhwz.ECommException;
import de.psi.go.lhwz.PSICompact;

/**
 * @author belger
 */
public class PsiUserService implements IUserService
{
  private final PSICompact m_psicompact;

  private final Logger m_logger = Logger.getLogger( PsiUserService.class.getName() );

  public PsiUserService()
  {
    try
    {
      m_logger.addHandler( new FileHandler( ServiceConfig.getTempDir() + "/IUserService%g.log",
          10000000, 10, true ) );
    }
    catch( Exception e ) // generic Exception caught for simplicity
    {
      e.printStackTrace();
      System.out.println( "Logger für User-Service konnte nicht erzeugt werden" );
    }

    m_logger.info( "Initialisiere PSI-Compact Schnittstelle" );

    m_psicompact = PSICompactFactory.getConnection();

    if( m_psicompact == null )
      m_logger.info( "PSI-Compact Schnittstelle konnte nicht initialisiert werden" );
    else
      m_logger.info( "PSI-Compact Schnittstelle erfolgreich initialisiert" );
  }

  /**
   * @see org.kalypso.services.user.IUserService#getRights(java.lang.String)
   */
  public String[] getRights( final String username ) throws RemoteException
  {
    if( m_psicompact == null )
      return null;
    
    try
    {
      return m_psicompact.getUserClasses( username );
    }
    catch( final ECommException e )
    {
      e.printStackTrace();

      throw new RemoteException( "Fehler beim Abfragen der Benutzerrechte", e );
    }
  }

  /**
   * @see org.kalypso.services.IKalypsoService#getServiceVersion()
   */
  public int getServiceVersion()
  {
    return 0;
  }

}