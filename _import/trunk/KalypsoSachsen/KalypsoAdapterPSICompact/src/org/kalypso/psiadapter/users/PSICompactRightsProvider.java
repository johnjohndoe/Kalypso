package org.kalypso.psiadapter.users;

import org.kalypso.psiadapter.PSICompactFactory;
import org.kalypso.users.IUserRightsProvider;
import org.kalypso.users.UserRightsException;

import de.psi.go.lhwz.ECommException;
import de.psi.go.lhwz.PSICompact;

/**
 * PSICompactRightsProvider, it simply forwards the rights that it gets from
 * PSICompact.
 * 
 * @author schlienger
 */
public class PSICompactRightsProvider implements IUserRightsProvider
{
  private final PSICompact m_psicompact;

  public PSICompactRightsProvider( )
  {
    m_psicompact = PSICompactFactory.getConnection();

    if( m_psicompact == null )
      throw new IllegalStateException(
          "PSI-Compact Schnittstelle konnte nicht initialisiert werden" );
  }

  /**
   * @see org.kalypso.users.IUserRightsProvider#getRights(java.lang.String)
   */
  public String[] getRights( final String username ) throws UserRightsException
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

      throw new UserRightsException( "Fehler beim Abfragen der Benutzerrechte",
          e );
    }
  }
}