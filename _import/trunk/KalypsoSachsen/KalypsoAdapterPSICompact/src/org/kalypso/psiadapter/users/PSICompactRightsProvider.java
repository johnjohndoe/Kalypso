package org.kalypso.psiadapter.users;

import java.util.ArrayList;
import java.util.Properties;

import org.kalypso.psiadapter.PSICompactFactory;
import org.kalypso.services.user.IUserRightsProvider;
import org.kalypso.services.user.UserRightsException;

import de.psi.go.lhwz.ECommException;
import de.psi.go.lhwz.PSICompact;

/**
 * PSICompactRightsProvider, it simply forwards the rights that it gets from PSICompact.
 * 
 * @author schlienger
 */
public class PSICompactRightsProvider implements IUserRightsProvider
{
  private final PSICompact m_psicompact;

  public PSICompactRightsProvider()
  {
    m_psicompact = PSICompactFactory.getConnection();

    if( m_psicompact == null )
      throw new IllegalStateException( "PSI-Compact Schnittstelle konnte nicht initialisiert werden" );
  }

  /**
   * @see org.kalypso.services.user.IUserRightsProvider#init(java.util.Properties)
   */
  public void init( Properties props ) throws UserRightsException
  {
  // empty, not interesting for us
  }

  /**
   * @see org.kalypso.services.user.IUserRightsProvider#dispose()
   */
  public void dispose()
  {
  // nothing to do
  }

  /**
   * @see org.kalypso.services.user.IUserRightsProvider#getRights(java.lang.String)
   */
  public String[] getRights( final String username ) throws UserRightsException
  {
    if( m_psicompact == null )
      return null;

    try
    {
      final ArrayList rights = new ArrayList();
      final String[] userClasses = m_psicompact.getUserClasses( username.toLowerCase() );

      // handle "null", remove whitespaces, convert right for kalypso
      for( int i = 0; i < userClasses.length; i++ )
      {
        if( userClasses[i] != null )
          rights.add( PSICompactFactory.toKalypsoRight( userClasses[i].trim() ) );
      }
      
      // CONVENTION: always add the Vorhersage right
      rights.add( PSICompactFactory.toKalypsoRight( "" ) );

      return (String[])rights.toArray( new String[rights.size()] );
    }
    catch( final ECommException e )
    {
      e.printStackTrace();

      throw new UserRightsException( "Fehler beim Abfragen der Benutzerrechte", e );
    }
  }

  /**
   * @see org.kalypso.services.user.IUserRightsProvider#getRights(java.lang.String, java.lang.String)
   */
  public String[] getRights( String username, String password ) throws UserRightsException
  {
    // in Sachsen hat diese Methode keinen Sinn. PSICompact soll nur mit dem
    // am System angemeldete Benutzer gefragt werden.
    // Theoretisch könnten wir hier den Aufruf an getRights( username )
    // weiterleiten, aber Sicherheitshalber machen wir das nicht
    // sonst wäre es möglich sich unerlaubt einem anderen Benutzer
    // die Rechte zu klauen.

    return null;
  }
}