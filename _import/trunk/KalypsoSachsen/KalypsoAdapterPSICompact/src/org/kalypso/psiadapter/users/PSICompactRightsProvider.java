package org.kalypso.psiadapter.users;

import java.util.ArrayList;

import org.kalypso.psiadapter.PSICompactFactory;
import org.kalypso.users.IUserRightsProvider;
import org.kalypso.users.UserRightsException;
import org.kalypso.users.UserServiceConstants;

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
      final ArrayList rights = new ArrayList();
      final String[] userClasses = m_psicompact.getUserClasses( username.toLowerCase() );
      
      // leere Rechte rauschmeissen
      for( int i = 0; i < userClasses.length; i++ )
      {
        String right = userClasses[i];
        if( right != null )
        {
          right = right.trim();

          if( right.length() != 0
              && UserServiceConstants.isValidUserRight( right ) )
            rights.add( right.trim() );
        }
      }
      
      return (String[]) rights.toArray( new String[rights.size()] );
    }
    catch( final ECommException e )
    {
      e.printStackTrace();

      throw new UserRightsException( "Fehler beim Abfragen der Benutzerrechte",
          e );
    }
  }

  /**
   * @see org.kalypso.users.IUserRightsProvider#getRights(java.lang.String, java.lang.String)
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