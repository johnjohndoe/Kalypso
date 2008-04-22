package org.kalypso.psiadapter.users;

import java.util.ArrayList;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.hwv.services.user.UserServiceSimulation;
import org.kalypso.psiadapter.PSICompactFactory;
import org.kalypso.simulation.core.SimulationException;

/**
 * PSICompactRightsProvider, it simply forwards the rights that it gets from PSICompact.
 * 
 * @author schlienger
 */
public class PSICompactRightsProvider extends UserServiceSimulation implements IExecutableExtension
{
  /**
   * @see org.eclipse.core.runtime.IExecutableExtension#setInitializationData(org.eclipse.core.runtime.IConfigurationElement,
   *      java.lang.String, java.lang.Object)
   */
  public void setInitializationData( final IConfigurationElement config, final String propertyName, final Object data ) throws CoreException
  {
    if( PSICompactFactory.getConnection() == null )
    {
      final IStatus errorStatus = StatusUtilities.createErrorStatus( "PSI-Compact Schnittstelle konnte nicht initialisiert werden" );
      throw new CoreException( errorStatus );
    }
  }

  /**
   * @see org.kalypso.hwv.services.user.UserServiceSimulation#getRights(java.lang.String, java.lang.String,
   *      java.lang.String)
   */
  @Override
  protected String[] getRights( final String username, final String password, final String scenarioId ) throws SimulationException
  {
    try
    {
      final ArrayList<String> rights = new ArrayList<String>();
      final String[] userClasses = PSICompactFactory.getConnection().getUserClasses( username.toLowerCase() );

      // handle "null", remove whitespaces, convert right for kalypso
      for( final String element : userClasses )
      {
        if( element != null )
          rights.add( PSICompactFactory.toKalypsoRight( element.trim() ) );
      }

      // CONVENTION: always add the Vorhersage right
      rights.add( PSICompactFactory.toKalypsoRight( "" ) );

      return rights.toArray( new String[rights.size()] );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new SimulationException( "Fehler beim Abfragen der Benutzerrechte", e );
    }
  }
}