package org.kalypso.psiadapter;

import java.util.Properties;

import org.kalypso.java.reflect.ClassUtilities;
import org.kalypso.util.repository.RepositoryException;

import de.psi.go.lhwz.PSICompact;

/**
 * The entry point to the PSICompact interface from PSI.
 * 
 * @author schlienger
 */
public final class PSICompactFactory
{
  static String PSI_CLASS = null;

  protected static PSICompact psiCompact = null;

  protected static PSICompactRepository psiCompactRep = null;

  protected static int currentVersion = 0;

  private static Thread threadVersionChecker = null;

  private static Properties factoryProperties = null;

  /**
   * Returns the connection to the PSI-Interface implementation. This method is
   * only visible to the package.
   */
  static PSICompact getConnection()
  {
    if( psiCompact == null )
    {
      try
      {
        factoryProperties = new Properties();
        factoryProperties.load( PSICompactFactory.class.getResourceAsStream( "resources/config.ini" ) );

        // path of class which implements the PSICompact interface
        PSI_CLASS = factoryProperties.getProperty( "PSI_CLASS", "de.psi.go.lhwz.PSICompactImpl" );

        psiCompact = (PSICompact)ClassUtilities.newInstance( PSI_CLASS, PSICompact.class,
            PSICompactFactory.class.getClassLoader() );

        // Wichtig! init() aufrufen damit die PSI-Schnittstelle sich
        // initialisieren kann
        psiCompact.init();

        currentVersion = psiCompact.getDataModelVersion();
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }

    return psiCompact;
  }

  /**
   * Liefert den PSICompactRepository
   * @throws RepositoryException
   */
  static PSICompactRepository getRepository() throws RepositoryException
  {
    if( psiCompactRep == null )
    {
      // TODO: specify location of the service here
      psiCompactRep = new PSICompactRepository( "location" );

      threadVersionChecker = new Thread( new VersionChecker() );
      threadVersionChecker.start();
    }

    return psiCompactRep;
  }

  public static void dispose()
  {
    if( threadVersionChecker != null )
      threadVersionChecker.stop();
  }

  /**
   * Helper that translates the measure type into a string label.
   */
  public final static String measureTypeToString( int measType )
  {
    switch( measType )
    {
    case PSICompact.MEAS_FLOW:
      return factoryProperties.getProperty( "MEAS_FLOW" );
    case PSICompact.MEAS_LEVEL:
      return factoryProperties.getProperty( "MEAS_LEVEL" );
    case PSICompact.MEAS_RAINFALL:
      return factoryProperties.getProperty( "MEAS_RAINFALL" );
    case PSICompact.MEAS_TEMPERATUR:
      return factoryProperties.getProperty( "MEAS_TEMPERATUR" );
    case PSICompact.MEAS_UNDEF:
      return factoryProperties.getProperty( "MEAS_UNDEF" );
    default:
      return factoryProperties.getProperty( "UNKNOWN" );
    }
  }

  public final static String valueTypeToString( int valueType )
  {
    switch( valueType )
    {
    case PSICompact.TYPE_MEASUREMENT:
      return factoryProperties.getProperty( "TYPE_MEASUREMENT" );
    case PSICompact.TYPE_VALUE:
      return factoryProperties.getProperty( "TYPE_VALUE" );
    case PSICompact.TYPE_UNDEF:
      return factoryProperties.getProperty( "TYPE_UNDEF" );
    default:
      return factoryProperties.getProperty( "UNKNOWN" );
    }
  }

  /**
   * Internal version checker for the PSICompact Interface.
   * 
   * @author schlienger
   */
  private final static class VersionChecker implements Runnable
  {
    /**
     * @see java.lang.Runnable#run()
     */
    public void run()
    {
      try
      {
        Thread.sleep( 10 );

        int version = psiCompact.getDataModelVersion();

        if( version > currentVersion )
        {
          currentVersion = version;

          if( psiCompactRep != null )
            psiCompactRep.fireRepositoryStructureChanged();
        }
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
  }
}
