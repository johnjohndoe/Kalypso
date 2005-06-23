package org.kalypso.psiadapter;

import java.io.InputStream;
import java.util.Properties;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.lang.reflect.ClassUtilities;

import de.psi.go.lhwz.PSICompact;

/**
 * The entry point to the PSICompact interface from PSI.
 * 
 * @author schlienger
 */
public final class PSICompactFactory
{
  private final static String CONFIG = "/org/kalypso/psiadapter/resources/config.ini";

  private static String PSI_CLASS = null;

  protected static PSICompact m_psiCompact = null;

  private static Properties m_factoryProperties = null;

  /**
   * Returns the connection to the PSI-Interface implementation.
   * 
   * @return PSICompact instance
   */
  public final static PSICompact getConnection()
  {
    if( m_psiCompact == null )
    {
      InputStream stream = null;

      try
      {
        stream = PSICompactFactory.class.getResourceAsStream( CONFIG );

        if( stream == null )
          throw new IllegalStateException( "Could not load configuration file: " + CONFIG );

        m_factoryProperties = new Properties();
        m_factoryProperties.load( stream );

        // path of class which implements the PSICompact interface
        PSI_CLASS = m_factoryProperties.getProperty( "PSI_CLASS", "de.psi.go.lhwz.PSICompactImpl" );

        m_psiCompact = (PSICompact)ClassUtilities.newInstance( PSI_CLASS, PSICompact.class, PSICompactFactory.class
            .getClassLoader() );

        // Wichtig! init() aufrufen damit die PSI-Schnittstelle sich
        // initialisieren kann
        m_psiCompact.init();
      }
      catch( Exception e )
      {
        throw new IllegalStateException( "Error while creating PSICompact: " + e.toString() );
      }
      finally
      {
        IOUtils.closeQuietly( stream );
      }
    }

    return m_psiCompact;
  }

  /**
   * @return factory properties
   */
  public final static Properties getProperties()
  {
    if( m_factoryProperties == null )
      getConnection();

    return m_factoryProperties;
  }
}