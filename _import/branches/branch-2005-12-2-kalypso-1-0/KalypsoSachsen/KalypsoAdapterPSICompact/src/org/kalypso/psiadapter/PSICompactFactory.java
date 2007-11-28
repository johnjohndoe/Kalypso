package org.kalypso.psiadapter;

import java.io.InputStream;
import java.util.Calendar;
import java.util.Properties;
import java.util.TimeZone;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.util.CalendarUtilities;

import de.psi.go.lhwz.ECommException;
import de.psi.go.lhwz.PSICompact;
import de.psi.go.lhwz.PSICompactImpl;

/**
 * The entry point to the PSICompact interface from PSI.
 * 
 * @author schlienger
 */
public final class PSICompactFactory
{
  /**
   * System property which contains the location of the fake data. Must be an url. Normaly this points to the directory
   * containing the 'structure.txt' file.<br/>
   * ATTENTION: If this property is set, the fake implementation is used instead of the real PSICompact implementation!
   */
  private static final String SYSPROP_FAKE_LOCATION = "kalypso.psifake.location";

  private final static String CONFIG = "/org/kalypso/psiadapter/resources/config.ini";

  protected static PSICompact m_psiCompact = null;

  private static Properties m_factoryProperties = null;

  private static Calendar m_psiCalendar = null;

  /**
   * Kennzeichen der Zeitzone in welche PSICompact operiert
   */
  private static final String TIMEZONE_ID = "TIMEZONE_ID";

  /**
   * der Kalender-Field im Sinne von java.util.Calendar worauf die AMOUNT_BEFORE und AMOUNT_AFTER sich beziehen.
   */
  private static final String OVERWRITE_CALENDAR_FIELD = "OVERWRITE_CALENDAR_FIELD";

  /**
   * Anzahl an Zeit-Einheiten die vor der Begin einer Zeitreihe mit OVERWRITE_VALUE überschrieben werden, bevor die
   * Zeitreihe in PSICompact zurückgeschrieben wird
   */
  private static final String OVERWRITE_AMOUNT_BEFORE = "OVERWRITE_AMOUNT_BEFORE";

  /**
   * Anzahl an Zeit-Einheiten die nach der Ende einer Zeitreihe mit OVERWRITE_VALUE überschrieben werden, bevor die
   * Zeitreihe in PSICompact zurückgeschrieben wird
   */
  private static final String OVERWRITE_AMOUNT_AFTER = "OVERWRITE_AMOUNT_AFTER";

  /**
   * Der Step, mit welchem die Vorhersage rausgeschrieben wird Die Einheit ist OVERWRITE_CALENDAR_FIELD
   */
  private static final String OVERWRITE_STEP = "OVERWRITE_STEP";

  /**
   * Überschreibungswert um die Zeitreihe so zu markieren, dass keine Darstellung der entsprechende Werte bei
   * Betrachtung im Web (Informationsmanagementsystem) erfolgt
   */
  private static final String OVERWRITE_VALUE = "OVERWRITE_VALUE";

  /**
   * Returns the connection to the PSI-Interface implementation.
   * 
   * @return PSICompact instance
   */
  public final static PSICompact getConnection()
  {
    if( m_psiCompact != null )
    {
      try
      {
        m_psiCompact.getDataModelVersion();
      }
      catch( final ECommException e )
      {
        m_psiCompact = null;

        // es geht im nächsten if-Block weiter
        // damit wird es neu initialisiert (vielleicht wurde psicompact inzwischen neu gestartet)
      }
    }

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

        final String fakeLocation = System.getProperty( SYSPROP_FAKE_LOCATION, null );
        if( fakeLocation == null )
        {
          System.out.println( "Fake location not set, using real PSICompact-implementation. Use the following System-property to use a fake implementation instead: " + SYSPROP_FAKE_LOCATION );
          m_psiCompact = new PSICompactImpl();
        }
        else
        {
          System.out.println( "Fake location set. Using PSI-fake implementation on location: " + fakeLocation );
          m_psiCompact = new PSICompactFakeImpl( fakeLocation );
        }

        // Wichtig! init() aufrufen damit die PSI-Schnittstelle sich initialisieren kann
        m_psiCompact.init();
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        
        throw new IllegalStateException( "Error while creating PSICompact: " + e.toString() );
      }
      finally
      {
        IOUtils.closeQuietly( stream );
      }
    }

    return m_psiCompact;
  }

  public final static Properties getProperties()
  {
    if( m_factoryProperties == null )
      getConnection();

    return m_factoryProperties;
  }

  public final static Calendar getCalendarForPSICompact()
  {
    if( m_psiCalendar == null )
      m_psiCalendar = Calendar.getInstance( TimeZone.getTimeZone( getProperties().getProperty( TIMEZONE_ID ) ) );

    return m_psiCalendar;
  }

  public final static String toKalypsoRight( final String psiRight )
  {
    return getProperties().getProperty( "RIGHT_" + psiRight );
  }

  public final static double getOverwriteValue()
  {
    return Double.parseDouble( getProperties().getProperty( OVERWRITE_VALUE ) );
  }

  public final static int getOverwriteAmountBefore()
  {
    return Integer.parseInt( getProperties().getProperty( OVERWRITE_AMOUNT_BEFORE ) );
  }

  public final static int getOverwriteAmountAfter()
  {
    return Integer.parseInt( getProperties().getProperty( OVERWRITE_AMOUNT_AFTER ) );
  }

  public final static int getOverwriteStep()
  {
    return Integer.parseInt( getProperties().getProperty( OVERWRITE_STEP ) );
  }

  public final static int getOverwriteCalendarField()
  {
    return CalendarUtilities.getCalendarField( getProperties().getProperty( OVERWRITE_CALENDAR_FIELD ) );
  }
}