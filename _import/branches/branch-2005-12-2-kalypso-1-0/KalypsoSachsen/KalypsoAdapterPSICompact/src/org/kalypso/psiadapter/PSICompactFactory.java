package org.kalypso.psiadapter;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.util.CalendarUtilities;
import org.kalypso.ogc.sensor.timeseries.wq.at.AtTable;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQPair;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTable;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableSet;
import org.shiftone.cache.Cache;
import org.shiftone.cache.policy.fifo.FifoCacheFactory;

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
  public static final SimpleDateFormat DF_AT_DICT = new SimpleDateFormat( "dd.mm.yyyy" );
  static
  {
    // Setting the timezone to UTC; we only read days so we want to have 0:0 as time value
    DF_AT_DICT.setTimeZone( TimeZone.getTimeZone( "UTC" ) );
  }

  /**
   * Name of the system property which contains the location of the fake data. Must be an url. Normaly this points to
   * the directory containing the 'structure.txt' file. <br/>ATTENTION: If this property is set, the fake implementation
   * is used instead of the real PSICompact implementation!
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
   * Name of the system property which contains the location of the dictionary of .at files. <br>
   * The dictionary is in the properties-file format (@link{Properties}), each key representing a PSI-Object-ID, each
   * value representing the (relative) path to an .at file. <br>
   */
  public static final String SYSPROP_AT_DICTIONARY = "kalypso.psi.at.properties";

  /** The dictionary of at files: Strig -> Properties */
  private static Map s_atDictionary = null;

  private static URL s_atDictUrl;

  /** First-in-First-Out cache for at-content. 1 Minute, 100 objects */
  private static final Cache s_atCache = new FifoCacheFactory().newInstance( "atCache", 60 * 1000, 100 );

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
          System.out
              .println( "Fake location not set, using real PSICompact-implementation. Use the following System-property to use a fake implementation instead: "
                  + SYSPROP_FAKE_LOCATION );
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

  /**
   * Determines if at based wq-tables are available for the given id.
   */
  public static boolean hasWQTable( final String objectId )
  {
    final Map atDictionary = getAtDictionary();
    return atDictionary.containsKey( objectId );
  }

  /**
   * Returns a local wq-table for a given PSI object id. <br>
   * 
   * @param objectId
   *          Object id as obtained from PSICompact.ObjectInfo
   * @return A locally stored wq-table or <code>null</code> if no entry was found in the at-dictionary.
   */
  public static WQTableSet getWQTable( final String objectId )
  {
    // If the cache contains the desired table, immediately return it
    final WQTableSet cachedTable = (WQTableSet)s_atCache.getObject( objectId );
    if( cachedTable != null )
      return cachedTable;

    // Else, find the dictionary and try to load it from there
    final Map atDictionary = getAtDictionary();
    final WQTableSet tableSet = tableSetFromPreferences( objectId, atDictionary );
    s_atCache.addObject( objectId, tableSet );
    return tableSet;
  }

  private static WQTableSet tableSetFromPreferences( final String objectId, final Map atDictionary )
  {
    if( !atDictionary.containsKey( objectId ) )
      return null;

    final Properties atNode = (Properties)atDictionary.get( objectId );
    final Set dates = atNode.keySet();
    final WQTable[] tables = new WQTable[dates.size()];
    String typeFrom = null;
    String typeTo = null;

    int i = 0;
    for( final Iterator iter = dates.iterator(); iter.hasNext(); )
    {
      final String dateString = (String)iter.next();
      final String atPath = atNode.getProperty( dateString, null );

      try
      {
        final Date validity = DF_AT_DICT.parse( dateString );

        final URL atUrl = new URL( s_atDictUrl, atPath ); // resolve pathes relative to the dictionary location

        final AtTable atTable = AtTable.readAt( atUrl );
        final WQPair[] values = atTable.getValues();
        final String tFrom = atTable.getTypeFrom();
        final String tTo = atTable.getTypeTo();

        // Check consistancy of timeseries types
        if( typeFrom == null )
          typeFrom = tFrom;
        else if( !typeFrom.equals( tFrom ) )
          throw new IllegalStateException( "Konvertierungstyp 'von' ist nur für alle Tafeln gleich: " + tFrom );
        if( typeTo == null )
          typeTo = tTo;
        else if( !typeTo.equals( tTo ) )
          throw new IllegalStateException( "Konvertierungstyp 'nach' ist nur für alle Tafeln gleich: " + tTo );

        tables[i++] = new WQTable( validity, 0, values );
      }
      catch( IllegalStateException e )
      {
        System.out.println( "Invalid data for psi-id '" + objectId + "': " + e.getLocalizedMessage() );
        e.printStackTrace();

        return null;
      }
      catch( ParseException e )
      {
        System.out.println( "Invalid date '" + dateString + "' for psi-id '" + objectId + "': "
            + e.getLocalizedMessage() );
        e.printStackTrace();

        return null;
      }
      catch( MalformedURLException e )
      {
        System.out.println( "Invalid path '" + atPath + "' to at file for psi-id '" + objectId + "': "
            + e.getLocalizedMessage() );
        e.printStackTrace();

        return null;
      }
      catch( IOException e )
      {
        System.out.println( "Failed to read at-file '" + atPath + "' for psi-id '" + objectId + "': "
            + e.getLocalizedMessage() );
        e.printStackTrace();

        return null;
      }
    }

    return new WQTableSet( tables, typeFrom, typeTo );
  }

  /** Lazy loads the dictionary of at tables. */
  private static Map getAtDictionary()
  {
    if( s_atDictionary == null )
    {
      s_atDictionary = new HashMap();

      try
      {
        final String atDictLocation = System.getProperty( SYSPROP_AT_DICTIONARY, null );
        if( atDictLocation == null )
          return s_atDictionary;

        s_atDictUrl = new URL( atDictLocation );
        loadPreferences( s_atDictionary, s_atDictUrl );
      }
      catch( Throwable e )
      {
        System.out.println( "Failed to load AT-Dictionary: " + e.getLocalizedMessage() );
        e.printStackTrace();
      }
    }

    return s_atDictionary;
  }

  /**
   * Reads a windows-ini like file into a Map <String, Properties> <br>
   * TODO: If we change to java5, we could use ini4j instead
   */
  private static void loadPreferences( final Map preferences, final URL location )
  {
    InputStream is = null;
    try
    {
      Pattern groupPattern = Pattern.compile( "\\[(.*)\\]" );

      is = location.openStream();

      final LineNumberReader lnr = new LineNumberReader( new InputStreamReader( is ) );
      String[] currentGroups = new String[]
      { "nogroup" };

      while( lnr.ready() )
      {
        final String line = lnr.readLine();
        if( line == null )
          break;

        if( line.length() == 0 || line.trim().charAt( 0 ) == '#' )
          continue;

        final Matcher matcher = groupPattern.matcher( line );
        if( matcher.matches() )
          currentGroups = matcher.group( 1 ).split( "," );
        else
        {
          final String[] strings = line.split( "=" );
          if( strings.length == 2 )
            addGroupEntry( preferences, currentGroups, strings[0].trim(), strings[1].trim() );
        }
      }

      is.close();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( is );
    }
  }

  /**
   * Helper method for loadPreferences
   */
  private static void addGroupEntry( final Map preferences, final String[] groups, final String key, final String value )
  {
    for( int i = 0; i < groups.length; i++ )
    {
      final String group = groups[i].trim();

      if( !preferences.containsKey( group ) )
        preferences.put( group, new Properties() );

      final Properties properties = (Properties)preferences.get( group );
      properties.put( key, value );
    }
  }

}