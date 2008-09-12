package org.kalypso.psiadapter;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.ArrayUtils;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.contribs.java.util.PropertiesUtilities;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.proxy.RequestObservationProxy;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.interpolation.InterpolationFilter;
import org.kalypso.ogc.sensor.timeseries.wq.WQException;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannFactory;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannGroup;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannParams;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannSet;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.psiadapter.repository.PSICompactUtilitites;
import org.xml.sax.InputSource;

import de.psi.go.lhwz.ECommException;
import de.psi.go.lhwz.PSICompact;

/**
 * Test Implementierung von der PSICompact Schnitstelle.
 * 
 * @author Marc Schlienger
 */
public class PSICompactFakeImpl implements PSICompact
{
  private static final String JAR_PROTOCOL = "jar:";

  private static final String PROP_STRUCT_LOCATION = "structureFile";

  private static final String PROP_DISTRIB_PATH = "distributePath";

  private final static ArchiveData[] EMPTY_DATA = new ArchiveData[0];

  private final Map<String, ObjectInfo> m_id2obj = new HashMap<String, ObjectInfo>();

  private final Map<String, String> m_id2zml = new HashMap<String, String>();

  private final Map<String, WQParamSet[]> m_id2wq = new HashMap<String, WQParamSet[]>();

  private final Map<String, ArchiveData[]> m_id2values = new HashMap<String, ArchiveData[]>();

  private final Map<String, Set<Integer>> m_id2archiv = new HashMap<String, Set<Integer>>();

  private final Properties m_conf = new Properties();

  private final String m_fakeLocation;

  private URL m_structureLocation;

  public PSICompactFakeImpl( final String fakeLocation )
  {
    m_fakeLocation = fakeLocation;
  }

  /**
   * Simuliert eine Liste von PSI Objekten
   * 
   * @param mapId
   * @param mapZml
   * @param mapArchiveTypes
   * @throws ECommException
   */
  private static final void prepareObjects( final URL fakeStructureLocation, final Map<String, ObjectInfo> mapId, final Map<String, String> mapZml, final Map<String, Set<Integer>> mapArchiveTypes ) throws ECommException
  {
    BufferedReader reader = null;

    try
    {
      reader = new BufferedReader( new InputStreamReader( fakeStructureLocation.openStream(), "UTF-8" ) );

      String line = reader.readLine();
      while( line != null )
      {
        if( line.length() > 0 )
        {
          final String[] splitsLine = line.split( ";" );

          if( splitsLine.length >= 1 )
          {
            final String id = splitsLine[0].replaceAll( ";", "" );

            final String[] idParts = id.split( "#" );

            final String baseID;
            final String archivTypeStr;

            if( idParts.length == 1 )
            {
              baseID = idParts[0];
              archivTypeStr = null;
            }
            else if( idParts.length == 2 )
            {
              baseID = idParts[0];
              archivTypeStr = idParts[1];
            }
            else
              throw new ECommException( "Invalid id: " + id );

            final String zml = splitsLine.length >= 2 ? splitsLine[1] : "";

            if( !mapArchiveTypes.containsKey( baseID ) )
            {
              final Set<Integer> set = new TreeSet<Integer>();
              set.add( new Integer( PSICompact.ARC_MIN15 ) );
              mapArchiveTypes.put( baseID, set );
            }

            if( archivTypeStr != null )
            {
              final int archivType = PSICompactUtilitites.archiveTypeFromString( archivTypeStr );
              final Set<Integer> set = mapArchiveTypes.get( baseID );
              set.add( new Integer( archivType ) );
            }

            if( zml.length() > 0 )
              mapZml.put( id, zml );

            String desc = splitsLine.length >= 3 ? splitsLine[2] : null;
            if( desc == null )
            {
              final String[] splitsId = baseID.split( "\\." );
              desc = splitsId[splitsId.length - 1];
            }
            mapId.put( baseID, new ObjectInfo( baseID, desc ) );
          }
        }

        line = reader.readLine();
      }
    }
    catch( final IOException e )
    {
      throw new ECommException( e );
    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#init()
   */
  public void init( ) throws ECommException
  {
    try
    {
      final URL baseLocation = new URL( m_fakeLocation );
      final URL configLocation = new URL( baseLocation, "./config.ini" );

      PropertiesUtilities.load( configLocation, m_conf );

      // build PSICompact-Structure
      final String structurePath = m_conf.getProperty( PROP_STRUCT_LOCATION );
      m_structureLocation = createStructureURL( configLocation, structurePath );
      prepareObjects( m_structureLocation, m_id2obj, m_id2zml, m_id2archiv );
    }
    catch( final IOException e )
    {
      throw new ECommException( e );
    }

  }

  private URL createStructureURL( final URL configLocation, final String zipPath ) throws MalformedURLException
  {
    final URL url = new URL( configLocation, zipPath );
    final String zipUrl = JAR_PROTOCOL + url.toExternalForm() + "!/structure.txt";
    return new URL( zipUrl );
  }

  private void testInitDone( ) throws ECommException
  {
    if( m_conf.isEmpty() )
      throw new ECommException( "PSICompact.init() not performed!" );
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#writeProtocol(java.lang.String)
   */
  public boolean writeProtocol( final String message ) throws ECommException
  {
    testInitDone();

    System.out.println( "PSICompact.writeProtocol: " + message );

    return true;
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getInfo(int)
   */
  public ObjectInfo[] getInfo( final int typespec ) throws ECommException
  {
    testInitDone();

    return m_id2obj.values().toArray( new ObjectInfo[0] );
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getArchiveData(int, int, java.util.Date, java.util.Date)
   */
  public ArchiveData[] getArchiveData( final int arg0, final int arg1, final Date arg2, final Date arg3 )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getArchiveData(java.lang.String, int, java.util.Date, java.util.Date)
   */
  public ArchiveData[] getArchiveData( final String id, final int arcType, final Date from, final Date to ) throws ECommException
  {
    testInitDone();

    if( from == null || to == null )
      return EMPTY_DATA;

    final String fullID = getFullID( id, arcType );

    // Set from outside?
    if( m_id2values.containsKey( fullID ) )
      return m_id2values.get( fullID );

    // zml?
    return readFromZml( id, from, to, arcType );
  }

  private IObservation getInterpolatedZmlObs( final String id, final int arcType ) throws ECommException
  {
    final IObservation zmlObs = getZmlObs( id, arcType );
    if( zmlObs == null )
      return null;

    try
    {
      final int field = PSICompactUtilitites.arcTypeToCalendarField( arcType );
      final int amount = PSICompactUtilitites.arcTypeToCalendarAmount( arcType );

      // TODO: use intervall filter for rainfall instead of InterpolationFilter

      final InterpolationFilter filter = new InterpolationFilter( field, amount, false, Double.toString( Double.NaN ), -1 );
      filter.initFilter( null, zmlObs, null );
      return filter;
    }
    catch( final SensorException e )
    {
      e.printStackTrace();

      throw new ECommException( e );
    }

  }

  public static String getFullID( final String id, final int arcType ) throws ECommException
  {
    final String arcId = PSICompactUtilitites.getIdForArcType( arcType );
    return id + "#" + arcId;
  }

  /**
   * Helper for lazy loading the zml observation
   */
  private IObservation getZmlObs( final String id, final int arcType ) throws ECommException
  {
    final String fullID = getFullID( id, arcType );

    final String zmlPath;
    if( m_id2zml.containsKey( fullID ) )
      zmlPath = m_id2zml.get( fullID );
    /* Fallback, maybe only the pure id was specified in the structure file, so we try it like that */
    else if( m_id2zml.containsKey( id ) )
      zmlPath = m_id2zml.get( id );
    else
      zmlPath = null;

    if( zmlPath == null )
      return null;

    try
    {
      final URL location = new URL( m_structureLocation, zmlPath );

      return ZmlFactory.parseXML( location, zmlPath );
    }
    catch( final Exception e )
    {
      throw new ECommException( e );
    }
  }

  /**
   * Reads the data from the underlying zml
   * 
   * @param arcType
   */
  private ArchiveData[] readFromZml( final String id, final Date from, final Date to, final int arcType ) throws ECommException
  {
    try
    {
      final IObservation zmlObs = getInterpolatedZmlObs( id, arcType );
      if( zmlObs == null )
        return EMPTY_DATA;

      final RequestObservationProxy obs = new RequestObservationProxy( new ObservationRequest( new DateRange( from, to ) ), zmlObs );

      // TODO: probably we should use use a request here! Else we get always everything which is inside the zml
// final ITuppleModel values = obs.getValues( new ObservationRequest( new DateRange( from, to ) ) );
      final ITuppleModel values = obs.getValues( null );
      final IAxis dateAxis = ObservationUtilities.findAxisByClass( obs.getAxisList(), Date.class );
      final IAxis valueAxis = KalypsoStatusUtils.findAxisByClass( obs.getAxisList(), Number.class, true );

      final ArchiveData[] data = new ArchiveData[values.getCount()];
      for( int i = 0; i < data.length; i++ )
      {
        final Date d = (Date) values.getElement( i, dateAxis );
        final Number n = (Number) values.getElement( i, valueAxis );

        data[i] = new ArchiveData( d, PSICompact.STATUS_AUTO, n.doubleValue() );
      }

      return data;
    }
    catch( final Exception e )
    {
      throw new ECommException( e );
    }
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#setArchiveData(java.lang.String, int, java.util.Date,
   *      de.psi.go.lhwz.PSICompact.ArchiveData[])
   */
  public boolean setArchiveData( final String id, final int arcType, final Date from, final ArchiveData[] data ) throws ECommException
  {
    testInitDone();

    final String fullID = getFullID( id, arcType );

    // simply store it in memory
    m_id2values.put( fullID, data );

    return true;
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getWQParams(java.lang.String)
   */
  public WQParamSet[] getWQParams( final String id ) throws ECommException
  {
    testInitDone();

    final WQParamSet[] pset = m_id2wq.get( id );
    if( pset != null )
      return pset;

    final IObservation obs = getZmlObs( id, PSICompact.ARC_MIN15 );
    if( obs == null )
      return new WQParamSet[0];

    final String wqParam = obs.getMetadataList().getProperty( TimeserieConstants.MD_WQWECHMANN );

    if( wqParam != null )
    {
      final StringReader sr = new StringReader( wqParam );
      final InputSource src = new InputSource( sr );

      final WechmannGroup group;
      try
      {
        group = WechmannFactory.parse( src );
      }
      catch( final WQException e )
      {
        e.printStackTrace();
        throw new ECommException( e );
      }

      final List<WQParamSet> wqps = new ArrayList<WQParamSet>();

      final Iterator<WechmannSet> its = group.iterator();
      while( its.hasNext() )
      {
        final WechmannSet ws = its.next();

        final List<WQData> wqds = new ArrayList<WQData>();
        final Iterator<WechmannParams> itp = ws.iterator();
        while( itp.hasNext() )
        {
          final WechmannParams params = itp.next();
          final WQData data = new WQData( params.getWGR(), params.getW1(), params.getLNK1(), params.getK2() );
          wqds.add( data );
        }

        wqps.add( new WQParamSet( ws.getValidity(), wqds.toArray( new WQData[wqds.size()] ) ) );
      }

      final WQParamSet[] newPset = wqps.toArray( new WQParamSet[wqps.size()] );
      m_id2wq.put( id, newPset );
      return newPset;
    }

    return new WQParamSet[0];
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getObjectMetaData(java.lang.String)
   */
  public ObjectMetaData getObjectMetaData( final String id ) throws ECommException
  {
    testInitDone();

    final ObjectMetaData omd = new ObjectMetaData();
    omd.setId( id );

    // try to get the info from the underlying zml, if any
    if( m_id2zml.containsKey( id ) )
    {
      final IObservation obs = getZmlObs( id, PSICompact.ARC_MIN15 );
      final MetadataList mdl = obs.getMetadataList();

      String p = null;
      p = mdl.getProperty( TimeserieConstants.MD_ALARM_1 );
      if( p != null )
        omd.setAlarm1( NumberUtils.parseQuietDouble( p ) );

      p = mdl.getProperty( TimeserieConstants.MD_ALARM_2 );
      if( p != null )
        omd.setAlarm2( NumberUtils.parseQuietDouble( p ) );

      p = mdl.getProperty( TimeserieConstants.MD_ALARM_3 );
      if( p != null )
        omd.setAlarm3( NumberUtils.parseQuietDouble( p ) );

      p = mdl.getProperty( TimeserieConstants.MD_ALARM_4 );
      if( p != null )
        omd.setAlarm4( NumberUtils.parseQuietDouble( p ) );

      p = mdl.getProperty( TimeserieConstants.MD_GEWAESSER );
      if( p != null )
        omd.setRiver( p );
      else
        omd.setRiver( "" );

      p = mdl.getProperty( TimeserieConstants.MD_FLUSSGEBIET );
      if( p != null )
        omd.setRiversystem( p );
      else
        omd.setRiversystem( "" );

      p = mdl.getProperty( TimeserieConstants.MD_GKH );
      if( p != null )
        omd.setHeight( Double.valueOf( p ).intValue() );

      p = mdl.getProperty( TimeserieConstants.MD_GKR );
      if( p != null )
        omd.setRight( Double.valueOf( p ).intValue() );

      p = mdl.getProperty( TimeserieConstants.MD_HOEHENANGABEART );
      if( p != null )
        omd.setLevelUnit( p );
      else
        omd.setLevelUnit( "" );

      p = mdl.getProperty( TimeserieConstants.MD_MESSTISCHBLATT );
      if( p != null )
        omd.setMapNo( Double.valueOf( p ).intValue() );

      p = mdl.getProperty( TimeserieConstants.MD_PEGELNULLPUNKT );
      if( p != null )
        omd.setLevel( NumberUtils.parseQuietDouble( p ) );

      final IAxis nba = KalypsoStatusUtils.findAxisByClass( obs.getAxisList(), Number.class, true );

      omd.setUnit( whichUnit( nba.getUnit() ) );
    }
    else
    {
      // fake metadata
      omd.setAlarm1( 1.0 );
      omd.setAlarm2( 2.0 );
      omd.setAlarm3( 3.0 );
      omd.setAlarm4( 4.0 );
      omd.setHeight( 34 );
      omd.setId( id );
      omd.setLevel( 56.8 );
      omd.setLevelUnit( "Etwas..." );
      omd.setMapNo( 7 );
      omd.setRight( 987654321 );
      omd.setRiver( "Fluss..." );
      omd.setRiversystem( "Flussgebiet..." );
      omd.setUnit( SI_NO_UNIT );
    }

    // Archive data
    final Set<Integer> set = m_id2archiv.get( id );
    if( set == null )
      omd.setArchiveData( new int[] { PSICompact.ARC_MIN15 } );
    else
    {
      final Integer[] setArray = set.toArray( new Integer[set.size()] );
      omd.setArchiveData( ArrayUtils.toPrimitive( setArray ) );
    }

    return omd;
  }

  /**
   * Helper that returns the PSI unit according to a unit string
   * 
   * @param unit
   * @return psi unit
   */
  private int whichUnit( final String unit )
  {
    if( unit.equals( "m" ) )
      return PSICompact.SI_METER;
    if( unit.equals( "m³/s" ) )
      return PSICompact.SI_CUBIC_METER_PER_SECOND;
    if( unit.equals( "K" ) )
      return PSICompact.SI_KELVIN;
    if( unit.equals( "" ) )
      return PSICompact.SI_NO_UNIT;
    if( unit.equals( "m³" ) )
      return PSICompact.SI_QUBIC_METER;

    return PSICompact.SI_UNDEF;
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getUserClasses(java.lang.String)
   */
  public String[] getUserClasses( final String userId ) throws ECommException
  {
    testInitDone();

    return m_conf.getProperty( userId, "" ).split( "," );
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getUserRights(java.lang.String, java.lang.String)
   */
  public String[] getUserRights( final String userId, final String userClass ) throws ECommException
  {
    testInitDone();

    final String property = m_conf.getProperty( userId, null );
    if( property == null )
      return null;

    return property.split( "," );
  }

  /**
   * Helper: erzeugt eine Zeitreihe mit random Werte
   * 
   * @param from
   * @param to
   * @return random data
   */
  protected final ArchiveData[] randomData( final Date from, final Date to )
  {
    final Calendar cal = Calendar.getInstance();
    cal.setTime( from );

    final List<ArchiveData> data = new ArrayList<ArchiveData>();
    while( cal.getTime().compareTo( to ) < 0 )
    {
      data.add( new ArchiveData( cal.getTime(), PSICompact.STATUS_AUTO, Math.random() * 100 ) );

      cal.add( Calendar.MINUTE, 15 );
    }

    return data.toArray( new ArchiveData[data.size()] );
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getMeasureType(java.lang.String)
   */
  public int getMeasureType( final String id ) throws ECommException
  {
    testInitDone();

    // try to get the info from the underlying zml, if any
    final IObservation obs = getZmlObs( id, PSICompact.ARC_MIN15 );
    if( obs == null )
      return PSICompact.MEAS_LEVEL;

    final IAxis nba = KalypsoStatusUtils.findAxisByClass( obs.getAxisList(), Number.class, true );

    return toMeasType( nba.getType() );

  }

  /**
   * Helper that returns the psi measure type (MEAS_*) according to the axis type
   * 
   * @param axisType
   * @return measure type
   */
  private static int toMeasType( final String axisType )
  {
    if( axisType.equals( TimeserieConstants.TYPE_RAINFALL ) )
      return PSICompact.MEAS_RAINFALL;
    if( axisType.equals( TimeserieConstants.TYPE_RUNOFF ) )
      return PSICompact.MEAS_FLOW;
    if( axisType.equals( TimeserieConstants.TYPE_TEMPERATURE ) )
      return PSICompact.MEAS_TEMPERATUR;
    if( axisType.equals( TimeserieConstants.TYPE_WATERLEVEL ) )
      return PSICompact.MEAS_LEVEL;

    return PSICompact.MEAS_UNDEF;
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#distributeFile(java.lang.String)
   */
  public boolean distributeFile( final String filename ) throws ECommException
  {
    testInitDone();

    return false;
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#copyanddistributeFile(java.io.File, java.lang.String)
   */
  public boolean copyanddistributeFile( final File source, final String destination ) throws ECommException
  {
    testInitDone();

    final String basepath = m_conf.getProperty( PROP_DISTRIB_PATH, null );
    if( basepath == null )
      return false;

    final File dir = new File( basepath );
    final File destfile = new File( dir, destination );
    destfile.getParentFile().mkdirs();
    try
    {
      FileUtils.copyFile( source, destfile );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      return false;
    }

    return true;
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#removeFile(java.lang.String)
   */
  public boolean removeFile( final String filename ) throws ECommException
  {
    testInitDone();

    return false;
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getDataModelVersion()
   */
  public int getDataModelVersion( ) throws ECommException
  {
    testInitDone();

    return 0;
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getUser(java.lang.String)
   */
  public String[] getUser( final String userClass ) throws ECommException
  {
    testInitDone();

    return m_conf.getProperty( userClass, "" ).split( "," );
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getActivDBGroup()
   */
  public int getActivDBGroup( ) throws ECommException
  {
    testInitDone();

    return 0;
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getComment(java.lang.String)
   */
  public String getComment( final String Pegelkennziffer ) throws ECommException
  {
    testInitDone();

    return "Kommentar für " + Pegelkennziffer;
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getGewaesser(java.lang.String)
   */
  public String getGewaesser( final String Pegelkennziffer ) throws ECommException
  {
    testInitDone();

    return "Gewaesser von " + Pegelkennziffer;
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getFlussgebiet(java.lang.String)
   */
  public String getFlussgebiet( final String Pegelkennziffer ) throws ECommException
  {
    testInitDone();

    return "Flussgebiet von " + Pegelkennziffer;
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#finit()
   */
  public void finit( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#Name2Tid(java.lang.String)
   */
  public int Name2Tid( final String arg0 )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#PKZ2Kennzeichen(java.lang.String)
   */
  public String PKZ2Kennzeichen( final String arg0 )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#setComment(java.lang.String, java.lang.String)
   */
  public void setComment( final String arg0, final String arg1 )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#quittiere(int, java.lang.String, long, long)
   */
  public void quittiere( final int arg0, final String arg1, final long arg2, final long arg3 )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getLetztePegel(java.lang.String)
   */
  public Pegelinfo[] getLetztePegel( final String arg0 )
  {
    throw new UnsupportedOperationException();
  }
}