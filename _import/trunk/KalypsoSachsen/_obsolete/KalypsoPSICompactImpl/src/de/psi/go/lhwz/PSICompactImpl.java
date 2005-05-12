package de.psi.go.lhwz;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.proxy.ArgsObservationProxy;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.wq.WQException;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannFactory;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannGroup;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannParams;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannSet;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.util.runtime.args.DateRangeArgument;
import org.xml.sax.InputSource;

/**
 * Test Implementierung von der PSICompact Schnitstelle.
 * 
 * @author schlienger
 */
public class PSICompactImpl implements PSICompact
{
  private static final String PROP_DISTRIB_PATH = "distributePath";

  private final static ArchiveData[] EMPTY_DATA = new ArchiveData[0];
  
  private final Map m_id2obj;

  private final Map m_id2zml;

  private final Map m_id2zmlObs;

  private final Map m_id2wq;

  private final Map m_id2values;

  private boolean m_init = false;

  private Properties m_conf;


  public PSICompactImpl( )
  {
    super();

    m_id2values = new HashMap();
    m_id2obj = new HashMap();
    m_id2wq = new HashMap();
    m_id2zml = new HashMap();
    m_id2zmlObs = new HashMap();
  }

  /**
   * simuliert eine Liste von PSI Objekte
   * 
   * @param mapId
   * @param mapZml
   * @throws ECommException
   */
  private final void prepareObjects( final Map mapId, final Map mapZml )
      throws ECommException
  {
    final BufferedReader reader ;
    try
    {
      reader = new BufferedReader( new InputStreamReader(
          getClass().getResourceAsStream( "fake/lhwz-ids.csv" ), "UTF-8" ) );
    }
    catch( final UnsupportedEncodingException e1 )
    {
      e1.printStackTrace();
      throw new ECommException( e1 );
    }
    try
    {
      String line = reader.readLine();

      while( line != null )
      {
        if( line.length() > 0 )
        {
          final String[] splitsLine = line.split( ";" );

          if( splitsLine.length >= 1 )
          {
            String id = splitsLine[0].replaceAll( ";", "" );

            String zml = splitsLine.length >= 2 ? splitsLine[1] : "";
            String desc = splitsLine.length >= 3 ? splitsLine[2] : "";

            if( zml != "" )
              mapZml.put( id, zml );

            if( desc == "" )
            {
              final String[] splitsId = id.split( "\\." );
              desc = splitsId[splitsId.length - 1];
            }
            mapId.put( id, new ObjectInfo( id, desc ) );
          }
        }

        line = reader.readLine();
      }
    }
    catch( IOException e )
    {
      throw new ECommException( e );
    }
    finally
    {
      try
      {
        reader.close();
      }
      catch( IOException e )
      {
        e.printStackTrace();
      }
    }
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#init()
   */
  public void init( ) throws ECommException
  {
    m_init = true;

    // load configuration from properties file
    m_conf = new Properties();
    final InputStream ins = PSICompactImpl.class
        .getResourceAsStream( "config.ini" );
    try
    {
      m_conf.load( ins );
    }
    catch( IOException e )
    {
      throw new ECommException( e );
    }
    finally
    {
      try
      {
        ins.close();
      }
      catch( IOException e1 )
      {
        e1.printStackTrace();
      }
    }

    // build PSICompact-Structure
    prepareObjects( m_id2obj, m_id2zml );
  }

  private void testInitDone( ) throws ECommException
  {
    if( !m_init )
      throw new ECommException( "PSICompact.init() not performed!" );
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#writeProtocol(java.lang.String)
   */
  public boolean writeProtocol( String message ) throws ECommException
  {
    testInitDone();

    System.out.println( "PSICompact.writeProtocol: " + message );

    return true;
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getInfo(int)
   */
  public ObjectInfo[] getInfo( int typespec ) throws ECommException
  {
    testInitDone();

    return (ObjectInfo[]) m_id2obj.values().toArray( new ObjectInfo[0] );
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getArchiveData(java.lang.String, int,
   *      java.util.Date, java.util.Date)
   */
  public ArchiveData[] getArchiveData( String id, int arcType, Date from,
      Date to ) throws ECommException
  {
    testInitDone();

    if( from == null || to == null )
      return EMPTY_DATA;

    if( m_id2obj.containsKey( id ) )
    {
      // overriden?
      if( m_id2values.containsKey( id ) )
        return (ArchiveData[]) m_id2values.get( id );

      // zml?
      if( m_id2zml.containsKey( id ) )
        return readFromZml( id, from, to );

      // random
//      return randomData( from, to );
      return EMPTY_DATA;
    }

    return EMPTY_DATA;
  }

  /**
   * Helper for lazy loading the zml observation
   * 
   * @throws ECommException
   */
  private IObservation getZmlObs( final String id ) throws ECommException
  {
    // already loaded?
    IObservation obs = (IObservation) m_id2zmlObs.get( id );
    if( obs != null )
      return obs;

    final String fname = (String) m_id2zml.get( id );

    final InputStream stream = getClass().getResourceAsStream( "fake/" + fname );
    InputSource ins = new InputSource( stream );

    try
    {
      final URL url = getClass().getResource( "fake/" + fname );

      obs = ZmlFactory.parseXML( ins, fname, url );

      m_id2zmlObs.put( id, obs );

      return obs;
    }
    catch( Exception e )
    {
      throw new ECommException( e );
    }
    finally
    {
      IOUtils.closeQuietly( stream );
    }
  }

  /**
   * Reads the data from the underlying zml
   * 
   * @param id
   * @param from
   * @param to
   * @return data
   * @throws ECommException
   */
  private ArchiveData[] readFromZml( String id, Date from, Date to )
      throws ECommException
  {
    try
    {
      final ArgsObservationProxy obs = new ArgsObservationProxy(
          new DateRangeArgument( from, to ), getZmlObs( id ) );

      final ITuppleModel values = obs.getValues( null );
      final IAxis dateAxis = ObservationUtilities.findAxisByClass( obs
          .getAxisList(), Date.class );
      final IAxis valueAxis = KalypsoStatusUtils.findAxisByClass( obs
          .getAxisList(), Number.class, true );

      final ArchiveData[] data = new ArchiveData[values.getCount()];
      for( int i = 0; i < data.length; i++ )
      {
        Date d = (Date) values.getElement( i, dateAxis );
        Number n = (Number) values.getElement( i, valueAxis );

        data[i] = new ArchiveData( d, PSICompact.STATUS_AUTO, n.doubleValue() );
      }

      return data;
    }
    catch( Exception e )
    {
      throw new ECommException( e );
    }
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#setArchiveData(java.lang.String, int,
   *      java.util.Date, de.psi.go.lhwz.PSICompact.ArchiveData[])
   */
  public boolean setArchiveData( String id, int arcType, Date from,
      ArchiveData[] data ) throws ECommException
  {
    testInitDone();

    // simply store it temporarely
    m_id2values.put( id, data );

    return true;
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getWQParams(java.lang.String)
   */
  public WQParamSet[] getWQParams( String id ) throws ECommException
  {
    testInitDone();

    WQParamSet[] pset = (WQParamSet[]) m_id2wq.get( id );
    if( pset != null )
      return pset;

    // try to get the info from the underlying zml, if any
    if( m_id2zml.containsKey( id ) )
    {
      final String wqParam = getZmlObs( id ).getMetadataList().getProperty(
          TimeserieConstants.MD_WQWECHMANN );

      if( wqParam != null )
      {
        StringReader sr = new StringReader( wqParam );
        final InputSource src = new InputSource( sr );

        final WechmannGroup group;
        try
        {
          group = WechmannFactory.parse( src );
        }
        catch( WQException e )
        {
          e.printStackTrace();
          throw new ECommException( e );
        }

        final ArrayList wqps = new ArrayList();

        final Iterator its = group.iterator();
        while( its.hasNext() )
        {
          final WechmannSet ws = (WechmannSet) its.next();

          final ArrayList wqds = new ArrayList();
          final Iterator itp = ws.iterator();
          while( itp.hasNext() )
          {
            final WechmannParams params = (WechmannParams) itp.next();
            final WQData data = new WQData( params.getWGR(), params.getW1(),
                params.getLNK1(), params.getK2() );
            wqds.add( data );
          }

          wqps.add( new WQParamSet( ws.getValidity(), (WQData[]) wqds
              .toArray( new WQData[wqds.size()] ) ) );
        }

        pset = (WQParamSet[]) wqps.toArray( new WQParamSet[wqps.size()] );
        m_id2wq.put( id, pset );
      }
    }

    return pset;
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getObjectMetaData(java.lang.String)
   */
  public ObjectMetaData getObjectMetaData( String id ) throws ECommException
  {
    testInitDone();

    final ObjectMetaData omd = new ObjectMetaData();
    omd.setId( id );

    // try to get the info from the underlying zml, if any
    if( m_id2zml.containsKey( id ) )
    {
      final IObservation obs = getZmlObs( id );
      final MetadataList mdl = obs.getMetadataList();

      String p = null;
      p = mdl.getProperty( TimeserieConstants.MD_ALARM_1 );
      if( p != null )
        omd.setAlarm1( Double.valueOf(p).doubleValue() );

      p = mdl.getProperty( TimeserieConstants.MD_ALARM_2 );
      if( p != null )
        omd.setAlarm2( Double.valueOf(p).doubleValue() );

      p = mdl.getProperty( TimeserieConstants.MD_ALARM_3 );
      if( p != null )
        omd.setAlarm3( Double.valueOf(p).doubleValue() );

      p = mdl.getProperty( TimeserieConstants.MD_ALARM_4 );
      if( p != null )
        omd.setAlarm4( Double.valueOf(p).doubleValue() );

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
        omd.setHeight( Double.valueOf(p).intValue());

      
      p = mdl.getProperty( TimeserieConstants.MD_GKR );
      if( p != null )
        omd.setRight( Double.valueOf(p).intValue() );

      p = mdl.getProperty( TimeserieConstants.MD_HOEHENANGABEART );
      if( p != null )
        omd.setLevelUnit( p );
      else
        omd.setLevelUnit( "" );

      p = mdl.getProperty( TimeserieConstants.MD_MESSTISCHBLATT );
      if( p != null )
        omd.setMapNo( Double.valueOf(p).intValue() );

      p = mdl.getProperty( TimeserieConstants.MD_PEGELNULLPUNKT );
      if( p != null )
        omd.setLevel( Double.valueOf(p).doubleValue() );

      final IAxis nba = KalypsoStatusUtils.findAxisByClass(
          obs.getAxisList(), Number.class, true );

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
  public String[] getUserClasses( String userId ) throws ECommException
  {
    testInitDone();

    return m_conf.getProperty( userId, "" ).split( "," );
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getUserRights(java.lang.String,
   *      java.lang.String)
   */
  public String[] getUserRights( String userId, String userClass )
      throws ECommException
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
   * 
   * @return random data
   */
  protected final ArchiveData[] randomData( final Date from, final Date to )
  {
    final Calendar cal = Calendar.getInstance();
    cal.setTime( from );

    final ArrayList data = new ArrayList();
    while( cal.getTime().compareTo( to ) < 0 )
    {
      data.add( new ArchiveData( cal.getTime(), PSICompact.STATUS_AUTO, Math
          .random() * 100 ) );

      cal.add( Calendar.MINUTE, 15 );
    }

    return (ArchiveData[]) data.toArray( new ArchiveData[data.size()] );
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getMeasureType(java.lang.String)
   */
  public int getMeasureType( String id ) throws ECommException
  {
    testInitDone();

    // try to get the info from the underlying zml, if any
    if( m_id2zml.containsKey( id ) )
    {
      final IObservation obs = getZmlObs( id );
      final IAxis nba = KalypsoStatusUtils.findAxisByClass(
          obs.getAxisList(), Number.class, true );

      return toMeasType( nba.getType() );
    }

    return PSICompact.MEAS_LEVEL;
  }

  /**
   * Helper that returns the psi measure type (MEAS_*) according to the axis
   * type
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
  public boolean distributeFile( String filename ) throws ECommException
  {
    testInitDone();

    return false;
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#copyanddistributeFile(java.io.File,
   *      java.lang.String)
   */
  public boolean copyanddistributeFile( final File source, final String destination )
      throws ECommException
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
      FileUtils.copyFile( source , destfile );
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
  public boolean removeFile( String filename ) throws ECommException
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
  public String[] getUser( String userClass ) throws ECommException
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
  public String getComment( String Pegelkennziffer ) throws ECommException
  {
    testInitDone();

    return "Kommentar für " + Pegelkennziffer;
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getGewaesser(java.lang.String)
   */
  public String getGewaesser( String Pegelkennziffer ) throws ECommException
  {
    testInitDone();

    return "Gewaesser von " + Pegelkennziffer;
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getFlussgebiet(java.lang.String)
   */
  public String getFlussgebiet( String Pegelkennziffer ) throws ECommException
  {
    testInitDone();

    return "Flussgebiet von " + Pegelkennziffer;
  }
}