package de.psi.go.lhwz;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import javax.swing.JOptionPane;

import org.apache.commons.io.IOUtils;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.util.runtime.args.DateRangeArgument;
import org.xml.sax.InputSource;

/**
 * Test Implementierung von der PSICompact Schnitstelle.
 * 
 * @author schlienger
 */
public class PSICompactImpl implements PSICompact
{
  private final Map m_id2obj;

  private final Map m_id2zml;

  private final Map m_id2wq;

  private final Map m_id2values;

  private final Map m_id2measType;

  private boolean m_init = false;

  private Properties m_conf;

  public PSICompactImpl( )
  {
    super();

    m_id2values = new HashMap();
    m_id2obj = new HashMap();
    m_id2measType = new HashMap();
    m_id2wq = new HashMap();
    m_id2zml = new HashMap();
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
    final BufferedReader reader = new BufferedReader( new InputStreamReader(
        getClass().getResourceAsStream( "fake/lhwz-ids.csv" ) ) );

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
            String id = splitsLine[0].replaceAll(";", "");
            
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

    JOptionPane.showMessageDialog( JOptionPane.getRootFrame(), message );

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
      return new ArchiveData[0];

    if( m_id2obj.containsKey( id ) )
    {
      // overriden?
      if( m_id2values.containsKey( id ) )
        return (ArchiveData[]) m_id2values.get( id );

      final ArchiveData[] data;

      // zml?
      if( m_id2zml.containsKey( id ) )
        data = readFromZml( id, from, to );
      else
        data = randomData( from, to );

      m_id2values.put( id, data );
      return data;
    }

    return new ArchiveData[0];
  }

  /**
   * @param id
   * @param from
   * @param to
   * @return data
   */
  private ArchiveData[] readFromZml( String id, Date from, Date to )
  {
    final String fname = (String) m_id2zml.get( id );

    final InputStream stream = getClass().getResourceAsStream( "fake/" + fname );
    InputSource ins = new InputSource( stream );

    final DateRangeArgument dra = new DateRangeArgument( from, to );
    final String strUrl = ZmlURL.insertDateRange( getClass().getResource(
        "fake/" + fname ).toExternalForm(), dra );
    try
    {
      final URL url = new URL( strUrl );

      final IObservation obs = ZmlFactory.parseXML( ins, fname, url );

      final ITuppleModel values = obs.getValues( dra );
      final IAxis dateAxis = ObservationUtilities.findAxisByClass( obs
          .getAxisList(), Date.class )[0];
      final IAxis valueAxis = ObservationUtilities.findAxisByClass( obs
          .getAxisList(), Number.class )[0];

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
      e.printStackTrace();

      return null;
    }
    finally
    {
      IOUtils.closeQuietly( stream );
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
    if( pset == null )
    {
      pset = new WQParamSet[] { new WQParamSet( new Date(),
          new WQData[] { new WQData( Math.random(), Math.random(), Math
              .random(), Math.random() ) } ) };

      m_id2wq.put( id, pset );
    }

    return pset;
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getObjectMetaData(java.lang.String)
   */
  public ObjectMetaData getObjectMetaData( String id ) throws ECommException
  {
    testInitDone();

    ObjectMetaData omd = new ObjectMetaData();

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

    return omd;
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
  private final ArchiveData[] randomData( final Date from, final Date to )
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

    Integer intObj = (Integer) m_id2measType.get( id );

    if( intObj == null )
    {
      double d = Math.random();

      int enumMeas = 4;
      double interval = 1.0 / enumMeas;

      for( int i = 0; i < enumMeas; i++ )
        if( d <= i * interval )
        {
          intObj = new Integer( i );
          break;
        }

      if( intObj == null )
        intObj = new Integer( PSICompact.MEAS_UNDEF );

      m_id2measType.put( id, intObj );
    }

    return intObj.intValue();
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
  public boolean copyanddistributeFile( File source, String destination )
      throws ECommException
  {
    testInitDone();

    return false;
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