package de.psi.go.lhwz;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import javax.swing.JOptionPane;

/**
 * Test Implementierung von der PSICompact Schnitstelle.
 * 
 * @author schlienger
 */
public class PSICompactImpl implements PSICompact
{
  //  private final String m_replicationDir;

  private final Map m_id2gemessene;

  private final Map m_id2vorhergesagte;

  private final Map m_id2values;

  private final Map m_id2measType;

  private final Map m_id2wq;

  //  private final Map m_id2zml;

  private boolean m_init = false;

  private Properties m_conf;

  public PSICompactImpl( )
  {
    super();

    m_id2values = new HashMap();
    m_id2gemessene = new HashMap();
    m_id2vorhergesagte = new HashMap();
    m_id2measType = new HashMap();
    m_id2wq = new HashMap();
    //    m_id2zml = new HashMap();

    //    m_replicationDir = System.getProperty( "java.io.tmpdir" );
  }

  /**
   * simuliert eine Liste von PSI Objekte
   * 
   * @param map
   * @param suffix
   * @throws ECommException
   */
  private final void prepareObjects( Map map, String suffix )
      throws ECommException
  {
    final BufferedReader reader = new BufferedReader( new InputStreamReader(
        getClass().getResourceAsStream( "lhwz-ids.csv" ) ) );

    try
    {
      String line = reader.readLine();

      while( line != null )
      {
        final String id = line + "." + suffix;
        final String[] splits = line.split( "\\." );
        map.put( id , new ObjectInfo( id, splits[splits.length - 1] + "." + suffix ) );

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
    prepareObjects( m_id2gemessene, "Gemessene" );
    prepareObjects( m_id2vorhergesagte, "Vorhergesagte" );
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

    if( typespec == TYPE_MEASUREMENT )
      return (ObjectInfo[]) m_id2gemessene.values().toArray( new ObjectInfo[0] );
    else if( typespec == TYPE_VALUE )
      return (ObjectInfo[]) m_id2vorhergesagte.values().toArray(
          new ObjectInfo[0] );
    else
      return new ObjectInfo[0];
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

    if( m_id2gemessene.containsKey( id ) || m_id2vorhergesagte.containsKey( id ) )
    {
      // overriden?
      if( m_id2values.containsKey( id ) )
        return (ArchiveData[]) m_id2values.get( id );

      // generate random data
      return randomData( from, to );
    }

    return new ArchiveData[0];
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