package de.psi.go.lhwz;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.swing.JOptionPane;

/**
 * Test Implementierung von der PSICompact Schnitstelle.
 * 
 * @author schlienger
 */
public class PSICompactImpl implements PSICompact
{
  private final String m_replicationDir;

  private final Map m_id2gemessene;

  private final Map m_id2vorhergesagte;

  private final Map m_id2values;

  private final Map m_id2measType;

  private final Map m_id2wq;

  private final Map m_id2zml;

  private boolean m_init = false;

  public PSICompactImpl( )
  {
    super();

    m_id2values = new HashMap();
    m_id2gemessene = new HashMap();
    m_id2vorhergesagte = new HashMap();
    m_id2measType = new HashMap();
    m_id2wq = new HashMap();
    m_id2zml = new HashMap();

    m_replicationDir = System.getProperty( "java.io.tmpdir" );
    
    prepareObjects( m_id2gemessene, "m" );
    prepareObjects( m_id2vorhergesagte, "v" );
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#init()
   */
  public void init( ) throws ECommException
  {
    m_init = true;

    final InputStream ins = getClass().getResourceAsStream( "lhwz-ids.csv" );

    try
    {
      final BufferedReader reader = new BufferedReader( new InputStreamReader(
          ins ) );

      int l = 1;
      String line = reader.readLine();

      while( line != null )
      {
        final String[] splits = line.split( ";" );

        if( splits.length < 2 )
          throw new ECommException(
              "Konnte Kennzeichendatei nicht vollständig einlesen! Letzte Zeile ist: "
                  + line + " NR:" + l );

        m_id2zml.put( splits[0], splits[1] );

        line = reader.readLine();
        l++;
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
        ins.close();
      }
      catch( IOException e1 )
      {
        e1.printStackTrace();
      }
    }
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

    if( m_id2gemessene.containsKey( id ) )
    {
      ArchiveData[] data = (ArchiveData[]) m_id2values.get( id );

      if( data == null )
      {
        data = randomData();

        m_id2values.put( id, data );
      }

      return data;
    }
    else if( m_id2vorhergesagte.containsKey( id ) )
    {
      ArchiveData[] data = (ArchiveData[]) m_id2values.get( id );

      if( data == null )
      {
        data = new ArchiveData[0];

        m_id2values.put( id, data );
      }

      return data;
    }

    else
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

    return null;
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getUserRights(java.lang.String,
   *      java.lang.String)
   */
  public String[] getUserRights( String userId, String userClass )
      throws ECommException
  {
    testInitDone();

    return null;
  }

  /**
   * Helper: erzeugt eine Zeitreihe
   * 
   * @return random data
   */
  private final ArchiveData[] randomData( )
  {
    int size = 200;

    ArchiveData[] data = new ArchiveData[size];

    Calendar cal = Calendar.getInstance();
    cal.set( Calendar.YEAR, 1998 );

    for( int i = 0; i < data.length; i++ )
    {
      data[i] = new ArchiveData( cal.getTime(), PSICompact.STATUS_AUTO, Math
          .random() * 100 );

      cal.add( Calendar.DAY_OF_YEAR, 1 );
    }

    return data;
  }

  /**
   * simuliert einer Liste von PSI Objekte
   */
  private final void prepareObjects( Map map, String suffix )
  {
    String id = "PSI-ROOT";
    map.put( id, new ObjectInfo( id, "PSI-Compact" ) );

    id = "PSI-ROOT.PEGEL_1." + suffix;
    map.put( id, new ObjectInfo( id, "Pegel1" + suffix ) );

    id = "PSI-ROOT.PEGEL_2." + suffix;
    map.put( id, new ObjectInfo( id, "Pegel2" + suffix ) );

    id = "PSI-ROOT.LEVEL_1";
    map.put( id, new ObjectInfo( id, "Level1" ) );

    id = "PSI-ROOT.LEVEL_1.PEGEL_11." + suffix;
    map.put( id, new ObjectInfo( id, "Pegel11" + suffix ) );

    id = "PSI-ROOT.LEVEL_1.PEGEL_12." + suffix;
    map.put( id, new ObjectInfo( id, "Pegel12" + suffix ) );

    id = "PSI-ROOT.LEVEL_1.SUBLEVEL_1";
    map.put( id, new ObjectInfo( id, "Sublevel1" ) );

    id = "PSI-ROOT.LEVEL_1.SUBLEVEL_1.PEGEL_111." + suffix;
    map.put( id, new ObjectInfo( id, "Pegel111" + suffix ) );

    id = "PSI-ROOT.LEVEL_1.SUBLEVEL_1.PEGEL_112." + suffix;
    map.put( id, new ObjectInfo( id, "Pegel112" + suffix ) );

    id = "PSI-ROOT.LEVEL_2";
    map.put( id, new ObjectInfo( id, "Level2" ) );
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

    return 1;
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getUser(java.lang.String)
   */
  public String[] getUser( String userClass ) throws ECommException
  {
    testInitDone();

    return new String[] { "User1", "User2", "User3", "User4" };
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
   * @see de.psi.go.lhwz.PSICompact#getComment(java.lang.String)
   */
  public String getComment( String Pegelkennziffer ) throws ECommException
  {
    testInitDone();

    return "Kommentar...";
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getGewaesser(java.lang.String)
   */
  public String getGewaesser( String Pegelkennziffer ) throws ECommException
  {
    testInitDone();

    return "Gewaesser" + Math.random();
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getFlussgebiet(java.lang.String)
   */
  public String getFlussgebiet( String Pegelkennziffer ) throws ECommException
  {
    testInitDone();

    return "Flussgebiet" + Math.random();
  }
}