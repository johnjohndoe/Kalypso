package de.psi.go.lhwz;

import java.io.File;
import java.util.Calendar;
import java.util.Date;
import java.util.Hashtable;
import java.util.Map;

import javax.swing.JOptionPane;

/**
 * Test Implementierung von der PSICompact Schnitstelle. 
 * 
 * @author schlienger
 */
public class PSICompactImpl implements PSICompact
{
	private static final String REPLICATION_BASEDIR = "C:/temp/psicompact/replication";
  
  private final Map m_id2gemessene;
	private final Map m_id2vorhergesagte;

	private final Map m_id2values;
	private final Map m_id2measType;

	public PSICompactImpl()
	{
		super();
		
		m_id2values = new Hashtable();
		m_id2gemessene = new Hashtable();
		m_id2vorhergesagte = new Hashtable();
		m_id2measType = new Hashtable();
		
		prepareObjects( m_id2gemessene, "m" );
		prepareObjects( m_id2vorhergesagte, "v" );
	}

	/**
	 * @see de.psi.go.lhwz.PSICompact#init()
	 */
	public void init()
	{
		System.out.println( "PCICompact.init() aufgerufen");
	}

	/**
	 * @see de.psi.go.lhwz.PSICompact#writeProtocol(java.lang.String)
	 */
	public boolean writeProtocol(String message)
	{
		JOptionPane.showMessageDialog( JOptionPane.getRootFrame(), message );
		
		return true;
	}

	/**
	 * @see de.psi.go.lhwz.PSICompact#getInfo(int)
	 */
	public ObjectInfo[] getInfo(int typespec)
	{
		if( typespec == TYPE_MEASUREMENT )
			return (ObjectInfo[]) m_id2gemessene.values().toArray( new ObjectInfo[0] );
		else if( typespec == TYPE_VALUE )
			return (ObjectInfo[]) m_id2vorhergesagte.values().toArray( new ObjectInfo[0] );
		else
			return new ObjectInfo[0];
	}

	/**
	 * @see de.psi.go.lhwz.PSICompact#getArchiveData(java.lang.String, int, java.util.Date, java.util.Date)
	 */
	public ArchiveData[] getArchiveData(String id, int arcType, Date from, Date to)
	{
		if( m_id2gemessene.containsKey( id ) )
		{
			ArchiveData[] data = (ArchiveData[]) m_id2values.get(id);
			
			if( data == null )
			{
				data = randomData();
		
				m_id2values.put( id, data );
			}
			
			return data;
		}
		else if( m_id2vorhergesagte.containsKey( id ) )
		{
			ArchiveData[] data = (ArchiveData[]) m_id2values.get(id);
			
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
	 * @see de.psi.go.lhwz.PSICompact#setArchiveData(java.lang.String, int, java.util.Date, de.psi.go.lhwz.PSICompact.ArchiveData[])
	 */
	public boolean setArchiveData(String id, int arcType, Date from, ArchiveData[] data)
	{
		m_id2values.put( id, data );
		
		return true;
	}

	/**
	 * @see de.psi.go.lhwz.PSICompact#getWQParams(java.lang.String)
	 */
	public WQParamSet[] getWQParams(String id)
  {
		return null;
	}

	/**
	 * @see de.psi.go.lhwz.PSICompact#getObjectMetaData(java.lang.String)
	 */
	public ObjectMetaData getObjectMetaData(String id)
	{
		return null;
	}

	/**
	 * @see de.psi.go.lhwz.PSICompact#getUserClasses(java.lang.String)
	 */
	public String[] getUserClasses(String userId)
	{
		return null;
	}

	/**
	 * @see de.psi.go.lhwz.PSICompact#getUserRights(java.lang.String, java.lang.String)
	 */
	public String[] getUserRights(String userId, String userClass)
	{
		return null;
	}

	/**
	 * Helper: erzeugt eine Zeitreihe
	 * 
	 * @return
	 */
	private final ArchiveData[] randomData()
	{
		int size = 200;
		
		ArchiveData[] data = new ArchiveData[ size ];
		
		Calendar cal = Calendar.getInstance();
		cal.set( Calendar.YEAR, 1998 );
				
		for (int i = 0; i < data.length; i++)
		{
			data[i] = new ArchiveData( cal.getTime(), PSICompact.STATUS_AUTO, Math.random() * 100 );
			
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
		map.put( id, new ObjectInfo( id, "PSI-Compact") );
		
		id = "PSI-ROOT.PEGEL_1." + suffix;
		map.put( id, new ObjectInfo( id, "Pegel1" + suffix) );
		
		id = "PSI-ROOT.PEGEL_2." + suffix;
		map.put( id, new ObjectInfo( id, "Pegel2" + suffix) );
		
		id = "PSI-ROOT.LEVEL_1";
		map.put( id, new ObjectInfo( id, "Level1") );
		
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
	public int getMeasureType( String id )
	{
		Integer intObj = (Integer) m_id2measType.get(id);
		
		if( intObj == null )
		{
			double d = Math.random();
			
			int enumMeas = 4;
			double interval = 1.0/enumMeas;
			
			for (int i = 0; i < enumMeas; i++)
				if( d <= i * interval )
				{
					intObj = new Integer( i );
					break;
				}
			
			if( intObj == null )
				intObj = new Integer( PSICompact.MEAS_UNDEF );
			
			m_id2measType.put(id, intObj);
		}
	
		return intObj.intValue();
	}

  /**
   * @see de.psi.go.lhwz.PSICompact#distributeFile(java.lang.String)
   */
  public boolean distributeFile( String filename )
  {
    System.out.println( "File <" + filename + "> wurde erfolgreich repliziert." );
    
    return true;
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#removeFile(java.lang.String)
   */
  public boolean removeFile( String filename )
  {
    File f = new File( REPLICATION_BASEDIR + File.separator + filename );
    
    return f.delete();
  }

  /**
   * @see de.psi.go.lhwz.PSICompact#getDataModelVersion()
   */
  public int getDataModelVersion()
  {
    return 1;
  }
}
