package org.kalypso.psiadapter;

import java.text.SimpleDateFormat;
import java.util.Hashtable;
import java.util.Map;
import java.util.Properties;

import org.kalypso.java.reflect.ClassUtilities;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.util.repository.RepositoryException;

import de.psi.go.lhwz.PSICompact;
import de.psi.go.lhwz.PSICompact.WQData;
import de.psi.go.lhwz.PSICompact.WQParamSet;

/**
 * The entry point to the PSICompact interface from PSI.
 * 
 * @author schlienger
 */
public final class PSICompactFactory
{
  private final static SimpleDateFormat m_sdf = new SimpleDateFormat( "dd.MM.yyyy HH.mm.ss" );

  private static String PSI_CLASS = null;

  protected static PSICompact m_psiCompact = null;

  protected static PSICompactRepository m_psiCompactRep = null;

  protected static int m_currentVersion = 0;

  private static VersionChecker m_threadVersionChecker = null;

  private static Properties m_factoryProperties = null;

  private final static Map m_axes = new Hashtable();

  private final static Integer m_zero = new Integer( 0 );

  /**
   * Returns the connection to the PSI-Interface implementation. This method is
   * only visible to the package.
   */
  static PSICompact getConnection()
  {
    if( m_psiCompact == null )
    {
      try
      {
        m_factoryProperties = new Properties();
        m_factoryProperties.load( PSICompactFactory.class
            .getResourceAsStream( "resources/config.ini" ) );

        // path of class which implements the PSICompact interface
        PSI_CLASS = m_factoryProperties.getProperty( "PSI_CLASS", "de.psi.go.lhwz.PSICompactImpl" );

        m_psiCompact = (PSICompact)ClassUtilities.newInstance( PSI_CLASS, PSICompact.class,
            PSICompactFactory.class.getClassLoader() );

        // Wichtig! init() aufrufen damit die PSI-Schnittstelle sich
        // initialisieren kann
        m_psiCompact.init();

        m_currentVersion = m_psiCompact.getDataModelVersion();
      }
      catch( Exception e )
      {
        e.printStackTrace();

        return null;
      }
    }

    return m_psiCompact;
  }

  /**
   * Liefert den PSICompactRepository
   * 
   * @throws RepositoryException
   */
  static PSICompactRepository getRepository() throws RepositoryException
  {
    if( m_psiCompactRep == null )
    {
      // TODO: specify location of the service here
      m_psiCompactRep = new PSICompactRepository( "PSICompact@localhost" );

      // TODO siehe Kommentar in VersionChecker Klasse
      // m_threadVersionChecker = new VersionChecker();
      // m_threadVersionChecker.start();
    }

    return m_psiCompactRep;
  }

  public static void dispose()
  {
    if( m_threadVersionChecker != null )
      m_threadVersionChecker.cancel();
  }

  /**
   * Helper that translates the measure type into a string label.
   */
  public final static String measureTypeToString( int measType )
  {
    switch( measType )
    {
    case PSICompact.MEAS_FLOW:
      return m_factoryProperties.getProperty( "MEAS_FLOW" );
    case PSICompact.MEAS_LEVEL:
      return m_factoryProperties.getProperty( "MEAS_LEVEL" );
    case PSICompact.MEAS_RAINFALL:
      return m_factoryProperties.getProperty( "MEAS_RAINFALL" );
    case PSICompact.MEAS_TEMPERATUR:
      return m_factoryProperties.getProperty( "MEAS_TEMPERATUR" );
    case PSICompact.MEAS_UNDEF:
      return m_factoryProperties.getProperty( "MEAS_UNDEF" );
    default:
      return m_factoryProperties.getProperty( "UNKNOWN" );
    }
  }

  /**
   * Helper für die Übersetzung des valueType in eine leesbare String
   */
  public final static String valueTypeToString( int valueType )
  {
    switch( valueType )
    {
    case PSICompact.TYPE_MEASUREMENT:
      return m_factoryProperties.getProperty( "TYPE_MEASUREMENT" );
    case PSICompact.TYPE_VALUE:
      return m_factoryProperties.getProperty( "TYPE_VALUE" );
    case PSICompact.TYPE_UNDEF:
      return m_factoryProperties.getProperty( "TYPE_UNDEF" );
    default:
      return m_factoryProperties.getProperty( "UNKNOWN" );
    }
  }

  /**
   * Helper für die Übersetzung des 'Unit' (ObjectMetaData) in eine leesbare
   * String
   */
  public final static String unitToString( int unit )
  {
    switch( unit )
    {
    case PSICompact.SI_CUBIC_METER_PER_SECOND:
      return m_factoryProperties.getProperty( "SI_CUBIC_METER_PER_SECOND" );
    case PSICompact.SI_KELVIN:
      return m_factoryProperties.getProperty( "SI_KELVIN" );
    case PSICompact.SI_METER:
      return m_factoryProperties.getProperty( "SI_METER" );
    case PSICompact.SI_QUBIC_METER:
      return m_factoryProperties.getProperty( "SI_QUBIC_METER" );
    case PSICompact.SI_NO_UNIT:
      return m_factoryProperties.getProperty( "SI_NO_UNIT" );
    case PSICompact.SI_UNDEF:
      return m_factoryProperties.getProperty( "SI_UNDEF" );
    default:
      return m_factoryProperties.getProperty( "UNKNOWN" );
    }
  }

  /**
   * Helper für die Übersetzung des 'Status' (ArchiveData) in eine leesbare
   * String
   */
  public final static String statusToString( int status )
  {
    switch( status )
    {
    case PSICompact.STATUS_AUTO:
      return m_factoryProperties.getProperty( "STATUS_AUTO" );
    case PSICompact.STATUS_ERSALLG:
      return m_factoryProperties.getProperty( "STATUS_ERSALLG" );
    case PSICompact.STATUS_MANKOR:
      return m_factoryProperties.getProperty( "STATUS_MANKOR" );
    case PSICompact.STATUS_NACH:
      return m_factoryProperties.getProperty( "STATUS_NACH" );
    case PSICompact.STATUS_NORM:
      return m_factoryProperties.getProperty( "STATUS_NORM" );
    case PSICompact.STATUS_NORMALLG:
      return m_factoryProperties.getProperty( "STATUS_NORMALLG" );
    case PSICompact.STATUS_OK:
      return m_factoryProperties.getProperty( "STATUS_OK" );
    case PSICompact.STATUS_REKO:
      return m_factoryProperties.getProperty( "STATUS_REKO" );
    case PSICompact.STATUS_UNDEF:
      return m_factoryProperties.getProperty( "STATUS_UNDEF" );

    default:
      return m_factoryProperties.getProperty( "UNKNOWN" );
    }
  }

  /**
   * Converts the PSICompact-Status to the Kalypso internal BitMask.
   * 
   * @param status
   *          as delivered by PSICompact
   * @return an integer representing a bitmask.
   */
  public final static Integer statusToMask( int status )
  {
    switch( status )
    {
    case PSICompact.STATUS_AUTO:
      return Integer.valueOf( m_factoryProperties.getProperty( "BM_" + "STATUS_AUTO" ) );
    case PSICompact.STATUS_ERSALLG:
      return Integer.valueOf( m_factoryProperties.getProperty( "BM_" + "STATUS_ERSALLG" ) );
    case PSICompact.STATUS_MANKOR:
      return Integer.valueOf( m_factoryProperties.getProperty( "BM_" + "STATUS_MANKOR" ) );
    case PSICompact.STATUS_NACH:
      return Integer.valueOf( m_factoryProperties.getProperty( "BM_" + "STATUS_NACH" ) );
    case PSICompact.STATUS_NORM:
      return Integer.valueOf( m_factoryProperties.getProperty( "BM_" + "STATUS_NORM" ) );
    case PSICompact.STATUS_NORMALLG:
      return Integer.valueOf( m_factoryProperties.getProperty( "BM_" + "STATUS_NORMALLG" ) );
    case PSICompact.STATUS_OK:
      return Integer.valueOf( m_factoryProperties.getProperty( "BM_" + "STATUS_OK" ) );
    case PSICompact.STATUS_REKO:
      return Integer.valueOf( m_factoryProperties.getProperty( "BM_" + "STATUS_REKO" ) );
    case PSICompact.STATUS_UNDEF:
      return Integer.valueOf( m_factoryProperties.getProperty( "BM_" + "STATUS_UNDEF" ) );

    default:
      return m_zero;
    }
  }

  /**
   * Helper that translates the status string back to an integer
   */
  public static int statusTranslate( String status )
  {
    if( status.equals( m_factoryProperties.getProperty( "STATUS_AUTO" ) ) )
      return PSICompact.STATUS_AUTO;
    else if( status.equals( m_factoryProperties.getProperty( "STATUS_ERSALLG" ) ) )
      return PSICompact.STATUS_ERSALLG;
    else if( status.equals( m_factoryProperties.getProperty( "STATUS_MANKOR" ) ) )
      return PSICompact.STATUS_MANKOR;
    else if( status.equals( m_factoryProperties.getProperty( "STATUS_NACH" ) ) )
      return PSICompact.STATUS_NACH;
    else if( status.equals( m_factoryProperties.getProperty( "STATUS_NORM" ) ) )
      return PSICompact.STATUS_NORM;
    else if( status.equals( m_factoryProperties.getProperty( "STATUS_NORMALLG" ) ) )
      return PSICompact.STATUS_NORMALLG;
    else if( status.equals( m_factoryProperties.getProperty( "STATUS_OK" ) ) )
      return PSICompact.STATUS_OK;
    else if( status.equals( m_factoryProperties.getProperty( "STATUS_REKO" ) ) )
      return PSICompact.STATUS_REKO;
    else
      return PSICompact.STATUS_UNDEF;
  }

  /**
   * TODO: vielleicht kein Thread, dafür aber auf jede Struktur Abfrage. Jede
   * Client soll sich dann die Zeit der letzte aktuelle Version merken und wenn
   * nicht gleich wie Server, dann Struktur neu darstellen.
   * 
   * Internal version checker for the PSICompact Interface.
   * 
   * @author schlienger
   */
  private final static class VersionChecker extends Thread
  {
    private boolean m_cancelled = false;

    public boolean isCancelled()
    {
      return m_cancelled;
    }

    public void cancel( )
    {
      m_cancelled = true;
    }

    /**
     * @see java.lang.Runnable#run()
     */
    public void run()
    {
      try
      {
        while( !m_cancelled )
        {
          Thread.sleep( 1000 );

          int version = m_psiCompact.getDataModelVersion();

          if( version > m_currentVersion )
          {
            m_currentVersion = version;

            if( m_psiCompactRep != null )
              m_psiCompactRep.fireRepositoryStructureChanged();
          }
        }
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
  }

  /**
   * Factory method um Achsen zu erzeugen.
   * <p>
   */
  public static IAxis getAxis( final String label, final String type, final String unit, final Class dataClass,
      final int position )
  {
    final String key = label + unit + dataClass.getName();

    IAxis axis = (IAxis)m_axes.get( key );

    if( axis == null )
    {
      axis = new PSICompactAxis( label, type, unit, dataClass, false, position );

      m_axes.put( key, axis );
    }

    return axis;
  }

  /**
   * Helper that produces a string from the given WQParamSet array.
   */
  public static String wqParamSet2String( final WQParamSet[] pset )
  {
    final StringBuffer bf = new StringBuffer();

    for( int i = 0; i < pset.length; i++ )
    {
      bf.append( m_sdf.format( pset[i].getValidFrom() ) ).append( '#' );

      final WQData[] ds = pset[i].getWqData();
      for( int j = 0; j < ds.length; j++ )
      {
        // WGR: obere Wasserstandsgrenze in cm
        // W1: Konstante W1
        // LNK1: Konstante LNK1
        // K2: Konstante K2

        bf.append( ds[j].getWGR() ).append( ';' ).append( ds[j].getW1() ).append( ';' ).append(
            ds[j].getLNK1() ).append( ';' ).append( ds[j].getK2() );

        if( j == ds.length - 1 )
          bf.append( " < " );
      }

      bf.append( " | " );
    }

    return bf.toString();
  }
}