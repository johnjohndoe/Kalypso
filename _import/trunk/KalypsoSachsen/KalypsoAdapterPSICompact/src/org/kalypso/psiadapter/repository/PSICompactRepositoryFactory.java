package org.kalypso.psiadapter.repository;

import java.util.Properties;

import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.psiadapter.PSICompactFactory;
import org.kalypso.repository.AbstractRepositoryFactory;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.RepositoryException;

import de.psi.go.lhwz.PSICompact;

/**
 * @author schlienger
 */
public class PSICompactRepositoryFactory extends AbstractRepositoryFactory
{
  protected static PSICompactRepository m_psiCompactRep = null; // protected for
                                                                // access in
                                                                // thread

  protected static int m_currentVersion = 0;

  private static VersionChecker m_threadVersionChecker = null; // protected for
                                                               // access in
                                                               // thread

  private final static Integer m_zero = new Integer( 0 );

  /**
   * Does nothing.
   * 
   * @see org.kalypso.repository.IRepositoryFactory#configureRepository()
   */
  public boolean configureRepository( )
  {
    return true;
  }

  /**
   * @throws RepositoryException
   * @see org.kalypso.repository.IRepositoryFactory#createRepository()
   */
  public IRepository createRepository( ) throws RepositoryException
  {
    return getRepository();
  }

  /**
   * Liefert den PSICompactRepository
   * 
   * @return repository
   * 
   * @throws RepositoryException
   */
  public final static PSICompactRepository getRepository( )
      throws RepositoryException
  {
    if( m_psiCompactRep == null )
    {
      // PSICompact Repository is always in read/write mode
      m_psiCompactRep = new PSICompactRepository(
          new PSICompactRepositoryFactory(), false );

      // TODO siehe Kommentar in VersionChecker Klasse
      // m_threadVersionChecker = new VersionChecker();
      // m_threadVersionChecker.start();
    }

    return m_psiCompactRep;
  }

  /**
   * Converts the PSICompact-Status to the Kalypso internal BitMask.
   * 
   * @param status
   *          as delivered by PSICompact
   * @return an integer representing a bitmask.
   */
  public final static Integer psiStatusToMask( int status )
  {
    final Properties props = PSICompactFactory.getProperties();

    switch( status )
    {
      case PSICompact.STATUS_AUTO:
        return Integer.valueOf( props.getProperty( "BM_" + "STATUS_AUTO" ) );
      case PSICompact.STATUS_ERSALLG:
        return Integer.valueOf( props.getProperty( "BM_" + "STATUS_ERSALLG" ) );
      case PSICompact.STATUS_MANKOR:
        return Integer.valueOf( props.getProperty( "BM_" + "STATUS_MANKOR" ) );
      case PSICompact.STATUS_NACH:
        return Integer.valueOf( props.getProperty( "BM_" + "STATUS_NACH" ) );
      case PSICompact.STATUS_NORM:
        return Integer.valueOf( props.getProperty( "BM_" + "STATUS_NORM" ) );
      case PSICompact.STATUS_NORMALLG:
        return Integer.valueOf( props.getProperty( "BM_" + "STATUS_NORMALLG" ) );
      case PSICompact.STATUS_OK:
        return Integer.valueOf( props.getProperty( "BM_" + "STATUS_OK" ) );
      case PSICompact.STATUS_REKO:
        return Integer.valueOf( props.getProperty( "BM_" + "STATUS_REKO" ) );
      case PSICompact.STATUS_UNDEF:
        return Integer.valueOf( props.getProperty( "BM_" + "STATUS_UNDEF" ) );

      default:
        return m_zero;
    }
  }

  /**
   * Converts bitMask (Kalypso Status) to PSI-Status
   * 
   * @param mask
   * @return PSICompact-Status
   */
  public final static int maskToPsiStatus( int mask )
  {
    if( KalypsoStatusUtils.checkMask( mask,
        KalypsoStatusUtils.BIT_USER_MODIFIED ) )
      return PSICompact.STATUS_MANKOR;

    return PSICompact.STATUS_AUTO;
  }

  /**
   * Helper that translates the measure type into a string label.
   * 
   * @param measType
   * @return label
   */
  public final static String measureTypeToString( int measType )
  {
    final Properties props = PSICompactFactory.getProperties();

    switch( measType )
    {
      case PSICompact.MEAS_FLOW:
        return props.getProperty( "MEAS_FLOW" );
      case PSICompact.MEAS_LEVEL:
        return props.getProperty( "MEAS_LEVEL" );
      case PSICompact.MEAS_RAINFALL:
        return props.getProperty( "MEAS_RAINFALL" );
      case PSICompact.MEAS_TEMPERATUR:
        return props.getProperty( "MEAS_TEMPERATUR" );
      case PSICompact.MEAS_UNDEF:
        return props.getProperty( "MEAS_UNDEF" );
      default:
        return props.getProperty( "UNKNOWN" );
    }
  }

  /**
   * Helper that returns the appropriate archive type depending on the 
   * measure type.
   * 
   * @param measType
   * @return archive type
   */
  public final static int measureTypeToArchiveType( int measType )
  {
    switch( measType )
    {
      case PSICompact.MEAS_RAINFALL:
        return PSICompact.ARC_HOUR;
      default:
        return PSICompact.ARC_MIN15;
    }
  }
  
  /**
   * Helper für die Übersetzung des valueType in eine leesbare String
   * 
   * @param valueType
   * @return string
   */
  public final static String valueTypeToString( int valueType )
  {
    final Properties props = PSICompactFactory.getProperties();

    switch( valueType )
    {
      case PSICompact.TYPE_MEASUREMENT:
        return props.getProperty( "TYPE_MEASUREMENT" );
      case PSICompact.TYPE_VALUE:
        return props.getProperty( "TYPE_VALUE" );
      case PSICompact.TYPE_UNDEF:
        return props.getProperty( "TYPE_UNDEF" );
      default:
        return props.getProperty( "UNKNOWN" );
    }
  }

  /**
   * Helper für die Übersetzung des 'Unit' (ObjectMetaData) in eine leesbare
   * String
   * 
   * @param unit
   * @return string
   */
  public final static String unitToString( int unit )
  {
    final Properties props = PSICompactFactory.getProperties();

    switch( unit )
    {
      case PSICompact.SI_CUBIC_METER_PER_SECOND:
        return props.getProperty( "SI_CUBIC_METER_PER_SECOND" );
      case PSICompact.SI_KELVIN:
        return props.getProperty( "SI_KELVIN" );
      case PSICompact.SI_METER:
        return props.getProperty( "SI_METER" );
      case PSICompact.SI_QUBIC_METER:
        return props.getProperty( "SI_QUBIC_METER" );
      case PSICompact.SI_NO_UNIT:
        return props.getProperty( "SI_NO_UNIT" );
      case PSICompact.SI_UNDEF:
        return props.getProperty( "SI_UNDEF" );
      default:
        return props.getProperty( "UNKNOWN" );
    }
  }

  //  /**
  //   * Helper für die Übersetzung des 'Status' (ArchiveData) in eine leesbare
  //   * String
  //   */
  //  public final static String statusToString( int status )
  //  {
  //    switch( status )
  //    {
  //    case PSICompact.STATUS_AUTO:
  //      return props.getProperty( "STATUS_AUTO" );
  //    case PSICompact.STATUS_ERSALLG:
  //      return props.getProperty( "STATUS_ERSALLG" );
  //    case PSICompact.STATUS_MANKOR:
  //      return props.getProperty( "STATUS_MANKOR" );
  //    case PSICompact.STATUS_NACH:
  //      return props.getProperty( "STATUS_NACH" );
  //    case PSICompact.STATUS_NORM:
  //      return props.getProperty( "STATUS_NORM" );
  //    case PSICompact.STATUS_NORMALLG:
  //      return props.getProperty( "STATUS_NORMALLG" );
  //    case PSICompact.STATUS_OK:
  //      return props.getProperty( "STATUS_OK" );
  //    case PSICompact.STATUS_REKO:
  //      return props.getProperty( "STATUS_REKO" );
  //    case PSICompact.STATUS_UNDEF:
  //      return props.getProperty( "STATUS_UNDEF" );
  //
  //    default:
  //      return props.getProperty( "UNKNOWN" );
  //    }
  //  }

  //  /**
  //   * Helper that translates the status string back to an integer
  //   */
  //  public static int statusTranslate( final String status )
  //  {
  //    if( status.equals( props.getProperty( "STATUS_AUTO" ) ) )
  //      return PSICompact.STATUS_AUTO;
  //    else if( status.equals( props.getProperty( "STATUS_ERSALLG" ) ) )
  //      return PSICompact.STATUS_ERSALLG;
  //    else if( status.equals( props.getProperty( "STATUS_MANKOR" ) ) )
  //      return PSICompact.STATUS_MANKOR;
  //    else if( status.equals( props.getProperty( "STATUS_NACH" ) ) )
  //      return PSICompact.STATUS_NACH;
  //    else if( status.equals( props.getProperty( "STATUS_NORM" ) ) )
  //      return PSICompact.STATUS_NORM;
  //    else if( status.equals( props.getProperty( "STATUS_NORMALLG" ) ) )
  //      return PSICompact.STATUS_NORMALLG;
  //    else if( status.equals( props.getProperty( "STATUS_OK" ) ) )
  //      return PSICompact.STATUS_OK;
  //    else if( status.equals( props.getProperty( "STATUS_REKO" ) ) )
  //      return PSICompact.STATUS_REKO;
  //    else
  //      return PSICompact.STATUS_UNDEF;
  //  }

  public static void dispose( )
  {
    if( m_threadVersionChecker != null )
      m_threadVersionChecker.cancel();
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

    public boolean isCancelled( )
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
    public void run( )
    {
      try
      {
        while( !m_cancelled )
        {
          Thread.sleep( 1000 );

          int version = PSICompactFactory.getConnection().getDataModelVersion();

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
}