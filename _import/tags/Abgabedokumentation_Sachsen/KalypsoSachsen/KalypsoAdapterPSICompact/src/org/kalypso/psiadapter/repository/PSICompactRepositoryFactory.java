package org.kalypso.psiadapter.repository;

import java.util.Properties;

import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannGroup;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannParams;
import org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannSet;
import org.kalypso.psiadapter.PSICompactFactory;
import org.kalypso.psiadapter.repository.conversion.IValueConverter;
import org.kalypso.psiadapter.repository.conversion.KelvinCelsiusConverter;
import org.kalypso.psiadapter.repository.conversion.NoConverter;
import org.kalypso.psiadapter.repository.conversion.SIConverter;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.RepositoryException;
import org.kalypso.repository.factory.AbstractRepositoryFactory;

import de.psi.go.lhwz.PSICompact;
import de.psi.go.lhwz.PSICompact.WQData;
import de.psi.go.lhwz.PSICompact.WQParamSet;

/**
 * @author schlienger
 */
public class PSICompactRepositoryFactory extends AbstractRepositoryFactory
{
  private static PSICompactRepository m_psiCompactRep = null;

  private final static Integer m_zero = new Integer( 0 );

  /**
   * Does nothing.
   * 
   * @see org.kalypso.repository.factory.IRepositoryFactory#configureRepository()
   */
  public boolean configureRepository( )
  {
    return true;
  }

  /**
   * @see org.kalypso.repository.factory.IRepositoryFactory#createRepository()
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
      m_psiCompactRep = new PSICompactRepository( "PSICompact", false );
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
   * Converts bitMask (Kalypso Status) to PSI-Status. Always returns
   * <code>PSICompact.STATUS_AUTO</code>. PSICompact automatically
   * converts it to STATUS_REKO, independently from the status.
   * 
   * @param mask
   * @return PSICompact-Status
   */
  public final static int maskToPsiStatus( final int mask )
  {
//    if( KalypsoStatusUtils.checkMask( mask,
//        KalypsoStati.BIT_USER_MODIFIED ) )
//      return PSICompact.STATUS_MANKOR;

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
        return PSICompact.ARC_MIN15;
//      PSICompact.ARC_HOUR;
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
  private final static String unitToString( final int unit )
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
  
  /**
   * Creates the adequate converter between psi and kalypso units
   * 
   * @param psiUnit
   * @param kalypsoUnit
   * @return adequate converter
   */
  public final static IValueConverter getConverter( final int psiUnit, final String kalypsoUnit )
  {
    switch( psiUnit )
    {
      case PSICompact.SI_KELVIN:
        return KelvinCelsiusConverter.getInstance();
      
      case PSICompact.SI_CUBIC_METER_PER_SECOND:
      case PSICompact.SI_METER:
      case PSICompact.SI_QUBIC_METER:
      {
        final String strPsiUnit = unitToString( psiUnit );
      
        if( !strPsiUnit.equals( kalypsoUnit ) )
          return new SIConverter( strPsiUnit, kalypsoUnit );
      }
      
      case PSICompact.SI_NO_UNIT:
      case PSICompact.SI_UNDEF:
      default:
      {
        // empty
      }
    }
    
    return NoConverter.getInstance();
  }
  
  /**
   * Helper that converts PSICompact WQParamSet objects to a WechmannSets
   * object.
   * 
   * @param pset
   * @return WechmannGroup constructed from the WQParamSet array
   * 
   */
  public static WechmannGroup readWQParams( final WQParamSet[] pset )
  {
    final WechmannSet[] wsets = new WechmannSet[pset.length];
    for( int i = 0; i < pset.length; i++ )
    {
      final WQData[] ds = pset[i].getWqData();
      final WechmannParams[] wps = new WechmannParams[ds.length];

      for( int j = 0; j < ds.length; j++ )
        wps[j] = new WechmannParams( ds[j].getW1(), ds[j].getLNK1(), ds[j]
            .getK2(), ds[j].getWGR() );

      wsets[i] = new WechmannSet( pset[i].getValidFrom(), wps );
    }

    return new WechmannGroup( wsets );
  }
}