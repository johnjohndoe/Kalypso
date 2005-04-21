package org.kalypso.wiskiadapter;

import java.rmi.RemoteException;
import java.sql.Timestamp;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.IObservationListener;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.event.ObservationEventAdapter;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.timeseries.wq.WQException;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTable;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableFactory;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableSet;
import org.kalypso.util.conversion.units.SIConverter;
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.util.runtime.args.DateRangeArgument;
import org.kalypso.util.xml.xlink.IXlink;

import de.kisters.wiski.webdataprovider.common.net.KiWWDataProviderInterface;
import de.kisters.wiski.webdataprovider.common.util.KiWWException;

/**
 * WiskiTimeserie
 * 
 * @author schlienger
 */
public class WiskiTimeserie implements IObservation
{
  private final TsInfoItem m_tsinfo;

  private final ObservationEventAdapter m_evtPrv = new ObservationEventAdapter(
      this );

  private final IAxis[] m_axes;

  private final SIConverter m_cv;

  private MetadataList m_metadata = null;

  public WiskiTimeserie( final TsInfoItem tsinfo )
  {
    m_tsinfo = tsinfo;

    m_axes = new IAxis[3];
    m_axes[0] = new DefaultAxis( TimeserieUtils
        .getName( TimeserieConstants.TYPE_DATE ), TimeserieConstants.TYPE_DATE,
        TimeserieUtils.getUnit( TimeserieConstants.TYPE_DATE ), Date.class,
        true );

    final String wiskiType = tsinfo.getWiskiType();
    final String kalypsoType = WiskiUtils.wiskiType2Kalypso( wiskiType );
    final String wiskiUnit = tsinfo.getWiskiUnit();
    final String kalypsoUnit = TimeserieUtils.getUnit( kalypsoType );

    m_cv = new SIConverter( wiskiUnit, kalypsoUnit );

    m_axes[1] = new DefaultAxis( TimeserieUtils.getName( kalypsoType ),
        kalypsoType, kalypsoUnit, Double.class, false );

    m_axes[2] = KalypsoStatusUtils.createStatusAxisFor( m_axes[1] );
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getIdentifier()
   */
  public String getIdentifier( )
  {
    return m_tsinfo.getIdentifier();
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getName()
   */
  public String getName( )
  {
    return m_tsinfo.getName();
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#isEditable()
   */
  public boolean isEditable( )
  {
    // TODO only a prognose is editable
    return false;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getTarget()
   */
  public IXlink getTarget( )
  {
    return null;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getMetadataList()
   */
  public MetadataList getMetadataList( )
  {
    if( m_metadata == null )
    {
      m_metadata = new MetadataList();

      try
      {
        fetchAlarmLevels( m_metadata );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }

      // note: WQ-Table is only incorporated into metadata
      // when calling getValues()
    }

    return m_metadata;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getAxisList()
   */
  public IAxis[] getAxisList( )
  {
    return m_axes;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getValues(org.kalypso.util.runtime.IVariableArguments)
   */
  public ITuppleModel getValues( final IVariableArguments args )
      throws SensorException
  {
    final DateRangeArgument dr;

    try
    {
      // tricky: when no date range specified, we create a default one
      if( args == null || !(args instanceof DateRangeArgument) )
        dr = DateRangeArgument.createFromPastDays( Integer.valueOf(
            WiskiUtils.getProperties().getProperty(
                WiskiUtils.PROP_NUMBER_OF_DAYS, "7" ) ).intValue() );
      else
        dr = (DateRangeArgument) args;

      //  data range
      final Timestamp from = new Timestamp( dr.getFrom().getTime() );
      final Timestamp to = new Timestamp( dr.getTo().getTime() );

      final WiskiRepository rep = (WiskiRepository) m_tsinfo.getRepository();

      //getTsData for specified timeseries (or ts list)
      final HashMap gettsdata = rep.getWiski().getTsData( rep.getUserData(),
          new long[] { Long.parseLong( m_tsinfo.getWiskiId() ) }, from, to,
          null );

      final HashMap series = (HashMap) gettsdata
          .get( KiWWDataProviderInterface.KEY_TIMESERIES );
      final HashMap serie = (HashMap) series.get( m_tsinfo.getWiskiId() );
      if( serie == null )
        return SimpleTuppleModel.EMPTY_TUPPLEMODEL;
      final LinkedList data = (LinkedList) serie
          .get( KiWWDataProviderInterface.KEY_TSDATA );

      return new WiskiTuppleModel( getAxisList(), data, m_cv );
    }
    catch( Exception e ) // RepositoryException, RemoteException, KiWWException
    {
      e.printStackTrace();
      throw new SensorException( e );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#setValues(org.kalypso.ogc.sensor.ITuppleModel)
   */
  public void setValues( ITuppleModel values ) throws SensorException
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getHref()
   */
  public String getHref( )
  {
    // no meaning here
    return null;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationEventProvider#addListener(org.kalypso.ogc.sensor.IObservationListener)
   */
  public void addListener( IObservationListener listener )
  {
    m_evtPrv.addListener( listener );
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationEventProvider#removeListener(org.kalypso.ogc.sensor.IObservationListener)
   */
  public void removeListener( IObservationListener listener )
  {
    m_evtPrv.removeListener( listener );
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationEventProvider#clearListeners()
   */
  public void clearListeners( )
  {
    m_evtPrv.clearListeners();
  }

  /**
   * Helper for translating Wiski Rating-Tables into Kalypso Metadata
   * 
   * @param metadata
   * @param from
   * @param to
   * @throws NumberFormatException
   * @throws RemoteException
   * @throws KiWWException
   * @throws WQException
   */
  private void fetchWQTable( final MetadataList metadata, final Date from,
      final Date to ) throws NumberFormatException, RemoteException,
      KiWWException, WQException
  {
    final WiskiRepository rep = (WiskiRepository) m_tsinfo.getRepository();

    // TODO WQ-Table holen, Frage: welche Datum muss man mitgeben?
    final HashMap tables = rep.getWiski().getRatingTables( rep.getUserData(),
        KiWWDataProviderInterface.OBJECT_TIMESERIES,
        new Long[] { new Long( m_tsinfo.getWiskiId() ) },
        new Timestamp( to.getTime() ) );

    final HashMap table = (HashMap) tables.get( m_tsinfo.getWiskiId() );

    final String[] strW = ((String) table.get( "curve_table_stage" ))
        .split( "," );
    final String[] strQ = ((String) table.get( "curve_table_flow" ))
        .split( "," );

    if( strW.length != strQ.length )
      throw new IllegalArgumentException(
          "Anzahl von W-Werte und Q-Werte ist nicht gleich" );

    final double[] W = new double[strW.length];
    final double[] Q = new double[strW.length];
    for( int i = 0; i < strW.length; i++ )
    {
      W[i] = Double.parseDouble( strW[i] );
      Q[i] = Double.parseDouble( strQ[i] );
    }

    final WQTable wqt = new WQTable( from, W, Q );
    final WQTableSet set = new WQTableSet( new WQTable[] { wqt } );
    final String xml = WQTableFactory.createXMLString( set );

    metadata.setProperty( TimeserieConstants.MD_WQTABLE, xml );
  }

  /**
   * Helper for translating wiski alarm levels into kalypso metadata
   * 
   * @throws KiWWException
   * @throws RemoteException
   * @throws NumberFormatException
   */
  private void fetchAlarmLevels( final MetadataList md ) throws NumberFormatException, RemoteException, KiWWException
  {
    final WiskiRepository rep = (WiskiRepository) m_tsinfo.getRepository();

    final HashMap alarmlevel = rep.getWiski().getAlarmLevelList(
        rep.getUserData(),
        new Long[] { new Long( Long.parseLong( m_tsinfo.getWiskiId() ) ) },
        null );

    final List al = (List) alarmlevel.get( m_tsinfo.getWiskiId() );
    if( al != null )
    {
      for( final Iterator it = al.iterator(); it.hasNext(); )
      {
        final HashMap map = (HashMap) it.next();

        final String level = (String) map.get( "epv_name" );
        final String value = (String) map.get( "epv_value" );

        md.put( WiskiUtils.wiskiMetadataName2Kalypso( level ), value );
      }
    }
  }
}
