package org.kalypso.dcadapter;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.exception.CancelVisitorException;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IAxisRange;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.TupleModelDataSet;
import org.kalypso.ogc.sensor.impl.DefaultAxisRange;
import org.kalypso.ogc.sensor.impl.ITupleModelChangeListener;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.AxisUtils;
import org.kalypso.ogc.sensor.transaction.ITupleModelTransaction;
import org.kalypso.ogc.sensor.visitor.ITupleModelValueContainer;
import org.kalypso.ogc.sensor.visitor.ITupleModelVisitor;

import com.bce.datacenter.db.timeseries.TimeserieTupple;

/**
 * DataCenterTuppleModel
 *
 * @author marc
 */
public class DataCenterTuppleModel implements ITupleModel
{
  protected final TimeserieTupple[] m_tupples;

  private final IAxis[] m_axes;

  private final Map<IAxis, Integer> m_axesPos;

  public DataCenterTuppleModel( final TimeserieTupple[] tupples, final IAxis[] axes )
  {
    m_tupples = tupples;
    m_axes = axes;

    m_axesPos = new HashMap<>( axes.length );
    for( int i = 0; i < axes.length; i++ )
      m_axesPos.put( axes[i], new Integer( i ) );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getAxisList()
   */
  @Override
  public IAxis[] getAxes( )
  {
    return m_axes;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getCount()
   */
  @Override
  public int size( )
  {
    return m_tupples.length;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getRangeFor(org.kalypso.ogc.sensor.IAxis)
   */
  @Override
  public IAxisRange getRange( final IAxis axis ) throws SensorException
  {
    if( m_tupples.length == 0 )
      return null;

    switch( getPosition( axis ) )
    {
      case 0:
        return new DefaultAxisRange( m_tupples[0].getDate(), m_tupples[m_tupples.length - 1].getDate() );
      default:
        throw new SensorException( "Axis " + axis + " not supported for method getRangeFor() " ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getElement(int, org.kalypso.ogc.sensor.IAxis)
   */
  @Override
  public Object get( final int index, final IAxis axis ) throws SensorException
  {
    switch( getPosition( axis ) )
    {
      case 0:
        return m_tupples[index].getDate();
      case 1:
        return m_tupples[index].getValue();
      case 2:
        return m_tupples[index].getStatus();
      default:
        throw new SensorException( "Invalid axis position. Must be 0, 1 or 2." ); //$NON-NLS-1$
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#setElement(int, java.lang.Object, org.kalypso.ogc.sensor.IAxis)
   */
  @Override
  public void set( final int index, final IAxis axis, final Object element ) throws SensorException
  {
    switch( getPosition( axis ) )
    {
      case 0:
        m_tupples[index].setDate( (Date) element );
      case 1:
        m_tupples[index].setValue( (Double) element );
      case 2:
        m_tupples[index].setStatus( "x" ); // TODO richtigen Status ermitteln //$NON-NLS-1$
      default:
        throw new SensorException( "Invalid axis position. Must be 0, 1 or 2." ); //$NON-NLS-1$
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#indexOf(java.lang.Object, org.kalypso.ogc.sensor.IAxis)
   */
  @Override
  public int indexOf( final Object element, final IAxis axis )
  {
    if( m_axesPos.get( axis ).intValue() == 0 )
    {
      for( int i = 0; i < m_tupples.length; i++ )
        if( m_tupples[i].getDate().equals( element ) )
          return i;
    }

    return -1;
  }

  /**
   * Creates tupples according to the model
   */
  public static TimeserieTupple[] toTupples( final ITupleModel model ) throws SensorException
  {
    final IAxis[] axes = model.getAxes();

    final IAxis dateAxis = ObservationUtilities.findAxisByClass( axes, Date.class );
    final IAxis valueAxis = KalypsoStatusUtils.findAxisByClass( axes, Double.class, true );

    final TimeserieTupple[] tupples = new TimeserieTupple[model.size()];

    for( int i = 0; i < model.size(); i++ )
    {
      tupples[i].setDate( (Date) model.get( i, dateAxis ) );
      tupples[i].setValue( (Double) model.get( i, valueAxis ) );
      // TODO tupples[i].setStatus( (Double) model.getElement(i, valueAxis) );
    }

    return tupples;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getPositionFor(org.kalypso.ogc.sensor.IAxis)
   */
  @Override
  public int getPosition( final IAxis axis ) throws SensorException
  {
    if( m_axesPos.containsKey( axis ) )
      return m_axesPos.get( axis ).intValue();

    throw new SensorException( "The axis " + axis //$NON-NLS-1$
        + " is not part of this model" ); //$NON-NLS-1$
  }

  @Override
  public void accept( final ITupleModelVisitor visitor, final int direction ) throws SensorException
  {
    if( direction >= 0 )
    {
      for( int index = 0; index < m_tupples.length; index++ )
      {
        try
        {
          doVisit( visitor, index );
        }
        catch( final CancelVisitorException e )
        {
          return;
        }
      }
    }
    else
    {
      for( int index = m_tupples.length - 1; index >= 0; index-- )
      {
        try
        {
          doVisit( visitor, index );
        }
        catch( final CancelVisitorException e )
        {
          return;
        }
      }
    }

  }

  private void doVisit( final ITupleModelVisitor visitor, final int index ) throws SensorException, CancelVisitorException
  {
    visitor.visit( new ITupleModelValueContainer()
    {
      @Override
      public int getIndex( )
      {
        return index;
      }

      @Override
      public Object get( final IAxis axis )
      {
        if( AxisUtils.isDateAxis( axis ) )
          return m_tupples[index].getDate();
        else if( AxisUtils.isStatusAxis( axis ) )
          return m_tupples[index].getStatus();

        return m_tupples[index].getValue();
      }

      @Override
      public Object getPrevious( final IAxis axis )
      {
        if( index > 0 )
          if( AxisUtils.isDateAxis( axis ) )
            return m_tupples[index - 1].getDate();
          else if( AxisUtils.isStatusAxis( axis ) )
            return m_tupples[index - 1].getStatus();
          else
            return m_tupples[index - 1].getValue();

        return null;
      }

      @Override
      public Object getNext( final IAxis axis )
      {
        if( index + 1 < m_tupples.length )
          if( index > 0 )
            if( AxisUtils.isDateAxis( axis ) )
              return m_tupples[index - 1].getDate();
            else if( AxisUtils.isStatusAxis( axis ) )
              return m_tupples[index - 1].getStatus();
            else
              return m_tupples[index - 1].getValue();

        return null;
      }

      @Override
      public boolean hasAxis( final String... types )
      {
        return true;
      }

      @Override
      public IAxis[] getAxes( )
      {
        return DataCenterTuppleModel.this.getAxes();
      }

      @Override
      public void set( final IAxis axis, final Object value )
      {
        if( AxisUtils.isDateAxis( axis ) )
          m_tupples[index].setDate( (Date) value );
        else if( AxisUtils.isStatusAxis( axis ) )
          m_tupples[index].setStatus( (String) value );

        m_tupples[index].setValue( ((Number) value).doubleValue() );
      }

      @Override
      public TupleModelDataSet getDataSetFor( final MetadataList metadata, final String valueAxis ) throws SensorException
      {
        return TupleModelDataSet.toDataSet( this, metadata, valueAxis );
      }
    } );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITupleModel#isEmpty()
   */
  @Override
  public boolean isEmpty( )
  {
    return size() == 0;
  }

  @Override
  public void addChangeListener( final ITupleModelChangeListener listener )
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public IStatus execute( final ITupleModelTransaction transaction )
  {
    throw new UnsupportedOperationException();
  }
}