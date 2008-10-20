/*--------------- Kalypso-Header ------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 --------------------------------------------------------------------------*/

package org.kalypso.ogc.sensor.timeseries.envelope;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.kalypso.commons.math.IMathOperation;
import org.kalypso.commons.math.MathOperationFactory;
import org.kalypso.core.i18n.Messages;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.filters.AbstractObservationFilter;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.wq.WQTuppleModel;

/**
 * The Linear-Progressiv-Transformation Filter is used for creating 'lower and upper envelopes' in the sense of a
 * timeserie. It is currently designed to work only with timeseries: meaning an observation with an axis of type date.
 * 
 * @author schlienger
 */
public class TranProLinFilter extends AbstractObservationFilter
{
  private final Date m_dateBegin;

  private final Date m_dateEnd;

  private final double m_operandBegin;

  private final double m_operandEnd;

  private final int m_statusToMerge;

  private IMathOperation m_operation;

  private final String m_axisTypes;

  /**
   * @param statusToMerge
   *          status is merged to modified values as bitwise OR operation (use <code>statusToMerge=0</code> for
   *          unchanged status)
   * @param axisTypes
   */
  public TranProLinFilter( final Date dateBegin, final Date dateEnd, final String operator, final double operandBegin, final double operandEnd, final int statusToMerge, final String axisTypes )
  {
    m_dateBegin = dateBegin;
    m_dateEnd = dateEnd;
    m_operandBegin = operandBegin;
    m_operandEnd = operandEnd;
    m_statusToMerge = statusToMerge;
    m_axisTypes = axisTypes;
    m_operation = MathOperationFactory.createMathOperation( operator );
    if( dateBegin != null && dateEnd != null && (dateBegin.after( dateEnd ) || dateBegin.equals( dateEnd )) )
      throw new IllegalArgumentException( Messages.getString("org.kalypso.ogc.sensor.timeseries.envelope.TranProLinFilter.0") + dateBegin + " - " + dateEnd ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.ogc.sensor.filter.filters.AbstractObservationFilter#getValues(org.kalypso.ogc.sensor.request.IRequest)
   */
  @Override
  public ITuppleModel getValues( final IRequest args ) throws SensorException
  {
    final ITuppleModel outerSource = super.getValues( args );

    final int outerSourceCount = outerSource.getCount();
    if( outerSourceCount == 0 )
      return outerSource;

    final IAxis[] axes = outerSource.getAxisList();

    try
    {
      final IAxis dateAxis = ObservationUtilities.findAxisByClass( axes, Date.class );

      //      Date dateBegin = m_dateBegin;
      //      Date dateEnd = m_dateEnd;

      // Always use request/full range for the target
      Date targetBegin = null;
      Date targetEnd = null;

      // If transformation start/end are null:
      // 1. use from/to from request
      // 2. use first/last from base observation
      Date transformBegin = m_dateBegin;
      Date transformEnd = m_dateEnd;

      // try to assume from request if needed
      if( args != null && args.getDateRange() != null )
      {
        final DateRange dateRange = args.getDateRange();
        if( targetBegin == null && dateRange.getFrom() != null )
          targetBegin = dateRange.getFrom();
        if( targetEnd == null && dateRange.getTo() != null )
          targetEnd = dateRange.getTo();
      }
      // try to assume from base tuppel model if needed
      if( targetBegin == null )
        targetBegin = (Date)outerSource.getElement( 0, dateAxis );
      if( targetEnd == null )
        targetEnd = (Date)outerSource.getElement( outerSourceCount - 1, dateAxis );

      if( transformBegin == null )
        transformBegin = targetBegin;
      if( transformEnd == null )
        transformEnd = targetEnd;

      final int sourceIndexBegin = ObservationUtilities.findNextIndexForDate( outerSource, dateAxis, targetBegin, 0,
          outerSourceCount );
      final int sourceIndexEnd = ObservationUtilities.findNextIndexForDate( outerSource, dateAxis, targetEnd,
          sourceIndexBegin, outerSourceCount );

      if( sourceIndexEnd > outerSourceCount - 1 )
      {
        System.out.println( Messages.getString("org.kalypso.ogc.sensor.timeseries.envelope.TranProLinFilter.2") ); //$NON-NLS-1$
      }

      final int targetMaxRows = sourceIndexEnd - sourceIndexBegin + 1;

      // sort axis
      final List<IAxis> axesListToCopy = new ArrayList<IAxis>();
      final List<IAxis> axesListToTransform = new ArrayList<IAxis>();
      final List<IAxis> axesListStatus = new ArrayList<IAxis>();
      for( int i = 0; i < axes.length; i++ )
      {
        final IAxis axis = axes[i];
        if( axis.getDataClass() == Date.class ) // always copy date axis
          continue;
        // axesListToCopy.add( axis );
        else if( KalypsoStatusUtils.isStatusAxis( axis ) ) // special status axis
          axesListStatus.add( axis );
        else if( m_axisTypes == null || m_axisTypes.indexOf( axis.getType() ) > -1 ) // transform axis
          axesListToTransform.add( axis );
        else
          axesListToCopy.add( axis ); // copy axis
      }
      final IAxis[] axesStatus = axesListStatus.toArray( new IAxis[axesListStatus.size()] );
      final IAxis[] axesCopy = axesListToCopy.toArray( new IAxis[axesListToCopy.size()] );
      final IAxis[] axesTransform = axesListToTransform.toArray( new IAxis[axesListToTransform.size()] );
      final Date[] targetDates = new Date[targetMaxRows];
      for( int row = sourceIndexBegin; row < sourceIndexEnd + 1; row++ )
        targetDates[row] = (Date) outerSource.getElement( row, dateAxis );

      final ITuppleModel innerSource;
      // find inner source to initialize inner target model
      if( outerSource instanceof WQTuppleModel )
      {
        final WQTuppleModel wqTuppleModel = (WQTuppleModel) outerSource;
        innerSource = wqTuppleModel.getBaseModel();
      }
      else
        innerSource = outerSource;

      // initialize inner target
      final Object[][] vallues = createValueArray( targetDates, innerSource.getAxisList().length, innerSource.getPositionFor( dateAxis ) );
      final SimpleTuppleModel innerTarget = new SimpleTuppleModel( innerSource.getAxisList(), vallues );

      // initialize outer target
      final ITuppleModel outerTarget;
      if( outerSource instanceof WQTuppleModel )
      {
        final WQTuppleModel wqTuppleModel = (WQTuppleModel) outerSource;
        outerTarget = new WQTuppleModel( innerTarget, axes, dateAxis, wqTuppleModel.getSrcAxis(), wqTuppleModel
            .getSrcStatusAxis(), wqTuppleModel.getDestAxis(), wqTuppleModel.getDestStatusAxis(), wqTuppleModel
            .getConverter(), wqTuppleModel.getDestAxisPos(), wqTuppleModel.getDestStatusAxisPos() );
      }
      else
        outerTarget = innerTarget;

      performTransformation( outerSource, dateAxis, transformBegin, transformEnd, sourceIndexBegin, sourceIndexEnd,
          axesStatus, axesCopy, axesTransform, outerTarget );

      return outerTarget;
    }
    catch( final Exception e )
    {
      // TODO: allways gets here, even if only one value cannot be computed
      // better catch exceptions individually?

      e.printStackTrace();
      final Logger logger = Logger.getLogger( getClass().getName() );
      logger.log( Level.WARNING, "Umhüllende konnte nicht erzeugt werden. (WQ-Parameter vollständig ?)", e );
      return outerSource;
    }
  }

  /**
   * @param outerSource
   * @param dateAxis
   * @param dateBegin
   * @param dateEnd
   * @param sourceIndexBegin
   * @param sourceIndexEnd
   * @param axesStatus
   * @param axesCopy
   * @param axesTransform
   * @param outerTarget
   * @throws SensorException
   */
  private void performTransformation( final ITuppleModel outerSource, final IAxis dateAxis, Date dateBegin,
      Date dateEnd, final int sourceIndexBegin, final int sourceIndexEnd, final IAxis[] axesStatus,
      final IAxis[] axesCopy, final IAxis[] axesTransform, final ITuppleModel outerTarget ) throws SensorException
  {
    final long distTime = dateEnd.getTime() - dateBegin.getTime();
    final double deltaOperand = m_operandEnd - m_operandBegin;

    // iterate second time to perform transformation
    int targetRow = 0;
    for( int sourceRow = sourceIndexBegin; sourceRow < sourceIndexEnd + 1; sourceRow++ )
    {
      // copy
      for( int t = 0; t < axesCopy.length; t++ )
      {
        final IAxis axis = axesCopy[t];
        final Object value = outerSource.getElement( sourceRow, axis );
        outerTarget.setElement( targetRow, value, axis );
      }

      // status
      for( int t = 0; t < axesStatus.length; t++ )
      {
        final IAxis axis = axesStatus[t];
        final Number oldValue = (Number)outerSource.getElement( sourceRow, axis );
        final Number newValue = new Integer( KalypsoStatusUtils
            .performArithmetic( oldValue.intValue(), m_statusToMerge ) );
        outerTarget.setElement( targetRow, newValue, axis );
      }

      //transform: important to set transformed last, as there may be dependencies to other axes
      // (e.g. WQ-Transformation)
      // TODO: why are we copying generated values at all? We shouldn't do it then there is no problem

      final Date date = (Date)outerSource.getElement( sourceRow, dateAxis );

      final long hereTime = date.getTime() - dateBegin.getTime();
      final double hereCoeff = m_operandBegin + deltaOperand * ( (double)hereTime / (double)distTime );

      for( int t = 0; t < axesTransform.length; t++ )
      {
        final IAxis axis = axesTransform[t];
        final String type = axis.getType();

        final double currentValue = ( (Number)outerSource.getElement( sourceRow, axis ) ).doubleValue();
        final double changedValue;

        // We do only transform within the specified interval
        if( date.before( dateBegin ) || date.after( dateEnd ) )
          changedValue = currentValue;
        else
        {
          final double[] operands = new double[]
          {
              currentValue,
              hereCoeff };

          changedValue = m_operation.calculate( operands );
        }

        final double checkedValue = checkValue( type, changedValue );

        outerTarget.setElement( targetRow, new Double( checkedValue ), axis );
      }
      targetRow++;
    }
  }

  /**
   * HACK we use this method to make some sanity checks here depending on the type of the axis. <b>Probably this should
   * better be a parameter to this filter?
   */
  private double checkValue( final String axisType, final double value )
  {
    // Prohibit negative value for runoff
    if( TimeserieConstants.TYPE_RUNOFF.equals( axisType ) )
      return Math.max( 0.0, value );

    return value;
  }


  /**
   * creates a nw ojectarray filled with <code>null</code> values and the given dates
   * 
   * @param targetDates
   * @param columns
   * @param positionForDate
   * @return Objectarray
   */
  private Object[][] createValueArray( final Date[] targetDates, final int columns, final int positionForDate )
  {
    final Object[][] result = new Object[targetDates.length][columns];
    for( int row = 0; row < targetDates.length; row++ )
    {
      Object[] objects = result[row];
      Arrays.fill( objects, null );
      result[row][positionForDate] = targetDates[row];
    }
    return result;
  }
}
