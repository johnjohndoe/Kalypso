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
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.filters.AbstractObservationFilter;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
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
  public TranProLinFilter( final Date dateBegin, final Date dateEnd, final String operator, final double operandBegin,
      final double operandEnd, final int statusToMerge, final String axisTypes )
  {
    m_dateBegin = dateBegin;
    m_dateEnd = dateEnd;
    m_operandBegin = operandBegin;
    m_operandEnd = operandEnd;
    m_statusToMerge = statusToMerge;
    m_axisTypes = axisTypes;
    m_operation = MathOperationFactory.createMathOperation( operator );
    if( dateBegin != null && dateEnd != null && ( dateBegin.after( dateEnd ) || dateBegin.equals( dateEnd ) ) )
      throw new IllegalArgumentException( "Anfangsdatum und Enddatum sind nicht gültig: " + dateBegin + " - " + dateEnd );
  }

  /**
   * @see org.kalypso.ogc.sensor.filter.filters.AbstractObservationFilter#getValues(org.kalypso.ogc.sensor.request.IRequest)
   */
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

      Date dateBegin = m_dateBegin;
      Date dateEnd = m_dateEnd;

      // policy if beginn/end is null assume values in following order:
      // 1. use from/to from request
      // 2. use first/last from base observation

      // try to assume from request if needed
      if( args != null && args.getDateRange() != null )
      {
        final DateRange dateRange = args.getDateRange();
        if( dateBegin == null && dateRange.getFrom() != null )
          dateBegin = dateRange.getFrom();
        if( dateEnd == null && dateRange.getTo() != null )
          dateEnd = dateRange.getTo();
      }
      // try to assume from base tuppel model if needed
      if( dateBegin == null )
        dateBegin = (Date)outerSource.getElement( 0, dateAxis );
      if( dateEnd == null )
        dateEnd = (Date)outerSource.getElement( outerSourceCount - 1, dateAxis );

      //      // iterate first time to know the real bounds
      //      Date tranpolinBegin = null;
      //      Date tranpolinEnd = null;
      //      for( int i = 0; i < values.getCount(); i++ )
      //      {
      //        final Date date = (Date)values.getElement( i, dateAxis );
      //
      //        if( date.compareTo( dateBegin ) >= 0 && tranpolinBegin == null )
      //          tranpolinBegin = date;
      //
      //        if( date.compareTo( dateEnd ) <= 0 )
      //          tranpolinEnd = date;
      //      }

      final int sourceIndexBegin = ObservationUtilities.findNextIndexForDate( outerSource, dateAxis, dateBegin, 0,
          outerSourceCount );
      final int sourceIndexEnd = ObservationUtilities.findNextIndexForDate( outerSource, dateAxis, dateEnd,
          sourceIndexBegin, outerSourceCount );

      if( sourceIndexEnd > outerSourceCount - 1 )
      {
        System.out.println( "bloed" );
      }

      int targetMaxRows = sourceIndexEnd - sourceIndexBegin + 1;

      final long distTime = dateEnd.getTime() - dateBegin.getTime();

      // sort axis
      final List axesListToCopy = new ArrayList();
      final List axesListToTransform = new ArrayList();
      final List axesListStatus = new ArrayList();
      for( int i = 0; i < axes.length; i++ )
      {
        final IAxis axis = axes[i];
        if( axis.getDataClass() == Date.class ) // always copy date axis
          continue;
        //          axesListToCopy.add( axis );
        else if( KalypsoStatusUtils.isStatusAxis( axis ) ) // special status axis
          axesListStatus.add( axis );
        else if( m_axisTypes == null || m_axisTypes.indexOf( axis.getType() ) > -1 ) // transform axis
          axesListToTransform.add( axis );
        else
          axesListToCopy.add( axis ); // copy axis
      }
      final IAxis[] axesStatus = (IAxis[])axesListStatus.toArray( new IAxis[axesListStatus.size()] );
      final IAxis[] axesCopy = (IAxis[])axesListToCopy.toArray( new IAxis[axesListToCopy.size()] );
      final IAxis[] axesTransform = (IAxis[])axesListToTransform.toArray( new IAxis[axesListToTransform.size()] );
      final Date[] targetDates = new Date[targetMaxRows];
      for( int row = sourceIndexBegin; row < sourceIndexEnd + 1; row++ )
        targetDates[row] = (Date)outerSource.getElement( row, dateAxis );

      final ITuppleModel innerSource;
      // find inner source to initialize inner target model
      if( outerSource instanceof WQTuppleModel )
      {
        final WQTuppleModel wqTuppleModel = (WQTuppleModel)outerSource;
        innerSource = wqTuppleModel.getBaseModel();
      }
      else
        innerSource = outerSource;

      // initialize inner target
      final Object[][] vallues = createValueArray( targetDates, innerSource.getAxisList().length, innerSource
          .getPositionFor( dateAxis ) );
      final SimpleTuppleModel innerTarget = new SimpleTuppleModel( innerSource.getAxisList(), vallues );

      // initialize outer target
      final ITuppleModel outerTarget;
      if( outerSource instanceof WQTuppleModel )
      {
        final WQTuppleModel wqTuppleModel = (WQTuppleModel)outerSource;
        outerTarget = new WQTuppleModel( innerTarget, axes, dateAxis, wqTuppleModel.getSrcAxis(), wqTuppleModel
            .getSrcStatusAxis(), wqTuppleModel.getDestAxis(), wqTuppleModel.getDestStatusAxis(), wqTuppleModel
            .getConverter(), wqTuppleModel.getDestAxisPos(), wqTuppleModel.getDestStatusAxisPos() );
      }
      else
        outerTarget = innerTarget;

      double deltaOperand = m_operandEnd - m_operandBegin;
      // iterate second time to perform transformation
      int targetRow = 0;
      for( int sourceRow = sourceIndexBegin; sourceRow < sourceIndexEnd + 1; sourceRow++ )
      {
        final Date date = (Date)outerSource.getElement( sourceRow, dateAxis );

        final long hereTime = date.getTime() - dateBegin.getTime();
        final double hereCoeff = m_operandBegin + deltaOperand * ( (double)hereTime / (double)distTime );

        // copy
        for( int t = 0; t < axesCopy.length; t++ )
        {
          final IAxis axis = axesCopy[t];
          final Object value = outerSource.getElement( sourceRow, axis );
          outerTarget.setElement( targetRow, value, axis );
        }
        // transform
        //        for( int t = 0; t < axesTransform.length; t++ )
        //        {
        //          final IAxis axis = axesTransform[t];
        //          final Object value = outerSource.getElement( sourceRow, axis );
        //          outerTarget.setElement( targetRow, value, axis );
        //        }

        // status
        for( int t = 0; t < axesStatus.length; t++ )
        {
          final IAxis axis = axesStatus[t];
          final Number oldValue = (Number)outerSource.getElement( sourceRow, axis );
          final Number newValue = new Integer( KalypsoStatusUtils.performArithmetic( oldValue.intValue(),
              m_statusToMerge ) );
          outerTarget.setElement( targetRow, newValue, axis );
        }

        //transform
        for( int t = 0; t < axesTransform.length; t++ )
        {
          final IAxis axis = axesTransform[t];
          Number value = (Number)outerSource.getElement( sourceRow, axis );
          value = new Double( m_operation.calculate( new double[]
          { value.doubleValue(), hereCoeff } ) );
          // important to set transformed last, as there may
          // be dependencies to other axes
          // (e.g. WQ-Transformation)
          outerTarget.setElement( targetRow, value, axis );
        }
        targetRow++;
      }
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
