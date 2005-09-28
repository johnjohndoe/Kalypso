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

import java.util.Date;
import java.util.NoSuchElementException;
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

  /**
   * @param statusToMerge
   *          status is merged to modified values as bitwise OR operation (use <code>statusToMerge=0</code> for
   *          unchanged status)
   */
  public TranProLinFilter( final Date dateBegin, final Date dateEnd, final String operator, final double operandBegin,
      final double operandEnd, final int statusToMerge )
  {
    m_dateBegin = dateBegin;
    m_dateEnd = dateEnd;
    m_operandBegin = operandBegin;
    m_operandEnd = operandEnd;
    m_statusToMerge = statusToMerge;
    m_operation = MathOperationFactory.createMathOperation( operator );
    if( dateBegin != null && dateEnd != null && ( dateBegin.after( dateEnd ) || dateBegin.equals( dateEnd ) ) )
      throw new IllegalArgumentException( "Anfangsdatum und Enddatum sind nicht gültig: " + dateBegin + " - " + dateEnd );
  }

  /**
   * @see org.kalypso.ogc.sensor.filter.filters.AbstractObservationFilter#getValues(org.kalypso.ogc.sensor.request.IRequest)
   */
  public ITuppleModel getValues( final IRequest args ) throws SensorException
  {
    final ITuppleModel values = super.getValues( args );

    if( values.getCount() == 0 )
      return values;

    final IAxis[] axes = values.getAxisList();

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
        dateBegin = (Date)values.getElement( 0, dateAxis );
      if( dateEnd == null )
        dateEnd = (Date)values.getElement( values.getCount() - 1, dateAxis );

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

      final long distTime = dateEnd.getTime() - dateBegin.getTime();

      final IAxis[] valueAxes = ObservationUtilities.findAxesByClass( axes, Number.class );
      final SimpleTuppleModel filtered = new SimpleTuppleModel( axes );

      double deltaOperand = m_operandEnd - m_operandBegin;
      // iterate second time to perform transformation
      for( int i = 0; i < values.getCount(); i++ )
      {
        final Date date = (Date)values.getElement( i, dateAxis );

        if( date.compareTo( dateBegin ) >= 0 && date.compareTo( dateEnd ) <= 0 )
        {
          final long hereTime = date.getTime() - dateBegin.getTime();

          final double hereCoeff = m_operandBegin + deltaOperand * hereTime / distTime;

          final Object[] tupple = new Object[valueAxes.length + 1];
          for( int t = 0; t < valueAxes.length; t++ )
          {
            final IAxis axis = valueAxes[t];
            if( !axis.getDataClass().equals( Date.class ) )
            {
              Number value = (Number)values.getElement( i, axis );

              if( !KalypsoStatusUtils.isStatusAxis( axis ) )
                value = new Double( m_operation.calculate( new double[]
                {
                    value.doubleValue(),
                    hereCoeff } ) );
              else
                value = new Integer( KalypsoStatusUtils.performArithmetic( value.intValue(), m_statusToMerge ) );
              tupple[values.getPositionFor( axis )] = value;
            }
          }

          tupple[values.getPositionFor( dateAxis )] = date;

          filtered.addTupple( tupple );
        }
      }

      return filtered;
    }
    catch( final NoSuchElementException e )
    {
      final Logger logger = Logger.getLogger( getClass().getName() );
      logger.log( Level.WARNING, "Umhüllende konnte nicht erzeugt werden.", e );

      return values;
    }
  }
}
