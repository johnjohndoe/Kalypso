/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ui.rrm.wizards.conversion.from103to230;

import java.util.Date;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.joda.time.DateTime;
import org.joda.time.Interval;
import org.joda.time.Period;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.SimpleTupleModel;
import org.kalypso.ogc.sensor.timeseries.AxisUtils;
import org.kalypso.ui.rrm.KalypsoUIRRMPlugin;

/**
 * @author Gernot Belger
 */
public class TimeseriesExtender
{
  private final ITupleModel m_sourceValues;

  private final IAxis[] m_axisList;

  private final String m_href;

  private final IAxis m_dateAxis;

  private final int m_dateIndex;

  private SimpleTupleModel m_targetValues;

  private Period m_stepping;

  private Interval m_sourceRange;

  private int m_addCounter;

  public TimeseriesExtender( final ITupleModel sourceValues, final String href ) throws CoreException, SensorException
  {
    m_sourceValues = sourceValues;
    m_href = href;

    m_axisList = m_sourceValues.getAxisList();
    m_dateAxis = AxisUtils.findDateAxis( m_axisList );
    if( m_dateAxis == null )
    {
      final String msg = String.format( "Not a timeseries: %s", href );
      final IStatus status = new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), msg );
      throw new CoreException( status );
    }
    m_dateIndex = m_sourceValues.getPosition( m_dateAxis );
  }

  public void checkSize( ) throws CoreException, SensorException
  {
    final int size = m_sourceValues.size();
    if( size == 0 )
      return;

    if( size == 1 )
    {
      final String msg = String.format( "Zeitreihe %s enth�lt nur einen Wert. Automatische Verl�ngerung nicht m�glich.", m_href );
      final IStatus status = new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), msg );
      throw new CoreException( status );
    }
  }

  public boolean checkRange( final Interval simulationRange ) throws SensorException
  {
    final int size = m_sourceValues.size();
    final Date startDate = (Date) m_sourceValues.get( 0, m_dateAxis );
    final Date endDate = (Date) m_sourceValues.get( size - 1, m_dateAxis );

    final DateTime startDateTime = new DateTime( startDate );
    final DateTime endDateTime = new DateTime( endDate );

    m_sourceRange = new Interval( startDateTime, endDateTime );
    return !m_sourceRange.contains( simulationRange );
  }

  public IStatus extend( final Interval simulationRange ) throws SensorException, CoreException
  {
    m_targetValues = new SimpleTupleModel( m_axisList );
    m_stepping = findStepping();

    final DateTime startSimulation = simulationRange.getStart();
    extendStart( startSimulation );

    copyValues();

    final DateTime endSimulation = simulationRange.getEnd();
    extendEnd( endSimulation );

    return createAddedStatus();
  }

  private IStatus createAddedStatus(  )
  {
    switch( m_addCounter )
    {
      case 0:
        return Status.OK_STATUS;

      case 1:
      {
        final String msg = String.format( "Zeitreihe %s wurde automatisch verl�ngert (1 Zeitschritt)", m_href );
        return new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), msg );
      }

      default:
      {
        final String msg = String.format( "Zeitreihe %s wurde um %d Zeitschritte automatisch verl�ngert. Die Zeitschritte sind mit dem Warnsymbol markiert. Bitte pr�fen Sie die Zeitreihe.", m_href, m_addCounter );
        return new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), msg );
      }
    }
  }

  private Period findStepping( ) throws SensorException
  {
    // REMARK: we determine the stepping solely on the first two dates. This is a bit dangerous....
    final Date firstDate = (Date) m_sourceValues.get( 0, m_dateAxis );
    final Date secondDate = (Date) m_sourceValues.get( 1, m_dateAxis );

    final DateTime firstDateTime = new DateTime( firstDate );
    final DateTime secondDateTime = new DateTime( secondDate );

    // FIXME: we probably need to sometimes round values?

    return new Period( firstDateTime, secondDateTime );
  }

  private void extendStart( final DateTime startSimulation ) throws SensorException, CoreException
  {
    final DateTime sourceValuesStart = m_sourceRange.getStart();
    final DateTime startDate = findStartDate( sourceValuesStart, startSimulation );
    addValues( 0, startDate, sourceValuesStart );
  }

  /* Reverse search for suitable start date. */
  private DateTime findStartDate( final DateTime sourceValuesStart, final DateTime startSimulation )
  {
    DateTime currentTime = sourceValuesStart;
    while( currentTime.isAfter( startSimulation ) )
      currentTime = currentTime.minus( m_stepping );

    return currentTime;
  }

  private void extendEnd( final DateTime endSimulation ) throws SensorException, CoreException
  {
    final int valueIndex = m_sourceValues.size() - 1;
    addValues( valueIndex, m_sourceRange.getEnd(), endSimulation );
  }

  private Object[] getSourceValuesAt( final int index ) throws SensorException
  {
    final Object[] startValues = new Object[m_axisList.length];
    for( int i = 0; i < m_axisList.length; i++ )
      startValues[i] = m_sourceValues.get( index, m_axisList[i] );
    return startValues;
  }

  private void copyValues( ) throws SensorException
  {
    final int size = m_sourceValues.size();
    final IAxis[] axisList = m_sourceValues.getAxisList();
    for( int i = 0; i < size; i++ )
    {
      final Object[] values = new Object[axisList.length];
      for( int j = 0; j < axisList.length; j++ )
        values[j] = m_sourceValues.get( i, axisList[j] );

      m_targetValues.addTuple( values );
    }
  }

  /**
   * Adds values into the target timeseries starting by <code>start</code> until the end-date (<code>until</code>) is
   * reached.<br/>
   * The values are copied from the source timeseries at the given <code>valueIndex</code>.
   */
  private void addValues( final int valueIndex, final DateTime start, final DateTime until ) throws SensorException, CoreException
  {
    final Object[] startValues = getSourceValuesAt( valueIndex );

    int timeout = 0;
    DateTime timePointer = start;
    while( timePointer.isBefore( until ) )
    {
      final Object[] newTupple = startValues.clone();
      newTupple[m_dateIndex] = timePointer.toDate();
      m_targetValues.addTuple( newTupple );
      m_addCounter++;

      timePointer = timePointer.plus( m_stepping );

      // REMARK: in order to avoid an endless loop here, we stop after 10 (which would already be quite suspicious).
      timeout++;
      if( timeout == 10 )
      {
        final String msg = String.format( "Zeitreihe %s: mehr als 10 Werte m�ssten erg�nzt werden. Abbruch, Zeitreihe wurde nicht verl�ngert." );
        final IStatus error = new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), msg );
        throw new CoreException( error );
      }
    }
  }

  public ITupleModel getExtendedValues( )
  {
    return m_targetValues;
  }
}
