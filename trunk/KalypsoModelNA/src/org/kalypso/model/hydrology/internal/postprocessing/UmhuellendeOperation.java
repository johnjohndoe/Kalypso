/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.hydrology.internal.postprocessing;

import java.lang.reflect.InvocationTargetException;
import java.util.Calendar;
import java.util.Date;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.jfree.data.time.DateRange;
import org.kalypso.commons.lhwz.LhwzHelper;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.convert.namodel.timeseries.NATimeSettings;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IAxisRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.timeseries.envelope.TranProLinFilterUtilities;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.simulation.core.SimulationException;

/**
 * Erzeugt umhüllende Kurven für eine Vorhersagezeitreihe
 * 
 * @author Gernot Belger
 */
public class UmhuellendeOperation implements ICoreRunnableWithProgress
{
  private final ZmlLink m_resultLink;

  private final ZmlLink m_measuredLink;

  private final ZmlLink m_linkAblageOben;

  private final ZmlLink m_linkAblageUnten;

  private final ZmlLink m_linkAblageMitte;

  private Double m_accuracy;

  private boolean m_useOffsetStart;

  private boolean m_useOffsetEnd;

  private final DateRange m_forecastRange;

  public UmhuellendeOperation( final ZmlLink resultLink, final ZmlLink measuredLink, final ZmlLink linkAblageMitte, final ZmlLink linkAblageUnten, final ZmlLink linkAblageOben, final DateRange forecastRange )
  {
    m_resultLink = resultLink;
    m_measuredLink = measuredLink;
    m_linkAblageMitte = linkAblageMitte;
    m_linkAblageUnten = linkAblageUnten;
    m_linkAblageOben = linkAblageOben;
    m_forecastRange = forecastRange;
  }

  public void setAccuracy( final Double accuracy )
  {
    m_accuracy = accuracy;
  }

  public void setUseOffsetStart( final boolean useOffsetStart )
  {
    m_useOffsetStart = useOffsetStart;
  }

  public void setUseOffsetEnd( final boolean useOffsetEnd )
  {
    m_useOffsetEnd = useOffsetEnd;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException, CoreException
  {
    try
    {
      return doExecute();
    }
    catch( final CoreException e )
    {
      throw e;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new InvocationTargetException( e );
    }
    finally
    {
      monitor.done();
    }
  }

  private IStatus doExecute( ) throws SensorException, SimulationException, CoreException
  {
    final IObservation resultObservation = m_resultLink.loadObservation();
    if( resultObservation == null )
      return new Status( IStatus.WARNING, ModelNA.PLUGIN_ID, "Umhüllende konnte nicht erzeugt werden. Ergebnis liegt nicht vor." );

    final IAxis[] axisList = resultObservation.getAxisList();
    final String axisType = determineTranpolinAxis( resultObservation );

    // Initalize some commen variables
    final ITupleModel resultValues = resultObservation.getValues( null );
    final IAxis resultDateAxis = ObservationUtilities.findAxisByClass( axisList, Date.class );
    final IAxis resultValueAxis = ObservationUtilities.findAxisByType( axisList, axisType );


    final IAxisRange rangeFor = resultValues.getRange( resultDateAxis );
    final Date endPrediction = (Date) rangeFor.getUpper();

    final Date startForecast = m_forecastRange.getLowerDate();
    final Date endForecast = m_forecastRange.getUpperDate();

    final NATimeSettings timeSettings = NATimeSettings.getInstance();
    final Calendar calBegin = timeSettings.getCalendar( startForecast );
    // REMARK: using endPrediction instead of endForecast, as they are not equals (but they should...)
    final Calendar calEnd = timeSettings.getCalendar( endPrediction );

    final double calcStartValue = ObservationUtilities.getInterpolatedValueAt( resultValues, resultDateAxis, resultValueAxis, startForecast );
    final double calcEndValue = ObservationUtilities.getInterpolatedValueAt( resultValues, resultDateAxis, resultValueAxis, endForecast );

    //
    // First, we adapt the result: correction at start and/or end of the calculated timeserie
    //
    final double deltaMeasureCalculation = getDeltaMeasuredCalculation( m_measuredLink, axisType, startForecast, calcStartValue );

    final double offsetStartPrediction = getOffsetStartPrediction( deltaMeasureCalculation );
    final double offsetEndPrediction = getOffsetEndPrediction( deltaMeasureCalculation );

    final Calendar tranpolinEnd = timeSettings.getCalendar( startForecast );
    tranpolinEnd.add( Calendar.HOUR, 24 );

    final IRequest request = new ObservationRequest( calBegin.getTime(), calEnd.getTime() );
    final String nameMitte = TranProLinFilterUtilities.getName( resultObservation, " - Spur Mitte" );//$NON-NLS-1$ 
    TranProLinFilterUtilities.transformAndWrite( resultObservation, calBegin, tranpolinEnd, offsetStartPrediction, offsetEndPrediction, "+", axisType, KalypsoStati.BIT_DERIVATED, m_linkAblageMitte, nameMitte, request ); //$NON-NLS-1$ //$NON-NLS-2$

    // read the freshly created file into a new observation, we are going to umhüll it
    final IObservation adaptedResultObservation = m_linkAblageMitte.loadObservation();

    //
    // Second, we build the umhüllenden for the adapted result
    //
    final double accuracyPrediction = getAccuracy();

    // accuracyPrediction // %/60h
    final long millisOf60hours = 1000 * 60 * 60 * 60;
    // endAccuracy: %/simulationRange
    final double endAccuracy = accuracyPrediction * (((double) (endForecast.getTime() - startForecast.getTime())) / (millisOf60hours));

    final double endOffset = calcEndValue * (endAccuracy / 100);

    final String nameUnten = TranProLinFilterUtilities.getName( resultObservation, " - spur Unten" ); //$NON-NLS-1$ 
    final String nameOben = TranProLinFilterUtilities.getName( resultObservation, " - spur Oben" );//$NON-NLS-1$ 
    TranProLinFilterUtilities.transformAndWrite( adaptedResultObservation, calBegin, calEnd, 0, endOffset, "-", axisType, KalypsoStati.BIT_DERIVATED, m_linkAblageUnten, nameUnten, request ); //$NON-NLS-1$
    TranProLinFilterUtilities.transformAndWrite( adaptedResultObservation, calBegin, calEnd, 0, endOffset, "+", axisType, KalypsoStati.BIT_DERIVATED, m_linkAblageOben, nameOben, request ); //$NON-NLS-1$

    if( m_accuracy == null )
    {
      final String msg = Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.44", accuracyPrediction ); //$NON-NLS-1$ 
      return new Status( IStatus.WARNING, ModelNA.PLUGIN_ID, msg );
    }

    return Status.OK_STATUS;
  }

  private double getAccuracy( )
  {
    if( m_accuracy == null )
      return LhwzHelper.getDefaultUmhuellendeAccuracy();

    return m_accuracy.doubleValue();
  }

  private double getOffsetStartPrediction( final double deltaMeasureCalculation )
  {
    if( m_useOffsetStart )
      return deltaMeasureCalculation;

    return 0;
  }

  private double getOffsetEndPrediction( final double deltaMeasureCalculation )
  {
    if( m_useOffsetEnd )
      return deltaMeasureCalculation;

    return 0;
  }

  /**
   * Return with which axis we are going to transform the umhüllenden. Q or W, depending on what is present)
   */
  private String determineTranpolinAxis( final IObservation observation ) throws SimulationException
  {
    final IAxis[] axisList = observation.getAxisList();

    if( ObservationUtilities.hasAxisOfType( axisList, ITimeseriesConstants.TYPE_RUNOFF ) )
      return ITimeseriesConstants.TYPE_RUNOFF;

    if( ObservationUtilities.hasAxisOfType( axisList, ITimeseriesConstants.TYPE_WATERLEVEL ) )
      return ITimeseriesConstants.TYPE_WATERLEVEL;

    throw new SimulationException( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.50" ), null ); //$NON-NLS-1$
  }

  private double getDeltaMeasuredCalculation( final ZmlLink measuredLink, final String axisType, final Date startForecast, final double calcStartValue )
  {
    try
    {
      final IObservation measuredObservation = measuredLink.loadObservation();
      if( measuredObservation == null )
        return 0;

      final ITupleModel measuredValues = measuredObservation.getValues( null );
      final IAxis measuredDateAxis = ObservationUtilities.findAxisByClass( measuredObservation.getAxisList(), Date.class );
      final IAxis measuredValueAxis = ObservationUtilities.findAxisByType( measuredObservation.getAxisList(), axisType );
      final double measureValue = ObservationUtilities.getInterpolatedValueAt( measuredValues, measuredDateAxis, measuredValueAxis, startForecast );
      return measureValue - calcStartValue;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return 0;
    }
  }
}
