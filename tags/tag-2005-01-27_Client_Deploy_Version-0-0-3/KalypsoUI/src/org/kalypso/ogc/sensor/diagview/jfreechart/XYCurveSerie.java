/*--------------- Kalypso-Header --------------------------------------------------------------------

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
  
---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.diagview.jfreechart;

import java.util.Date;

import org.jfree.data.general.Series;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagViewCurve;
import org.kalypso.ogc.sensor.diagview.DiagramAxis;

/**
 * A CurveSerie.
 * 
 * @author schlienger
 */
class XYCurveSerie extends Series
{
  private transient final IAxis m_xAxis;

  private transient final DiagramAxis m_xDiagAxis;

  private transient final IAxis m_yAxis;

  private transient final DiagramAxis m_yDiagAxis;

  private transient final DiagViewCurve m_curve;

  private transient ITuppleModel m_values = null;

  /**
   * Constructor. Fetches the values (ITuppleModel).
   * 
   * @param curve
   * @param xAxis
   *          the IAxis from the IObservation to be used as X-Axis.
   * @param yAxis
   *          the IAxis from the IObservation to be used as Y-Axis.
   * @param xDiagAxis
   *          the IDiagramAxis mapped to xAxis
   * @param yDiagAxis
   *          the IDiagramAxis mapped to yAxis
   * @throws SensorException
   */
  public XYCurveSerie( final DiagViewCurve curve, final IAxis xAxis, final IAxis yAxis,
      final DiagramAxis xDiagAxis, final DiagramAxis yDiagAxis ) throws SensorException
  {
    super( curve.getName() );

    m_curve = curve;
    m_xAxis = xAxis;
    m_yAxis = yAxis;
    m_xDiagAxis = xDiagAxis;
    m_yDiagAxis = yDiagAxis;

    m_values = m_curve.getTheme().getObservation().getValues( m_curve.getTheme().getArguments() );
  }

  public DiagramAxis getXDiagAxis()
  {
    return m_xDiagAxis;
  }

  public DiagramAxis getYDiagAxis()
  {
    return m_yDiagAxis;
  }

  public int getItemCount() throws SensorException
  {
    return m_values.getCount();
  }

  public Number getXValue( int item ) throws SensorException
  {
    final Object obj = m_values.getElement( item, m_xAxis );

    if( obj instanceof Number )
      return (Number) obj;
    else if( obj instanceof Date )
      return new Double(( (Date)obj ).getTime());

    return null;
  }

  public Number getYValue( int item ) throws SensorException
  {
    final Object obj = m_values.getElement( item, m_yAxis );

    if( obj instanceof Number )
      return (Number) obj;

    return null;
  }
}