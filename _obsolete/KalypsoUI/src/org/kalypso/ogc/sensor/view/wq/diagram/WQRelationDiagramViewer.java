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

package org.kalypso.ogc.sensor.view.wq.diagram;

import java.awt.Frame;

import org.eclipse.compare.internal.AbstractViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.contribs.java.awt.ColorUtilities;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.AxisMapping;
import org.kalypso.ogc.sensor.diagview.DiagView;
import org.kalypso.ogc.sensor.diagview.DiagViewCurve;
import org.kalypso.ogc.sensor.diagview.DiagramAxis;
import org.kalypso.ogc.sensor.diagview.jfreechart.ChartFactory;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationChart;
import org.kalypso.ogc.sensor.template.PlainObsProvider;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTable;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableSet;

/**
 * @author schlienger
 */
public class WQRelationDiagramViewer extends AbstractViewer implements DisposeListener
{
  private ObservationChart m_chart;

  private final DiagView m_diagView = new DiagView( true );
  private final DiagramAxis m_diagramAxisW;
  private final DiagramAxis m_diagramAxisQ;

  private Composite m_control = null;

  public WQRelationDiagramViewer( final Composite parent )
  {
    m_diagramAxisW = new DiagramAxis( "w", "double", "W", "cm", DiagramAxis.DIRECTION_VERTICAL, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        DiagramAxis.POSITION_LEFT, false );
    m_diagramAxisQ = new DiagramAxis( "q", "double", "Q", "m³/s", DiagramAxis.DIRECTION_HORIZONTAL, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        DiagramAxis.POSITION_BOTTOM, false );

    m_diagView.addAxis( m_diagramAxisW );
    m_diagView.addAxis( m_diagramAxisQ );

    createControl( parent );
  }

  private void dispose()
  {
    m_diagView.dispose();

    if( m_chart != null )
      m_chart.dispose();
  }

  private final void createControl( final Composite parent )
  {
    try
    {
      m_chart = new ObservationChart( m_diagView );
    }
    catch( final SensorException e )
    {
      e.printStackTrace();
      return;
    }

    // SWT-AWT Brücke für die Darstellung von JFreeChart
    m_control = new Composite( parent, SWT.RIGHT | SWT.EMBEDDED | SWT.BORDER );

    final Frame vFrame = SWT_AWT.new_Frame( m_control );
    vFrame.add( ChartFactory.createChartPanel( m_chart ) );
    vFrame.setVisible( true );
    
    m_control.addDisposeListener( this );
  }

  public void setInput( final WQTableSet wqs ) throws SensorException
  {
    m_chart.clearChart();
    
    if( wqs == null )
      return;

    final WQTable[] tables = wqs.getTables();
    for( int i = 0; i < tables.length; i++ )
    {
      final IObservation obs = WQCurveFactory.createObservation( tables[i] );

      final AxisMapping[] axmaps = new AxisMapping[2];
      axmaps[0] = new AxisMapping( obs.getAxisList()[0], m_diagramAxisW );
      axmaps[1] = new AxisMapping( obs.getAxisList()[1], m_diagramAxisQ );

      final DiagViewCurve curve = new DiagViewCurve( m_diagView, new PlainObsProvider( obs, null ), obs.getName(),
          ColorUtilities.random(), null, axmaps );

      m_chart.getObservationPlot().addCurve( curve );
    }
  }

  /**
   * @see org.eclipse.jface.viewers.Viewer#getControl()
   */
  @Override
  public Control getControl()
  {
    return m_control;
  }

  /**
   * @see org.eclipse.swt.events.DisposeListener#widgetDisposed(org.eclipse.swt.events.DisposeEvent)
   */
  public void widgetDisposed( DisposeEvent e )
  {
    dispose();
  }
}
