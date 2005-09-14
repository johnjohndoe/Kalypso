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

import java.awt.Color;
import java.awt.GradientPaint;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.internal.Workbench;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.StandardLegend;
import org.kalypso.contribs.java.lang.CatchRunnable;
import org.kalypso.contribs.java.swing.SwingInvokeHelper;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagView;
import org.kalypso.ogc.sensor.diagview.DiagViewCurve;
import org.kalypso.ogc.sensor.template.IObsViewEventListener;
import org.kalypso.ogc.sensor.template.ObsViewEvent;

/**
 * @author schlienger
 */
public class ObservationChart extends JFreeChart implements IObsViewEventListener
{
  protected static final Logger LOGGER = Logger.getLogger( ObservationChart.class.getName() );

  private final StandardLegend m_legend = new StandardLegend();

  private final DiagView m_view;

  private final boolean m_waitForSwing;

  public ObservationChart( final DiagView template ) throws SensorException
  {
    this( template, false );
  }

  /**
   * Creates an ObservationChart
   * 
   * @param waitForSwing
   *          when true, the events are handled synchonuously in onObsviewChanged(), this is usefull when you are
   *          creating the diagram for non-gui purposes such as in the export-document-wizard: there you need to wait
   *          for swing to be finished with updating/painting the diagram before doing the export, else you get strange
   *          results
   */
  public ObservationChart( final DiagView template, boolean waitForSwing ) throws SensorException
  {
    super( template.getTitle(), JFreeChart.DEFAULT_TITLE_FONT, ChartFactory.createObservationPlot( template ), false );

    m_view = template;
    m_waitForSwing = waitForSwing;

    setLegendProperties( template.getLegendName(), template.isShowLegend() );

    // removed in this.dispose()
    m_view.addObsViewEventListener( this );

    // good for the eyes
    setBackgroundPaint( new GradientPaint( 0, 0, Color.white, 0, 1000, new Color( 168, 168, 255 ) ) );
  }

  protected void setLegendProperties( String legendName, boolean showLegend )
  {
    m_legend.setTitle( legendName );

    if( showLegend )
      setLegend( m_legend );
    else
      setLegend( null );
  }

  /**
   * Disposes the chart.
   */
  public void dispose()
  {
    m_view.removeObsViewListener( this );

    clearChart();
  }

  /**
   * Clears the curves in the chart
   */
  public void clearChart()
  {
    getObservationPlot().clearCurves();
  }

  /**
   * @return plot casted as obs plot
   */
  public ObservationPlot getObservationPlot()
  {
    return (ObservationPlot)getPlot();
  }

  /**
   * @see org.kalypso.ogc.sensor.template.IObsViewEventListener#onObsViewChanged(org.kalypso.ogc.sensor.template.ObsViewEvent)
   */
  public void onObsViewChanged( final ObsViewEvent evt )
  {
    final CatchRunnable runnable = new CatchRunnable()
    {
      protected void runIntern() throws Throwable
      {
        final ObservationPlot obsPlot = getObservationPlot();

        DiagView view = null;
        if( evt.getObject() instanceof DiagView )
          view = (DiagView)evt.getObject();
        else if( evt.getObject() instanceof DiagViewCurve )
          view = (DiagView)( (DiagViewCurve)evt.getObject() ).getView();

        final int et = evt.getType();

        switch( et )
        {
          case ObsViewEvent.TYPE_ADD:
            obsPlot.addCurve( (DiagViewCurve)evt.getObject() );
            break;

          case ObsViewEvent.TYPE_REMOVE:
            obsPlot.removeCurve( (DiagViewCurve)evt.getObject() );
            break;

          case ObsViewEvent.TYPE_REMOVE_ALL:
            clearChart();
            break;

          case ObsViewEvent.TYPE_REFRESH_ITEMSTATE:
            final DiagViewCurve curve = (DiagViewCurve)evt.getObject();
            obsPlot.removeCurve( curve );
            if( curve.isShown() )
              obsPlot.addCurve( curve );
            break;

          case ObsViewEvent.TYPE_REFRESH:
            setTitle( view.getTitle() );
            setLegendProperties( view.getLegendName(), view.isShowLegend() );
            break;

          case ObsViewEvent.TYPE_REFRESH_FEATURES:
            obsPlot.refreshMetaInformation();
            break;
        }
      }
    };

    try
    {
      SwingInvokeHelper.invoke( runnable, m_waitForSwing );
    }
    catch( final Throwable e )
    {
      e.printStackTrace();

      final IWorkbenchWindow activeWorkbenchWindow = Workbench.getInstance().getActiveWorkbenchWindow();
      final Shell shell = activeWorkbenchWindow == null ? null : activeWorkbenchWindow.getShell();
      if( shell != null )
        MessageDialog.openError( shell, "Aktualisierungsfehler", e.toString() );
      else
        JOptionPane.showMessageDialog( null, e.toString(), "Aktualisierungsfehler", JOptionPane.ERROR_MESSAGE );
    }
  }
}