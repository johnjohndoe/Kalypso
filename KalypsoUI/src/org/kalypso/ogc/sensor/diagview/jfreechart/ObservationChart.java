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

import java.util.logging.Logger;

import javax.swing.SwingUtilities;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.internal.Workbench;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.StandardLegend;
import org.kalypso.java.lang.CatchRunnable;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagView;
import org.kalypso.ogc.sensor.diagview.DiagViewCurve;
import org.kalypso.ogc.sensor.template.IObsViewEventListener;
import org.kalypso.ogc.sensor.template.ObsViewEvent;
import org.kalypso.ogc.sensor.template.ObsViewItem;

/**
 * @author schlienger
 */
public class ObservationChart extends JFreeChart implements IObsViewEventListener
{
  protected static final Logger LOGGER = Logger.getLogger( ObservationChart.class.getName() );

  private final DiagView m_view;

  /**
   * Creates an ObservationChart
   * 
   * @param template
   * @throws SensorException
   */
  public ObservationChart( final DiagView template ) throws SensorException
  {
    super( template.getTitle(), JFreeChart.DEFAULT_TITLE_FONT, ChartFactory
        .createObservationPlot( template ), template.isShowLegend() );

    m_view = template;

    if( template.isShowLegend() )
    {
      final StandardLegend leg = new StandardLegend();
      leg.setTitle( template.getLegendName() );

      setLegend( leg );
    }

    // removed in this.dispose()
    m_view.addObsViewEventListener( this );
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
  protected void clearChart()
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

//        // ADD A CURVE
//        if( evt.getObject() instanceof DiagViewCurve )
//        {
//          final DiagViewCurve curve = (DiagViewCurve)evt.getObject();
//
//          if( evt.isType( ObsViewEvent.TYPE_ADD ) )
//          {
//           LOGGER.info( "addcurve" );
//            obsPlot.addCurve( curve );
//          }
//          else if( evt.isType( ObsViewEvent.TYPE_REMOVE ) )
//          {
//            LOGGER.info( "removecurve" );
//            obsPlot.removeCurve( curve );
//          }
//          else if( evt.getType() == ObsViewEvent.TYPE_REFRESH )
//          {
//            LOGGER.info( "refresh:" );
//            LOGGER.info( "removecurve" );
//            obsPlot.removeCurve( curve );
//            LOGGER.info( "addcurve" );
//            obsPlot.addCurve( curve );
//            //          fireChartChanged();
//          }
//        }

        // This is not thread safe!
        // better always refresh tha whola kaboodle
        
        DiagView view = null;
        if( evt.getObject() instanceof DiagView )
          view = (DiagView)evt.getObject();
        else if( evt.getObject() instanceof DiagViewCurve )
          view = (DiagView)((DiagViewCurve)evt.getObject()).getView();
          
        
        // REFRESH LIST OF THEMES
        // REMOVE ALL THEMES
        if( evt.getType() == ObsViewEvent.TYPE_REMOVE_ALL )
          clearChart();
        else // if( evt.getType() == ObsViewEvent.TYPE_REFRESH && evt.getObject() instanceof DiagView )
        {
          clearChart();

          final ObsViewItem[] items = view.getItems();
          for( int i = 0; i < items.length; i++ )
          {
            final DiagViewCurve curve = (DiagViewCurve)items[i];
            obsPlot.addCurve( curve );
          }
        }
      }
    };

    try
    {
      if( !SwingUtilities.isEventDispatchThread() )
        SwingUtilities.invokeLater( runnable );
      else
        runnable.run();

      if( runnable.getThrown() != null )
        throw runnable.getThrown();
    }
    catch( Throwable e )
    {
      // TODO: hier kann ne Nullpointer exception geben (es gibt nicht immer ein
      // activeWokbenchWindow)
      MessageDialog.openError( Workbench.getInstance().getActiveWorkbenchWindow().getShell(),
          "Aktualisierungsfehler", e.toString() );
    }
  }
}