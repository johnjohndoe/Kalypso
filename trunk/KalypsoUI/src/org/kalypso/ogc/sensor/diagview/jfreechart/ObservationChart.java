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

import java.util.Collection;
import java.util.Iterator;

import javax.swing.SwingUtilities;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.internal.Workbench;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.StandardLegend;
import org.kalypso.java.lang.CatchRunnable;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagViewCurve;
import org.kalypso.ogc.sensor.diagview.DiagViewTemplate;
import org.kalypso.ogc.sensor.diagview.DiagViewTheme;
import org.kalypso.ogc.sensor.template.ITemplateEventListener;
import org.kalypso.ogc.sensor.template.TemplateEvent;

/**
 * @author schlienger
 */
public class ObservationChart extends JFreeChart implements
    ITemplateEventListener
{
  private final DiagViewTemplate m_template;

  /**
   * Creates an ObservationChart
   * 
   * @param template
   * @throws SensorException
   */
  public ObservationChart( final DiagViewTemplate template )
      throws SensorException
  {
    super( template.getTitle(), JFreeChart.DEFAULT_TITLE_FONT, ChartFactory
        .createObservationPlot( template ), template.isShowLegend() );
    
    m_template = template;
    
    // removed in this.dispose()
    m_template.addTemplateEventListener( this );
    
    if( template.isShowLegend() )
    {
      final StandardLegend leg = new StandardLegend();
      leg.setTitle( template.getLegendName() );

      setLegend( leg );
    }
  }

  /**
   * Disposes the chart.
   */
  public void dispose( )
  {
    m_template.removeTemplateEventListener( this );
    
    clearChart();
  }

  /**
   * Clears the curves in the chart
   */
  protected void clearChart( )
  {
    getObservationPlot().clearCurves();
  }
  
  /**
   * @return plot casted as obs plot
   */
  public ObservationPlot getObservationPlot()
  {
    return (ObservationPlot) getPlot();
  }

  /**
   * @see org.kalypso.ogc.sensor.template.ITemplateEventListener#onTemplateChanged(org.kalypso.ogc.sensor.template.TemplateEvent)
   */
  public void onTemplateChanged( final TemplateEvent evt )
  {   
    final CatchRunnable runnable = new CatchRunnable()
    {
      protected void runIntern( ) throws Throwable
      {
        final ObservationPlot obsPlot = getObservationPlot();
        
        // ADD A CURVE
        if( evt.isType( TemplateEvent.TYPE_ADD )
            && evt.getObject() instanceof DiagViewCurve )
        {
          obsPlot.addCurve( (DiagViewCurve) evt
              .getObject() );
        }

        // REMOVE A CURVE
        if( evt.isType( TemplateEvent.TYPE_REMOVE )
            && evt.getObject() instanceof DiagViewCurve )
        {
          obsPlot.removeCurve( (DiagViewCurve) evt
              .getObject() );
        }

        // SHOW/HIDE A CURVE
        if( evt.isType( TemplateEvent.TYPE_SHOW_STATE )
            && evt.getObject() instanceof DiagViewCurve )
        {
          final DiagViewCurve curve = (DiagViewCurve) evt.getObject();
          
          if( curve.isShown() )
            obsPlot.addCurve( curve );
          else
            obsPlot.removeCurve( curve );
        }
        
        // REFRESH LIST OF THEMES
        if( evt.getType() == TemplateEvent.TYPE_REFRESH
            && evt.getObject() instanceof Collection )
        {
          clearChart();

          final Iterator itThemes = ((Collection) evt.getObject()).iterator();
          while( itThemes.hasNext() )
          {
            final DiagViewTheme theme = (DiagViewTheme) itThemes
                .next();
            final Iterator it = theme.getCurves().iterator();
            while( it.hasNext() )
              obsPlot
                  .addCurve( (DiagViewCurve) it.next() );
          }
          
          fireChartChanged();
        }
        
        // REFRESH ONE THEME
        if( evt.getType() == TemplateEvent.TYPE_REFRESH
            && evt.getObject() instanceof DiagViewTheme )
        {
          final DiagViewTheme theme = (DiagViewTheme) evt.getObject();
          final Iterator it = theme.getCurves().iterator();
          while( it.hasNext() )
          {
            final DiagViewCurve crv = (DiagViewCurve) it.next();
            obsPlot.removeCurve( crv );
            obsPlot.addCurve( crv );
          }
          
          fireChartChanged();
        }

        // REMOVE ALL THEMES
        if( evt.getType() == TemplateEvent.TYPE_REMOVE_ALL )
          clearChart();
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
      // TODO: hier kann ne Nullpointer exception geben (es gibt nicht immer ein activeWokbenchWindow)
      MessageDialog.openError( Workbench.getInstance()
          .getActiveWorkbenchWindow().getShell(), "Aktualisierungsfehler", e
          .toString() );
    }
  }
}