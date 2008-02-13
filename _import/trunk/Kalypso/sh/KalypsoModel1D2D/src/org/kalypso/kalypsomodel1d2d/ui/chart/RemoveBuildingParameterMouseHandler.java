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
package org.kalypso.kalypsomodel1d2d.ui.chart;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Display;
import org.kalypso.chart.framework.model.IChartModel;
import org.kalypso.chart.framework.model.layer.EditInfo;
import org.kalypso.chart.framework.model.layer.IChartLayer;
import org.kalypso.chart.framework.model.layer.ILayerManager;
import org.kalypso.chart.framework.view.ChartComposite;
import org.kalypso.chart.framework.view.IChartDragHandler;

/**
 * @author Gernot Belger
 */
public class RemoveBuildingParameterMouseHandler implements IChartDragHandler
{
  private final ChartComposite m_chartComposite;

  public RemoveBuildingParameterMouseHandler( final ChartComposite chartComposite )
  {
    m_chartComposite = chartComposite;
  }

  /**
   * @see org.eclipse.swt.events.MouseListener#mouseDoubleClick(org.eclipse.swt.events.MouseEvent)
   */
  public void mouseDoubleClick( final MouseEvent e )
  {
    final BuildingParameterLayer layer = findLayer();
    final EditInfo info = layer.getEditInfo( new Point( e.x, e.y ) );

    // TODO: move into layer or generalize
    layer.edit( info );

  }

  /**
   * @see org.eclipse.swt.events.MouseListener#mouseDown(org.eclipse.swt.events.MouseEvent)
   */
  public void mouseDown( final MouseEvent e )
  {
  }

  /**
   * @see org.eclipse.swt.events.MouseListener#mouseUp(org.eclipse.swt.events.MouseEvent)
   */
  public void mouseUp( final MouseEvent e )
  {

  }

  /**
   * @see org.eclipse.swt.events.MouseMoveListener#mouseMove(org.eclipse.swt.events.MouseEvent)
   */
  public void mouseMove( final MouseEvent e )
  {
    final Point point = new Point( e.x, e.y );

    // Show tooltip
    final BuildingParameterLayer layer = findLayer();
    final EditInfo info = layer.getEditInfo( point );
    // HACK/TODO: this is ugly and should not be necessary: there should be another mechanism, so that mouse handler can
    // draw tooltips (or other things) on the map.

    if( info == null )
    {
      m_chartComposite.setCursor( e.display.getSystemCursor( SWT.CURSOR_ARROW ) );
      layer.setTooltip( null, null );
    }
    else
    {
      m_chartComposite.setCursor( e.display.getSystemCursor( SWT.CURSOR_HAND ) );
      layer.setTooltip( info.text + " (Doppelklick zum Löschen)", point );
    }

  }

  private BuildingParameterLayer findLayer( )
  {
    final IChartModel model = m_chartComposite.getModel();
    final ILayerManager layerManager = model.getLayerManager();
    final IChartLayer< ? , ? >[] layers = layerManager.getLayers();
    for( final IChartLayer< ? , ? > chartLayer : layers )
    {
      if( chartLayer instanceof BuildingParameterLayer )
        return (BuildingParameterLayer) chartLayer;
    }

    return null;
  }

  /**
   * @see org.kalypso.chart.framework.view.IChartDragHandler#getCursor()
   */
  public Cursor getCursor( )
  {
    // TODO: do not use default display; may be null or whatever
    return Display.getDefault().getSystemCursor( SWT.CURSOR_HAND );
  }

}
