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
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Control;
import org.kalypso.chart.ui.editor.commandhandler.ChartHandlerUtilities;
import org.kalypso.chart.ui.editor.mousehandler.AbstractChartHandler;

import de.openali.odysseus.chart.framework.model.IChartModel;
import de.openali.odysseus.chart.framework.model.impl.visitors.ZoomInVisitor;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.ILayerManager;
import de.openali.odysseus.chart.framework.view.IChartComposite;

/**
 * @author Gernot Belger
 */
public class EditBuildingParameterMouseHandler extends AbstractChartHandler
{
  public static BuildingParameterLayer findLayer( final IChartModel model )
  {
    final ILayerManager layerManager = model.getLayerManager();
    final IChartLayer[] layers = layerManager.getLayers();
    for( final IChartLayer chartLayer : layers )
    {
      if( chartLayer instanceof BuildingParameterLayer )
        return (BuildingParameterLayer)chartLayer;
    }

    return null;
  }

  private EditInfo m_info;

  public EditBuildingParameterMouseHandler( final IChartComposite chartComposite )
  {
    super( chartComposite );
  }

  @Override
  public void mouseDoubleClick( final MouseEvent e )
  {
    final BuildingParameterLayer layer = findLayer( getChart().getChartModel() );
    final Point plotPoint = ChartHandlerUtilities.screen2plotPoint( new Point( e.x, e.y ), getChart().getPlotRect() );
    final EditInfo info = layer.getEditInfo( plotPoint );

    if( info != null && info.getData() != null )
    {
      setToolInfo( null );
      getChart().setEditInfo( null );

      layer.delete( info );
    }
  }

  @Override
  public void mouseDown( final MouseEvent e )
  {
    final BuildingParameterLayer layer = findLayer( getChart().getChartModel() );
    final Point plotPoint = ChartHandlerUtilities.screen2plotPoint( new Point( e.x, e.y ), getChart().getPlotRect() );
    final EditInfo editInfo = layer.getEditInfo( plotPoint );
    if( editInfo != null && editInfo.getData() != null )
      m_info = editInfo;
  }

  @Override
  public void mouseMove( final MouseEvent e )
  {
    final Point point = ChartHandlerUtilities.screen2plotPoint( new Point( e.x, e.y ), getChart().getPlotRect() );

    // Show tooltip
    final BuildingParameterLayer layer = findLayer( getChart().getChartModel() );
    final EditInfo info = layer.getEditInfo( point );

    getChart().setEditInfo( info );
    setToolInfo( info );

    // HACK/TODO: this is ugly and should not be necessary: there should be another mechanism, so that mouse handler can
    // draw tooltips (or other things) on the map.
    if( info == null )
      setCursor( SWT.CURSOR_ARROW );
    else
      setCursor( SWT.CURSOR_ARROW );
  }

  @Override
  public void mouseUp( final MouseEvent e )
  {
    final EditInfo info = m_info;

    final Point plotPoint = ChartHandlerUtilities.screen2plotPoint( new Point( e.x, e.y ), getChart().getPlotRect() );

    if( info == null )
    {
      // Klick on cross-point?
      final BuildingParameterLayer layer = findLayer( getChart().getChartModel() );
      final EditInfo editInfo = layer.getEditInfo( plotPoint );
      if( editInfo != null && editInfo.getData() == null )
      {
        final Control ctrl = (Control)e.getSource();
        final Rectangle bounds = ctrl.getBounds();
        final int zoomFactor = 3;
        final Point point = ChartHandlerUtilities.plotPoint2screen( editInfo.getPosition(), getChart().getPlotRect() );
        final Point zoomMin = new Point( point.x - bounds.width / zoomFactor, point.y - bounds.height / zoomFactor );
        final Point zoomMax = new Point( point.x + bounds.width / zoomFactor, point.y + bounds.height / zoomFactor );

        final ZoomInVisitor visitor = new ZoomInVisitor( zoomMin, zoomMax );

        final IChartModel model = getChart().getChartModel();
        model.getMapperRegistry().accept( visitor );
      }

      return;
    }

    // prepare for exception
    m_info = null;

    final BuildingParameterLayer layer = findLayer( getChart().getChartModel() );
    layer.edit( plotPoint, info );
  }
}
