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
package org.kalypso.kalypsomodel1d2d.ui.chart;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Control;
import org.kalypso.chart.ui.editor.mousehandler.AbstractChartHandler;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;

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
        return (BuildingParameterLayer) chartLayer;
    }

    return null;
  }

  private EditInfo m_info;

  public EditBuildingParameterMouseHandler( final IChartComposite chartComposite )
  {
    super( chartComposite );
  }

  /**
   * @see org.eclipse.swt.events.MouseListener#mouseDoubleClick(org.eclipse.swt.events.MouseEvent)
   */
  @Override
  public void mouseDoubleClick( final MouseEvent e )
  {
    final BuildingParameterLayer layer = findLayer( getChart().getChartModel() );
    final EditInfo info = layer.getEditInfo( new Point( e.x, e.y ) );

    if( info.getData() != null )
      layer.delete( info );
  }

  /**
   * @see org.eclipse.swt.events.MouseListener#mouseDown(org.eclipse.swt.events.MouseEvent)
   */
  @Override
  public void mouseDown( final MouseEvent e )
  {
    final BuildingParameterLayer layer = findLayer( getChart().getChartModel() );
    final EditInfo editInfo = layer.getEditInfo( new Point( e.x, e.y ) );
    if( editInfo != null && editInfo.getData() != null )
      m_info = editInfo;
  }

  /**
   * @see org.eclipse.swt.events.MouseMoveListener#mouseMove(org.eclipse.swt.events.MouseEvent)
   */
  @Override
  public void mouseMove( final MouseEvent e )
  {
    final Point point = new Point( e.x, e.y );

    // Show tooltip
    final BuildingParameterLayer layer = findLayer( getChart().getChartModel() );
    final EditInfo info = layer.getEditInfo( point );
    // HACK/TODO: this is ugly and should not be necessary: there should be another mechanism, so that mouse handler can
    // draw tooltips (or other things) on the map.
    final Control ctrl = (Control) e.getSource();
    if( info == null )
    {
      ctrl.setCursor( e.display.getSystemCursor( SWT.CURSOR_ARROW ) );
      layer.setTooltip( null, null );
    }
    else
    {
      ctrl.setCursor( e.display.getSystemCursor( SWT.CURSOR_HAND ) );

      if( info.getData() == null )
        layer.setTooltip( info.getText() + Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.chart.EditBuildingParameterMouseHandler.0" ), point ); //$NON-NLS-1$
      else
        layer.setTooltip( info.getText() + Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.chart.EditBuildingParameterMouseHandler.1" ), point ); //$NON-NLS-1$
    }
  }

  /**
   * @see org.eclipse.swt.events.MouseListener#mouseUp(org.eclipse.swt.events.MouseEvent)
   */
  @Override
  public void mouseUp( final MouseEvent e )
  {
    final EditInfo info = m_info;

    final Point plotPoint = getChart().screen2plotPoint(new Point( e.x, e.y ) );

    if( info == null )
    {
      // Klick on cross-point?
      final BuildingParameterLayer layer = findLayer( getChart().getChartModel() );
      final EditInfo editInfo = layer.getEditInfo(plotPoint );
      if( editInfo != null && editInfo.getData() == null )
      {
        final Control ctrl = (Control) e.getSource();
        final Rectangle bounds = ctrl.getBounds();
        final int zoomFactor = 3;
        final Point point = editInfo.getPosition();
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
