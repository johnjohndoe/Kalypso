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
package org.kalypso.ogc.gml.map.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.ChangeExtentCommand;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ui.editor.mapeditor.actiondelegates.WidgetActionPart;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Thomas Jung
 */
public class ZoomToActiveLayerHandler extends AbstractHandler implements IHandler
{
  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final IWorkbenchPart part = (IWorkbenchPart) context.getVariable( ISources.ACTIVE_PART_NAME );
    if( part == null )
      throw new ExecutionException( Messages.getString("org.kalypso.ogc.gml.map.handlers.ZoomToActiveLayerHandler.0") ); //$NON-NLS-1$

    final MapPanel mapPanel = (MapPanel) part.getAdapter( MapPanel.class );
    if( mapPanel == null )
      throw new ExecutionException( Messages.getString("org.kalypso.ogc.gml.map.handlers.ZoomToActiveLayerHandler.1") ); //$NON-NLS-1$

    final IKalypsoTheme activeTheme = mapPanel.getMapModell().getActiveTheme();
    if( activeTheme == null )
      throw new ExecutionException( Messages.getString("org.kalypso.ogc.gml.map.handlers.ZoomToActiveLayerHandler.2") ); //$NON-NLS-1$

// final IKalypsoFeatureTheme ft = (IKalypsoFeatureTheme) activeTheme;
// final FeatureList featureList = ft.getFeatureList();
// featureList.invalidate();
// for( Object object : featureList )
// {
// if( object instanceof Feature )
// ((Feature)object).invalidEnvelope();
// }

    final GM_Envelope zoomBox = activeTheme.getFullExtent();
    if( zoomBox == null )
      throw new ExecutionException( Messages.getString("org.kalypso.ogc.gml.map.handlers.ZoomToActiveLayerHandler.3") ); //$NON-NLS-1$

    GM_Envelope wishBBox = null;

    final GM_Position zoomMax = zoomBox.getMax();
    final GM_Position zoomMin = zoomBox.getMin();

    final double newMaxX = zoomMax.getX() + (zoomMax.getX() - zoomMin.getX()) / 20;
    final double newMinX = zoomMin.getX() - (zoomMax.getX() - zoomMin.getX()) / 20;

    final double newMaxY = zoomMax.getY() + (zoomMax.getY() - zoomMin.getY()) / 20;
    final double newMinY = zoomMin.getY() - (zoomMax.getY() - zoomMin.getY()) / 20;

    final GM_Position newMin = GeometryFactory.createGM_Position( newMinX, newMinY );
    final GM_Position newMax = GeometryFactory.createGM_Position( newMaxX, newMaxY );

    wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax, zoomBox.getCoordinateSystem() );

    new WidgetActionPart( part ).postCommand( new ChangeExtentCommand( mapPanel, wishBBox ), null );

    return Status.OK_STATUS;
  }
}
