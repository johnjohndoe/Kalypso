/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.ogc.gml.map.widgets;

import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author doemming
 * @deprecated Use SelectWidgetHandler stuff instead.
 */
@Deprecated
public class WidgetHelper
{
  private WidgetHelper( )
  {
    // not to instantiate
  }

  public static IWidget createWidget( final String widgetID )
  {
    if( MapPanel.WIDGET_ZOOM_IN.equals( widgetID ) )
      return new ZoomInWidget( "zoom in", "" );

    if( MapPanel.WIDGET_ZOOM_IN_RECT.equals( widgetID ) )
      return new ZoomInByRectWidget();

    if( MapPanel.WIDGET_PAN.equals( widgetID ) )
      return new PanToWidget();

    if( widgetID.startsWith( MapPanel.WIDGET_EDIT_FEATURE_GEOMETRY ) )
      return new EditFeatureGeometryWidget( Messages.getString( "org.kalypso.ogc.gml.map.widgets.WidgetHelper.2" ), Messages.getString( "org.kalypso.ogc.gml.map.widgets.WidgetHelper.3" ), null, null, null );

    if( widgetID.startsWith( MapPanel.WIDGET_CREATE_FEATURE_WITH_POINT ) )
      return new CreateGeometeryWidget2( Messages.getString( "org.kalypso.ogc.gml.map.widgets.WidgetHelper.4" ), "", GeometryUtilities.getPointClass() );

    if( widgetID.startsWith( MapPanel.WIDGET_CREATE_FEATURE_WITH_LINESTRING ) )
      return new CreateGeometeryWidget2( "create Geometry", "", GeometryUtilities.getLineStringClass() );

    if( widgetID.startsWith( MapPanel.WIDGET_CREATE_FEATURE_WITH_POLYGON ) )
      return new CreateGeometeryWidget2( "create Geometry", "", GeometryUtilities.getPolygonClass() );

    if( widgetID.startsWith( MapPanel.WIDGET_CREATE_FEATURE_WITH_GEOMETRY ) )
      return new CreateGeometeryWidget2( "create Geometry", "", null );

    if( widgetID.equals( MapPanel.WIDGET_EDIT_GEOMETRY ) )
      return new EditGeometryWidget( "edit Geometry", "" );

    if( MapPanel.WIDGET_SELECT.equals( widgetID ) )
      return new SelectWidget();

    if( MapPanel.WIDGET_UNSELECT.equals( widgetID ) )
      return new UnSelectWidget( "unselect", "" );
    if( MapPanel.WIDGET_TOGGLE_SELECT.equals( widgetID ) )
      return new ToggleSelectWidget( "toggle selection", "" );

    if( MapPanel.WIDGET_SINGLE_SELECT.equals( widgetID ) )
      return new SingleElementSelectWidget( "single select", "" );

    return null;
  }
}
