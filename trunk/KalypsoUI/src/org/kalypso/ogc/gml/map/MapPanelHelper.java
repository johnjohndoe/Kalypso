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
package org.kalypso.ogc.gml.map;

import org.eclipse.swt.widgets.Shell;
import org.kalypso.ogc.gml.map.widgets.EditFeatureWidget;
import org.kalypso.ogc.gml.widgets.CreateGeometryFeatureWidget;
import org.kalypso.ogc.gml.widgets.PanToWidget;
import org.kalypso.ogc.gml.widgets.SelectWidget;
import org.kalypso.ogc.gml.widgets.SingleElementSelectWidget;
import org.kalypso.ogc.gml.widgets.ToggleSelectWidget;
import org.kalypso.ogc.gml.widgets.UnSelectWidget;
import org.kalypso.ogc.gml.widgets.ZoomInByRectWidget;
import org.kalypso.ogc.gml.widgets.ZoomInWidget;

/**
 * @author belger
 */
public class MapPanelHelper
{
  private MapPanelHelper()
  {
    // wird nicht instantitiert
  }
  
  public static final void createWidgetsForMapPanel( final Shell shell, final MapPanel panel )
  {
    panel.setWidget( MapPanel.WIDGET_ZOOM_IN, new ZoomInWidget() );
    panel.setWidget( MapPanel.WIDGET_ZOOM_IN_RECT, new ZoomInByRectWidget() );
    panel.setWidget( MapPanel.WIDGET_PAN, new PanToWidget() );
    panel.setWidget( MapPanel.WIDGET_EDIT_FEATURE, new EditFeatureWidget( shell ) );
    panel.setWidget( MapPanel.WIDGET_CREATE_FEATURE, new CreateGeometryFeatureWidget() );
    panel.setWidget( MapPanel.WIDGET_SELECT, new SelectWidget() );
    panel.setWidget( MapPanel.WIDGET_UNSELECT, new UnSelectWidget() );
    panel.setWidget( MapPanel.WIDGET_TOGGLE_SELECT, new ToggleSelectWidget() );
    panel.setWidget( MapPanel.WIDGET_SINGLE_SELECT, new SingleElementSelectWidget() );
  }

}
