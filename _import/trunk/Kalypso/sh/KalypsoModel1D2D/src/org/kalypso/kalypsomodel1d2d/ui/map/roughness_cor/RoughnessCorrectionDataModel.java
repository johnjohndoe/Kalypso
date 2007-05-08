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
package org.kalypso.kalypsomodel1d2d.ui.map.roughness_cor;

import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.MapModell;
import org.kalypso.ogc.gml.widgets.IWidget;

/**
 * Data model for Rougness Correction map widget
 * 
 * @author Patrice Congo
 *
 */
public class RoughnessCorrectionDataModel extends KeyBasedDataModel
{
  /**
   * Key for the current Map panel
   */
  static final String KEY_MAP_PANEL  ="_MapPanel_";
  
  /**
   * Key for the current map model
   */
  static final String KEY_MAP_MODELL = "_Map_Model_";
  
  /**
   * Key for a boolean flag that govern the display of
   * roughess info while the mouse is moving on the map
   */
  static final String KEY_SHOWING_LIVE_ROUGHNESS_INFO ="_show_live_roughness_info";
  
  static final String KEY_CURRENT_WIDGET_STRATEGY = "_current_widget_strategy_";
  
  /**
   * the data model key 
   */
  private static final String[] KEYS = 
            { MapPanel.class.toString(), MapModell.class.toString()}; 

  
  public RoughnessCorrectionDataModel( )
  {
    super( 
          new String[]{ 
              KEY_MAP_MODELL, KEY_MAP_PANEL, 
              KEY_SHOWING_LIVE_ROUGHNESS_INFO}, 
          null );
    
  }

  public IMapModell getMapModell( )
  {
    return (IMapModell) getData( KEY_MAP_MODELL );
  }

  public void setMapModell( IMapModell mapModell )
  {
    setData( KEY_MAP_MODELL, mapModell );
  }

  public MapPanel getMapPanel( )
  {
    return (MapPanel) getData( KEY_MAP_PANEL );
  }

  public void setMapPanel( MapPanel mapPanel )
  {
    setData( KEY_MAP_PANEL, mapPanel );
  }
  
  public boolean isShowingLiveRoughnessInfoEnable( )
  {
    Object data = getData( KEY_SHOWING_LIVE_ROUGHNESS_INFO );
    
    if( data instanceof Boolean )
    {
      return ((Boolean)data).booleanValue();
    }
    else
    {
      return false;
    }
  }
  
  public void setShowingLiveRoughnessInfoEnable( 
                            boolean isShowingRoughnessInfoEnable )
  {
    Boolean isEnable =
        isShowingRoughnessInfoEnable? Boolean.TRUE:Boolean.FALSE;
    
    setData( 
        KEY_SHOWING_LIVE_ROUGHNESS_INFO,
        isEnable );
    
  }
  
  public IWidget getCurrentWidgetStrategy()
  {
    return (IWidget) getData( KEY_CURRENT_WIDGET_STRATEGY );
  }
  
  public void setCurrentWidgetStrategy(IWidget currentStrategy )
  {
    setData( KEY_CURRENT_WIDGET_STRATEGY, currentStrategy );
  }
  
  
}
