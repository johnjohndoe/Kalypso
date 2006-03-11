/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ogc.gml.map.widgets;

import java.awt.Point;

import org.eclipse.jface.viewers.ISelection;
import org.kalypso.ogc.gml.KalypsoFeatureThemeSelection;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Object;

/**
 * @author kuepfer
 */
public class EditGeometryWidget extends AbstractWidget implements IWidget
{
  private GM_Object m_Geometry;

  /**
   * @param geometryClass
   */
  public EditGeometryWidget( String name, String toolTip )
  {
    super( name, toolTip );
    m_Geometry = getSelectedGeometry( getMapPanel() );
    if( m_Geometry == null )
      finish();
  }

  /**
   * @param mapPanel
   * @return
   */
  private GM_Object getSelectedGeometry( MapPanel mapPanel )
  {
    ISelection selection = mapPanel.getSelection();
    if( !selection.isEmpty() )
    {
      if( selection instanceof KalypsoFeatureThemeSelection )
      {
        KalypsoFeatureThemeSelection ftSelection = (KalypsoFeatureThemeSelection)selection;
        Object firstElement = ftSelection.getFirstElement();
        if( firstElement instanceof Feature )
        {
          Feature feature = (Feature)firstElement;
          GM_Object geometryProperty = feature.getDefaultGeometryProperty();
          return geometryProperty;
        }
      }
    }
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#perform()
   */
  public void perform()
  {
  // TODO Auto-generated method stub
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( Point p )
  {
  // TODO Auto-generated method stub
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( Point p )
  {
    moved( p );
  }

//  private void drawGeometry( final Graphics g )
//  {
//    if( m_Geometry != null )
//    {
//      if( m_Geometry.equals( GeometryUtilities.getPolygonClass() ) )
//      {
//        MapPanel mapPanel = getMapPanel();
//        GeoTransform projection = mapPanel.getProjection();
//        GM_Surface geom = (GM_Surface)m_Geometry;
//      }
//    }
//  }
}