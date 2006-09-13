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
package org.kalypso.ui.editor.mapeditor.actiondelegates;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.widgets.WidgetHelper;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypsodeegree.model.feature.Feature;

/**
 * abstract delegate to create features drawing geometries on map
 * 
 * @author doemming
 */
public class EditFeatureGeometryWidgetDelegate extends AbstractGisMapEditorActionDelegate
{
  /**
   * @param geometryClass
   *          the geometryclass to create or <code>null</code> for "any geometry"
   */
  public EditFeatureGeometryWidgetDelegate( )
  {
    super( WidgetHelper.getWidget( MapPanel.WIDGET_EDIT_FEATURE_GEOMETRY ) );
  }

  @Override
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    action.setEnabled( fitsToAction( selection ) );
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.actiondelegates.AbstractGisMapEditorActionDelegate#refreshAction(org.eclipse.jface.action.IAction)
   */
  @Override
  protected void refreshAction( final IAction action, final ISelection selection )
  {
    super.refreshAction( action, selection );

    if( action != null )
      action.setEnabled( fitsToAction( selection ) );
  }

  public boolean fitsToAction( final ISelection selection )
  {
    final WidgetActionPart part = getPart();
    if( part != null )
    {
      final MapPanel mapPanel = part.getMapPanel();
      if( mapPanel == null )
        return false;

      final IMapModell mapModell = mapPanel.getMapModell();
      if( mapModell == null )
        return false;

      final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
      if( activeTheme == null )
        return false;

      if( activeTheme instanceof IKalypsoFeatureTheme && selection instanceof IFeatureSelection )
      {
        final Feature firstFeature = FeatureSelectionHelper.getFirstFeature( (IFeatureSelection) selection );
        if( firstFeature != null )
        {
          final IFeatureType featureType = firstFeature.getFeatureType();
          if( featureType == null )
            return false;

          final IPropertyType[] geomProps = featureType.getAllGeomteryProperties();
          return geomProps.length > 0;
        }
      }
    }

    return false;
  }
}
