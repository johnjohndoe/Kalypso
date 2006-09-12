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

import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * Lets the user replace an existing geometry of a given feature with a new edited one.
 * 
 * @author Holger Albert
 */
public class EditFeatureGeometryWidget extends AbstractFeatureGeometeryWidget
{
  // private final Class m_apreferedGeometryClass;

  public EditFeatureGeometryWidget( final String name, final String toolTip /* , final Class geometryClass */)
  {
    super( name, toolTip );

    // m_apreferedGeometryClass = geometryClass;

    update( getMapPanel() );
  }

  @Override
  protected Object createFeatureToEdit( final IKalypsoFeatureTheme theme )
  {
    final IFeatureSelectionManager selectionManager = theme.getSelectionManager();
    final Feature feature = FeatureSelectionHelper.getFirstFeature( selectionManager );

    final CommandableWorkspace workspace = theme.getWorkspace();

    final IValuePropertyType geometryProperty;
    if( feature == null )
      geometryProperty = null;
    else
      geometryProperty = GeometryUtilities.findGeometryProperty( feature.getFeatureType(), null );

    return new FeatureToEdit( workspace, feature, geometryProperty );
  }

  @Override
  protected void editFeature( final GM_Object validGeometryValue, final Object toEdit ) throws Exception
  {
    final FeatureToEdit featureToEdit = (FeatureToEdit) toEdit;

    final CommandableWorkspace workspace = featureToEdit.getWorkspace();
    final Feature feature = featureToEdit.getFeature();
    final IValuePropertyType geometryProperty = featureToEdit.getGeometryProperty();

    final FeatureChange change = new FeatureChange( feature, geometryProperty, validGeometryValue );
    final ICommand command = new ChangeFeaturesCommand( workspace, new FeatureChange[] { change } );
    workspace.postCommand( command );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractCreateGeometeryWidget#getGeometryClass()
   */
  @Override
  protected Class getGeometryClass( )
  {
    final FeatureToEdit featureToEdit = (FeatureToEdit) getFeatureToEdit();

    return featureToEdit.getGeometryProperty().getValueClass();
  }

  private final static class FeatureToEdit
  {
    private final IValuePropertyType m_geometryProperty;

    private final Feature m_feature;

    private final CommandableWorkspace m_workspace;

    public FeatureToEdit( final CommandableWorkspace workspace, final Feature feature, final IValuePropertyType geometryProperty )
    {
      m_workspace = workspace;
      m_feature = feature;
      m_geometryProperty = geometryProperty;
    }

    public Feature getFeature( )
    {
      return m_feature;
    }

    public IValuePropertyType getGeometryProperty( )
    {
      return m_geometryProperty;
    }

    public CommandableWorkspace getWorkspace( )
    {
      return m_workspace;
    }
  }
}
