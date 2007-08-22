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

import java.util.HashMap;
import java.util.Map;

import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * Lets the user edit a new geometry on the map and creates a new feature with this geometry.
 * 
 * @author Holger Albert
 */
public class CreateGeometeryWidget2 extends AbstractFeatureGeometeryWidget
{
  private final Class< ? extends GM_Object> m_apreferedGeometryClass;

  public CreateGeometeryWidget2( final String name, final String toolTip, final Class< ? extends GM_Object> geometryClass )
  {
    super( name, toolTip );

    m_apreferedGeometryClass = geometryClass;

    update( getMapPanel() );
  }

  @Override
  protected Object createFeatureToEdit( final IKalypsoFeatureTheme theme )
  {
    final CommandableWorkspace workspace = theme.getWorkspace();
    final IFeatureType featureType = theme.getFeatureType();

    final FeatureList featureList = theme.getFeatureList();
    final Feature parentFeature = featureList.getParentFeature();
    final IRelationType linkrelationType = featureList.getParentFeatureTypeProperty();

    final IValuePropertyType geometryProperty = GeometryUtilities.findGeometryProperty( featureType, m_apreferedGeometryClass );

    if( geometryProperty == null )
      return null;

    return new FeatureToEdit( parentFeature, linkrelationType, featureType, workspace, geometryProperty );
  }

  @Override
  protected void editFeature( final GM_Object validGeometryValue, final Object toEdit ) throws Exception
  {
    final FeatureToEdit featureToEdit = (FeatureToEdit) toEdit;

    final Map<IPropertyType, Object> valueMap = new HashMap<IPropertyType, Object>();
    valueMap.put( featureToEdit.getGeometryProperty(), validGeometryValue );

    // TODO ask for substitutions
    final CommandableWorkspace workspace = featureToEdit.getWorkspace();

    final ICommand command = new AddFeatureCommand( workspace, featureToEdit.getFeatureType(), featureToEdit.getParentFeature(), featureToEdit.getLinkProperty(), 0, valueMap, null, 0 );
    workspace.postCommand( command );
  }

  @Override
  protected Class< ? extends GM_Object> getGeometryClass( )
  {
    return m_apreferedGeometryClass;
  }

  private static final class FeatureToEdit
  {
    private final Feature m_parentFeature;

    private final IRelationType m_linkProperty;

    private final IFeatureType m_featureType;

    private final CommandableWorkspace m_workspace;

    private final IValuePropertyType m_geometryProperty;

    public FeatureToEdit( final Feature parentFeature, final IRelationType linkProperty, final IFeatureType featureType, final CommandableWorkspace workspace, final IValuePropertyType geometryProperty )
    {
      m_parentFeature = parentFeature;
      m_linkProperty = linkProperty;
      m_featureType = featureType;
      m_workspace = workspace;
      m_geometryProperty = geometryProperty;
    }

    public IFeatureType getFeatureType( )
    {
      return m_featureType;
    }

    public IValuePropertyType getGeometryProperty( )
    {
      return m_geometryProperty;
    }

    public IRelationType getLinkProperty( )
    {
      return m_linkProperty;
    }

    public Feature getParentFeature( )
    {
      return m_parentFeature;
    }

    public CommandableWorkspace getWorkspace( )
    {
      return m_workspace;
    }
  }

}
