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
package org.kalypso.model.wspm.sobek.core.utils;

import java.util.Map;
import java.util.Properties;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ui.catalogs.FeatureTypePropertiesCatalog;
import org.kalypso.ui.catalogs.IFeatureTypePropertiesConstants;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Like the normal AddFeatureCommand, but not undoable Needed for an transparent gml_workspace, some actions should
 * always be saved an not be undoable
 * 
 * @author Dirk Kuch
 */
public class AtomarAddFeatureCommand extends AddFeatureCommand
{

  private static int geDepth( final CommandableWorkspace workspace, final IFeatureType type )
  {
    final Properties uiProperties = FeatureTypePropertiesCatalog.getProperties( workspace.getContext(), type.getQName() );
    final String depthStr = uiProperties.getProperty( IFeatureTypePropertiesConstants.FEATURE_CREATION_DEPTH, IFeatureTypePropertiesConstants.FEATURE_CREATION_DEPTH_DEFAULT );

    return Integer.parseInt( depthStr );
  }

  public AtomarAddFeatureCommand( final CommandableWorkspace workspace, final IFeatureType type, final Feature parentFeature, final IRelationType propertyName, final int pos, final Map<IPropertyType, Object> properties, final IFeatureSelectionManager selectionManager )
  {
    super( workspace, type, parentFeature, propertyName, pos, properties, selectionManager, AtomarAddFeatureCommand.geDepth( workspace, type ) );
  }

  @Override
  public boolean isUndoable( )
  {
    return false;
  }

  @Override
  public void redo( ) throws Exception
  {
  }

  @Override
  public void undo( ) throws Exception
  {
  }
}