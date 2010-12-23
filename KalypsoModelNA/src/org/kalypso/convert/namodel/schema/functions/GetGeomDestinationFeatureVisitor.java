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
package org.kalypso.convert.namodel.schema.functions;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;

/**
 * @author doemming
 */
public class GetGeomDestinationFeatureVisitor implements FeatureVisitor
{
  private final GMLWorkspace m_workspace;

  private final IRelationType m_prelationPT;

  private final int m_maxLevel;

  private final List<GM_Object> m_result;

  public GetGeomDestinationFeatureVisitor( GMLWorkspace workspace, IRelationType initialPropName, int maxLevel )
  {
    m_workspace = workspace;
    m_prelationPT = initialPropName;
    m_maxLevel = maxLevel;
    m_result = new ArrayList<GM_Object>();
  }

  public GM_Object[] getGeometryDestinations( )
  {
    return m_result.toArray( new GM_Object[m_result.size()] );
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  @Override
  public boolean visit( Feature f )
  {
    visit( f, m_prelationPT, 0 );
    return false;
  }

  private void visit( Feature feature, int level )
  {
    level++;
    IPropertyType[] properties = feature.getFeatureType().getProperties();
    for( int i = 0; i < properties.length; i++ )
    {
      if( properties[i] instanceof IRelationType )
        visit( feature, (IRelationType) properties[i], level );
    }
  }

  private void visit( Feature feature, IRelationType linkProp, int level )
  {
    // get childs
    final Feature[] destFEs;

    if( linkProp.isList() )
      destFEs = m_workspace.resolveLinks( feature, linkProp );
    else
    {
      Feature destFE = m_workspace.resolveLink( feature, linkProp );
      if( destFE != null )
        destFEs = new Feature[] { destFE };
      else
        destFEs = new Feature[0];
    }
    // process childs
    for( int i = 0; i < destFEs.length; i++ )
    {
      if( destFEs[i] == null )
      {
        System.out.println( Messages.getString("org.kalypso.convert.namodel.schema.functions.GetGeomDestinationFeatureVisitor.0") ); //$NON-NLS-1$
        continue;
      }

      final IFeatureType featureType = destFEs[i].getFeatureType();
      if( featureType.getDefaultGeometryProperty() != null )
      {
        final GM_Object geom = destFEs[i].getDefaultGeometryProperty();
        if( geom != null )
          m_result.add( geom );
      }
      else
      {
        if( level <= m_maxLevel )
          visit( destFEs[i], level );
      }
    }
  }
}