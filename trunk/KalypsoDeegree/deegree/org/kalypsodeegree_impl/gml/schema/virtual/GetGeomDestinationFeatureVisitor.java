package org.deegree_impl.gml.schema.virtual;

import java.util.ArrayList;
import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureAssociationTypeProperty;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.feature.FeatureVisitor;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree.model.geometry.GM_Object;

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

public class GetGeomDestinationFeatureVisitor implements FeatureVisitor
{

  private final GMLWorkspace m_workspace;

  private final String m_initialPropName;

  private final int m_maxLevel;

  private final List m_result;

  /*
   * 
   * @author doemming
   */
  public GetGeomDestinationFeatureVisitor( GMLWorkspace workspace, String initialPropName,
      int maxLevel )
  {
    m_workspace = workspace;
    m_initialPropName = initialPropName;
    m_maxLevel = maxLevel;
    m_result = new ArrayList();
  }

  public GM_Object[] getGeometryDestinations()
  {
    return (GM_Object[])m_result.toArray( new GM_Object[m_result.size()] );
  }

  /**
   * @see org.deegree.model.feature.FeatureVisitor#visit(org.deegree.model.feature.Feature)
   */
  public boolean visit( Feature f )
  {
    visit( f, m_initialPropName, 0 );
    return false;
  }

  private void visit( Feature feature, int level )
  {
    FeatureTypeProperty[] properties = feature.getFeatureType().getProperties();
    for( int i = 0; i < properties.length; i++ )
    {
      if( properties[i] instanceof FeatureAssociationTypeProperty )
        visit( feature, properties[i].getName(), level );
    }
  }

  private void visit( Feature feature, String linkProp, int level )
  {

    // get childs
    final Feature[] destFEs;

    if( feature.getFeatureType().getMaxOccurs( linkProp ) > 1 )
    {
      destFEs = m_workspace.resolveLinks( feature, linkProp );
    }
    else
    {
      Feature destFE = m_workspace.resolveLink( feature, linkProp );
      if(destFE!=null)
      destFEs = new Feature[]
      { destFE};
      else
        destFEs=new Feature[0];
    }
    // process childs
    for( int i = 0; i < destFEs.length; i++ )
    {
      if( destFEs[i].getFeatureType().getDefaultGeometryProperty() != null )
      {
        final GM_Object geom = destFEs[i].getDefaultGeometryProperty();
        if( geom != null )
          m_result.add( geom );
      }
      else
      {
        level++;
        if( level < m_maxLevel )
          visit( destFEs[i], level );
      }
    }
  }
}

