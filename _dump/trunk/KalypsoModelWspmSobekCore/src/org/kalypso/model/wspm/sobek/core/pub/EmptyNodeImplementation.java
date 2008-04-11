/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.sobek.core.pub;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.IModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.model.AbstractNode;
import org.kalypso.model.wspm.sobek.core.model.Branch;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;

/**
 * Objects of class EmptyNodeImplementation will be returned for all created nodes which this plugin doesn't recognize
 * 
 * @author kuch
 */
public class EmptyNodeImplementation extends AbstractNode
{
  public EmptyNodeImplementation( final IModelMember model, final Feature node )
  {
    super( model, node );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#delete()
   */
  public void delete( ) throws Exception
  {
    FeatureUtils.deleteFeature( getModel().getWorkspace(), getFeature() );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getType()
   */
  public TYPE getType( )
  {
    return null;
  }

  public IBranch getLinkToBranch( )
  {
    IPropertyType myProperty = null;
    final IPropertyType[] properties = getFeature().getFeatureType().getProperties();
    for( final IPropertyType propertyType : properties )
    {
      if( ISobekConstants.F_LN_LINKS_TO_BRANCH.equals( propertyType.getQName().getLocalPart() ) )
      {
        myProperty = propertyType;
        break;
      }

    }

    final Object objBranch = getFeature().getProperty( myProperty );
    final Feature feature = FeatureUtils.resolveFeature( getModel().getWorkspace(), objBranch );

    return new Branch( getModel(), feature );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#isEmpty()
   */
  public boolean isEmpty( )
  {
    if( getLinkToBranch() == null )
      return true;

    return false;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getSperrzone(org.kalypso.model.wspm.sobek.core.interfaces.IBranch)
   */
  public GM_Object[] getSperrzone( final IBranch branch )
  {
    try
    {
      final GM_Point location = getLocation();
      final Geometry geometry = JTSAdapter.export( location );
      final QName name = getFeature().getFeatureType().getQName();

      if( ISobekConstants.QN_NOFDP_RETARDIN_BASIN_NODE.equals( name ) )
      {
        return new GM_Object[] { JTSAdapter.wrap( geometry.buffer( 15 ) ) };
      }

      return new GM_Object[] { JTSAdapter.wrap( geometry.buffer( 10 ) ) };
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }

    return new GM_Object[] {};
  }
}
