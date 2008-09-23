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
package org.kalypso.model.wspm.sobek.core.model;

import org.apache.commons.lang.NotImplementedException;
import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.eclipse.core.runtime.Assert;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.wspm.sobek.core.i18n.Messages;
import org.kalypso.model.wspm.sobek.core.interfaces.IModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.pub.FNNodeUtils;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 * @author Dirk Kuch
 */
public abstract class AbstractNode implements INode
{
  public static double GEOM_BUFFER = 0.1;

  public static INode createNode( final IModelMember model, final TYPE nodeType, final GM_Point point ) throws Exception
  {
    final IGMLSchema schema = model.getFeature().getFeatureType().getGMLSchema();

    IFeatureType targetFeatureType;
    if( TYPE.eBoundaryNode.equals( nodeType ) )
      targetFeatureType = schema.getFeatureType( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE );
    else if( TYPE.eConnectionNode.equals( nodeType ) )
      targetFeatureType = schema.getFeatureType( ISobekConstants.QN_HYDRAULIC_CONNECTION_NODE );
    else if( TYPE.eCrossSectionNode.equals( nodeType ) )
      targetFeatureType = schema.getFeatureType( ISobekConstants.QN_HYDRAULIC_CROSS_SECTION_NODE );
    else if( TYPE.eLinkageNode.equals( nodeType ) )
      targetFeatureType = schema.getFeatureType( ISobekConstants.QN_HYDRAULIC_LINKAGE_NODE );
    else if( TYPE.eSbkStructCompoundStructure.equals( nodeType ) )
      targetFeatureType = schema.getFeatureType( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_COMPOUND_STRUCTURE );
    else if( TYPE.eSbkStructDatabaseStructure.equals( nodeType ) )
      targetFeatureType = schema.getFeatureType( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_DATABASE_STRUCTURE );
    else if( TYPE.eSbkStructGeneralStructure.equals( nodeType ) )
      targetFeatureType = schema.getFeatureType( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_GENERAL_STRUCTURE );
    else if( TYPE.eSbkStructPump.equals( nodeType ) )
      targetFeatureType = schema.getFeatureType( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_PUMP );
    else if( TYPE.eSbkStructRiverWeir.equals( nodeType ) )
      targetFeatureType = schema.getFeatureType( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_RIVER_WEIR );
    else if( TYPE.eSbkStructWeir.equals( nodeType ) )
      targetFeatureType = schema.getFeatureType( ISobekConstants.QN_HYDRAULIC_SBK_STRUCTURE_WEIR );

    else
      throw new IllegalStateException( Messages.AbstractNode_0 + nodeType.name() );

    return FNNodeUtils.createNode( model, targetFeatureType, point, nodeType );
  }

  private final Feature m_node;

  private final IModelMember m_model;

  public AbstractNode( final IModelMember model, final Feature node )
  {
    Assert.isNotNull( model );
    Assert.isNotNull( node );

    m_model = model;
    m_node = node;
  }

  // $ANALYSIS-IGNORE
  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals( final Object obj )
  {
    if( obj instanceof INode )
    {
      final INode node = (INode) obj;
      final EqualsBuilder equalsBuilder = new EqualsBuilder();
      equalsBuilder.append( getFeature(), node.getFeature() );

      return equalsBuilder.isEquals();
    }

    return super.equals( obj );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getDescription()
   */
  public String getDescription( )
  {
    return (String) m_node.getProperty( ISobekConstants.QN_HYDRAULIC_DESCRIPTION );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getFeature()
   */
  public Feature getFeature( )
  {
    return m_node;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getId()
   */
  public String getId( )
  {
    return (String) m_node.getProperty( ISobekConstants.QN_HYDRAULIC_UNIQUE_ID );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getGeometry()
   */
  public GM_Point getLocation( )
  {
    return (GM_Point) getFeature().getDefaultGeometryProperty();
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getJTSLocation()
   */
  public Point getJTSLocation( ) throws GM_Exception
  {
    return (Point) JTSAdapter.export( getLocation() );
  }

  public IModelMember getModel( )
  {
    return m_model;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getModelMember()
   */
  public IModelMember getModelMember( )
  {
    return m_model;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getName()
   */
  public String getName( )
  {
    return FeatureUtils.getFeatureName( ISobekConstants.NS_SOBEK, m_node );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#getStationName()
   */
  public String getStationName( )
  {
    return (String) m_node.getProperty( ISobekConstants.QN_HYDRAULIC_NODE_STATION_NAME );
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode( )
  {
    return HashCodeBuilder.reflectionHashCode( this );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.interfaces.INode#relaysOnGeometry(com.vividsolutions.jts.geom.Geometry)
   */
  public boolean relaysOnGeometry( final Geometry geometry ) throws GM_Exception
  {
    if( geometry instanceof LineString )
    {
      final LineString lineString = (LineString) geometry;
      final GM_Point location = getLocation();
      final Geometry point = JTSAdapter.export( location );

      if( lineString.intersects( point.buffer( GEOM_BUFFER ) ) )
        return true;

      return false;
    }

    throw new NotImplementedException();
  }
}
