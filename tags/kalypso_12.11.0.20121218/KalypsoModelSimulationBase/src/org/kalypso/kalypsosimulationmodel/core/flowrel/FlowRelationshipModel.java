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
package org.kalypso.kalypsosimulationmodel.core.flowrel;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.afgui.model.UnversionedModel;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Gernot Belger
 */
public class FlowRelationshipModel extends UnversionedModel implements IFlowRelationshipModel
{
  private final IFeatureBindingCollection<IFlowRelationship> m_flowRelationsShips = new FeatureBindingCollection<>( this, IFlowRelationship.class, QNAME_PROP_FLOW_REL_MEMBER );

  public FlowRelationshipModel( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public IFeatureBindingCollection<IFlowRelationship> getFlowRelationsShips( )
  {
    return m_flowRelationsShips;
  }

  @Override
  public IFlowRelationship findFlowrelationship( final GM_Position position, final double searchRectWidth )
  {
    final List<Feature> foundFeatures = findFeatures( position, searchRectWidth );
    if( foundFeatures.isEmpty() )
      return null;

    double min = Double.MAX_VALUE;
    IFlowRelationship nearest = null;
    for( final Feature feature : foundFeatures )
    {
      final IFlowRelationship curNode = (IFlowRelationship) feature.getAdapter( IFlowRelationship.class );

      final double curDist = position.getDistance( curNode.getPosition().getPosition() );
      if( curDist < searchRectWidth && min > curDist )
      {
        nearest = curNode;
        min = curDist;
      }
    }
    return nearest;
  }

  @Override
  public IFlowRelationship[] findFlowrelationships( final GM_Position position, final double searchRectWidth )
  {
    final List<Feature> foundFeatures = findFeatures( position, searchRectWidth );

    final List<IFlowRelationship> result = new ArrayList<>();
    for( int i = 0; i < foundFeatures.size(); i++ )
    {
      final Feature feature = foundFeatures.get( i );
      final GM_Object geom = feature.getDefaultGeometryPropertyValue();
      if( geom != null )
      {
        final GM_Point point = GeometryFactory.createGM_Point( position, geom.getCoordinateSystem() );
        if( geom.distance( point ) < searchRectWidth )
        {
          final IFlowRelationship flowRel = (IFlowRelationship) feature.getAdapter( IFlowRelationship.class );
          if( flowRel != null )
            result.add( flowRel );
        }
      }

    }

    return result.toArray( new IFlowRelationship[result.size()] );
  }

  @Override
  public IFlowRelationship findFlowrelationship( final GM_Position position, final double searchDistance, final Class< ? extends IFlowRelationshipModel>[] flowRelationTypes )
  {
    final List<Feature> foundFeatures = findFeatures( position, searchDistance );
    if( foundFeatures.isEmpty() )
      return null;

    double min = Double.MAX_VALUE;
    IFlowRelationship nearest = null;
    for( final Feature feature : foundFeatures )
    {
      final IFlowRelationship curNode = (IFlowRelationship) feature.getAdapter( IFlowRelationship.class );
      if( curNode == null )
        continue;

      final Class< ? extends IFlowRelationship> clas = curNode.getClass();
      if( !checkRealtionType( flowRelationTypes, clas ) )
        continue;

      final double curDist = position.getDistance( curNode.getPosition().getPosition() );
      if( curDist < searchDistance && min > curDist )
      {
        nearest = curNode;
        min = curDist;
      }
    }

    return nearest;
  }

  private boolean checkRealtionType( final Class< ? extends IFlowRelationshipModel>[] flowRelationTypes, final Class< ? > clas )
  {
    for( final Class< ? extends IFlowRelationshipModel> element : flowRelationTypes )
    {
      if( element.isAssignableFrom( clas ) )
        return true;
    }
    return false;
  }

  // ATTENTION: this method returns possibly MUCH more features than expected
  private List<Feature> findFeatures( final GM_Position position, final double searchRectWidth )
  {
    final FeatureList nodeList = m_flowRelationsShips.getFeatureList();
    final double posX = position.getX();
    final double posY = position.getY();
    final double searchWidthHalf = searchRectWidth / 2;

    final GM_Position minPos = GeometryFactory.createGM_Position( posX - searchWidthHalf, posY - searchWidthHalf );
    final GM_Position maxPos = GeometryFactory.createGM_Position( posX + searchWidthHalf, posY + searchWidthHalf );
    final GM_Envelope reqEnvelope = GeometryFactory.createGM_Envelope( minPos, maxPos, null );

    return nodeList.query( reqEnvelope, null );
  }
}