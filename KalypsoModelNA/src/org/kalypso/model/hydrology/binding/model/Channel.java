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
package org.kalypso.model.hydrology.binding.model;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Binding class for {http://www.tuhh.de/kalypsoNA}_Channel.
 * 
 * @author Gernot Belger
 */
public abstract class Channel extends AbstractNaModelElement
{
  public static final QName FEATURE_CHANNEL = new QName( NaModelConstants.NS_NAMODELL, "_Channel" ); //$NON-NLS-1$

  private static final QName LINK_DOWNSTREAMNODE = new QName( NS_NAMODELL, "downStreamNodeMember" ); //$NON-NLS-1$

  public static final QName PROP_ORT = new QName( NS_NAMODELL, "Ort" ); //$NON-NLS-1$

  public Channel( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  public void setDownstreamNode( final Node downstreamNode )
  {
    setProperty( LINK_DOWNSTREAMNODE, downstreamNode.getId() );
  }

  public Node getDownstreamNode( )
  {
    return (Node) FeatureHelper.resolveLink( this, LINK_DOWNSTREAMNODE, true );
  }

  /**
   * Returns all catchments of the mode that link to this channel.<br/>
   * Handle with care, as this methods involves a linear search through all catchments of the model.
   */
  public Catchment[] findCatchments( )
  {
    final List<Catchment> catchmentList = new ArrayList<Catchment>();

    final NaModell naModel = getNaModel();
    final IFeatureBindingCollection<Catchment> catchments = naModel.getCatchments();
    for( final Catchment catchment : catchments )
    {
      final Channel channel = catchment.getChannel();
      if( this == channel )
        catchmentList.add( catchment );
    }

    return catchmentList.toArray( new Catchment[catchmentList.size()] );
  }

  /**
   * Returns the node connected to this channel.<br/>
   * Use with care, as this methods involves a linear search through all nodes.
   */
  public Node findUpstreamNode( )
  {
    final NaModell naModel = getNaModel();
    final IFeatureBindingCollection<Node> nodes = naModel.getNodes();

    for( final Node node : nodes )
    {
      final Channel downStreamChannel = node.getDownstreamChannel();
      if( this == downStreamChannel )
        return node;
    }
    return null;
  }

}
