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
package org.kalypso.kalypsomodel1d2d.schema.binding;

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsosimulationmodel.core.FeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

/**
 * @author Gernot Belger
 */
public class FE1D2DDiscretisationModel extends AbstractFeatureBinder
{
//  public final static QName QNAME_FE1D2DDiscretisationModel = 
//        new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "DiscretisationModel" );

//  public final static QName QNAME_PROP_EDGES = 
//          new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "edge" );

//  public final static QName QNAME_PROP_ELEMENTS = 
//          new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "element" );

//  public final static QName QNAME_PROP_NODES = 
//          new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "node" );

  private IFeatureWrapperCollection<IFE1D2DElement> m_elements = 
            new FeatureWrapperCollection<IFE1D2DElement>( 
                    getFeature(), 
                    IFE1D2DElement.class, 
                    Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENTS );

  private IFeatureWrapperCollection<IFE1D2DEdge> m_edges = 
            new FeatureWrapperCollection<IFE1D2DEdge>( 
                  getFeature(), 
                  IFE1D2DEdge.class, 
                  Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGES );

  private IFeatureWrapperCollection<IFE1D2DNode> m_nodes = 
            new FeatureWrapperCollection<IFE1D2DNode>( 
                      getFeature(), 
                      IFE1D2DNode.class, 
                      Kalypso1D2DSchemaConstants.WB1D2D_PROP_NODES
                      /*QNAME_PROP_NODES*/ );

  public FE1D2DDiscretisationModel( final Feature featureToBind )
  {
    super( 
        featureToBind, 
        Kalypso1D2DSchemaConstants.WB1D2D_F_DiscretisationModel);
  }

  public FE1D2DEdge findEdge( final FE1D2DNode node0, final FE1D2DNode node1 )
  {
    final List edgeList = 
        (List) getFeature().getProperty( 
            Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGES );
    // TODO: brute force search, check if this scales good with big models
    for( final Object object : edgeList )
    {
      final FE1D2DEdge edge = new FE1D2DEdge( (Feature) object );
      final FE1D2DNode[] nodes = edge.getNodesAsArray();

      if( nodes.length != 2 )
        return null;

      if( (node0.equals( nodes[0] ) && node1.equals( nodes[1] )) || (node0.equals( nodes[1] ) && node1.equals( nodes[0] )) )
        return edge;
    }

    return null;
  }

  public final IFeatureWrapperCollection<IFE1D2DElement> getElements( )
  {
    return m_elements;
  }

  public IFeatureWrapperCollection<IFE1D2DNode> getNodes( )
  {
    return m_nodes;
  }

  public IFeatureWrapperCollection<IFE1D2DEdge> getEdges( )
  {
    return m_edges;
  }

  public IFE1D2DContinuityLine<IFE1D2DComplexElement, IFE1D2DEdge> createContinuityLine( )
  {
    final Feature parentFeature = getFeature();
    final IFeatureType parentFT = parentFeature.getFeatureType();
    final IRelationType rt = 
        (IRelationType) parentFT.getProperty( 
              Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENTS );

    final IFeatureType contiType = 
          parentFT.getGMLSchema().getFeatureType( 
              Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DContinuityLine );
    final Feature contiFeature = 
              parentFeature.getWorkspace().createFeature( 
                                    parentFeature, rt, contiType );
    return new FE1D2DContinuityLine( contiFeature );
  }

}
