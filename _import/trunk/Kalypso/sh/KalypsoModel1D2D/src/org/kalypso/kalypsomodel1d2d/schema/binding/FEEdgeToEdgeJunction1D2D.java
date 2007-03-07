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

import org.kalypso.kalypsomodel1d2d.geom.ModelGeometryBuilder;
import org.kalypso.kalypsomodel1d2d.ops.ModelOps;
import org.kalypso.kalypsomodel1d2d.ops.TypeInfo;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

@SuppressWarnings("unchecked")
/**
 * Default implementation for {@link IFEJunction1D2D} for
 * binding a feature of the type wb1d2d:Junction1D2D
 * 
 * @author Patrice Congo
 */
public class FEEdgeToEdgeJunction1D2D 
                  extends AbstractFeatureBinder
                  implements IFEEdgeToEdgeJunction1D2D<
                                    IFE1D2DComplexElement, IFE1D2DEdge>
{
  private IFeatureWrapperCollection<IFE1D2DComplexElement> containers;




  /**
   * Create a new continuity line binding the provided feature.
   * @param featureToBind the feature to bind. null values are illegal
   * @throws IllegalArgumentException if the passed featureToBind 
   *    parameter is null
   */
  public FEEdgeToEdgeJunction1D2D( 
                final Feature featureToBind )
                throws IllegalArgumentException
  {
    super( 
        featureToBind, 
        Kalypso1D2DSchemaConstants.WB1D2D_F_JUNCTION1D2D_EDGE_EDGE );
    containers = ModelOps.getElementContainer( featureToBind );
  }



  
  /**
   * Creates a Junction with a specified GML ID.
   * The parent feature respectively its link to the newly 
   * created continuity line are specified as parameters.
   * @param parentFeature the parent feature
   * @param propQName the qname of the property linking the
   *    parent feature to the continuity line 
   */
  public FEEdgeToEdgeJunction1D2D( 
                        Feature parentFeature,
                        QName propQName,
                        String gmlID)
  {
    this(
      org.kalypso.kalypsosimulationmodel.core.Util.createFeatureWithId( 
          Kalypso1D2DSchemaConstants.WB1D2D_F_JUNCTION1D2D_EDGE_EDGE,
          parentFeature, 
          propQName, 
          gmlID ));
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DContinuityLine#recalculateGeometry()
   */
  public GM_Object recalculateElementGeometry( ) throws GM_Exception
  {
    return ModelGeometryBuilder.computeEdgeToEdgeJunction1D2DGeometry( this ); 
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2D_2DElement#getNodes()
   */
  
  public List<IFE1D2DNode> getNodes( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFEEdgeToEdgeJunction1D2D#get1DEdge()
   */
  public IFE1D2DEdge get1DEdge( )
  {
    return getEdge(Kalypso1D2DSchemaConstants.WB1D2D_PROP_JUNCTION_1DEDGE);
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFEEdgeToEdgeJunction1D2D#get2DEdge()
   */
  public IFE1D2DEdge get2DEdge( )
  {
    return getEdge( Kalypso1D2DSchemaConstants.WB1D2D_PROP_JUNCTION_2DEDGE );
  }
  
  private final IFE1D2DEdge getEdge(QName propToEdgeQName )
  {
    Object property = m_featureToBind.getProperty( propToEdgeQName);
    Feature edge1Dfeature;
    if(property==null)
    {
      return null;
    }
    else if(property instanceof String)
    {
     edge1Dfeature=m_featureToBind.getWorkspace().getFeature( (String)property); 
    }else if(property instanceof Feature)
    {
      edge1Dfeature = (Feature)property;
    }
    else
    {
      throw new IllegalStateException(
                "Ref or feature expected"+
                "\n\ttype="+((property==null)?null:property.getClass())+
                "\n\tvalue="+property);
    }
    return (IFE1D2DEdge) edge1Dfeature.getAdapter( IFE1D2DEdge.class );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFEEdgeToEdgeJunction1D2D#set1DEdge(org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge)
   */
  public void set1DEdge( IFE1D2DEdge newEdge )
  {
    if(!TypeInfo.is1DEdge( newEdge ))
    {
      throw new IllegalArgumentException(
          "1D edge Expected:"+newEdge);
    }
    unrergisterAsContainer(get1DEdge());
    setEdge( 
        newEdge, 
        Kalypso1D2DSchemaConstants.WB1D2D_PROP_JUNCTION_1DEDGE );
  }

  private void unrergisterAsContainer( IFE1D2DEdge newEdge )
  {
    if(newEdge==null)
    {
      return;
    }    
  }

  private final void setEdge(IFE1D2DEdge edge,QName propToEdgeQName )
  {
    m_featureToBind.setProperty(propToEdgeQName, edge.getGmlID());
    edge.getContainers().getWrappedList().add( getGmlID() );
//    m_featureToBind.invalidEnvelope();
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFEEdgeToEdgeJunction1D2D#set2DEdge(org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge)
   */
  public void set2DEdge( IFE1D2DEdge new2DEdge )
  {
    if(!TypeInfo.is2DEdge( new2DEdge ))
    {
      throw new IllegalArgumentException(
          "2D edge expedted:"+new2DEdge);
    }
    unrergisterAsContainer( get2DEdge() );
    setEdge( 
        new2DEdge, 
        Kalypso1D2DSchemaConstants.WB1D2D_PROP_JUNCTION_2DEDGE );
  }




  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement#addEdge(java.lang.String)
   */
  public void addEdge( String edgeID )
  {
    throw new UnsupportedOperationException();
    
  }




  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEElement#getContainers()
   */
  public IFeatureWrapperCollection<IFE1D2DComplexElement> getContainers( )
  {
    return containers;
  }




  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEElement#getEdges()
   */
  public IFeatureWrapperCollection<IFE1D2DEdge> getEdges( )
  {
    throw new UnsupportedOperationException();
  }
  
  
  
}
