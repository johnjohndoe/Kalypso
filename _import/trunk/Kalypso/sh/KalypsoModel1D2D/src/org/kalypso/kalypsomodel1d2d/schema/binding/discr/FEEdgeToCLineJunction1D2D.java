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
package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.geom.ModelGeometryBuilder;
import org.kalypso.kalypsomodel1d2d.ops.ModelOps;
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
public class FEEdgeToCLineJunction1D2D 
                  extends AbstractFeatureBinder
                  implements IFEEdgeToCLineJunction1D2D<
                                    IFE1D2DComplexElement, IFE1D2DEdge>
{
  
  private IFeatureWrapperCollection<IFE1D2DComplexElement> containers;

  /**
   * Create a new continuity line binding the provided feature.
   * @param featureToBind the feature to bind. null values are illegal
   * @throws IllegalArgumentException if the passed featureToBind 
   *    parameter is null
   */
  public FEEdgeToCLineJunction1D2D( 
                final Feature featureToBind )
                throws IllegalArgumentException
  {
    super( 
        featureToBind, 
        Kalypso1D2DSchemaConstants.WB1D2D_F_JUNCTION1D2D_EDGE_CLINE );
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
  public FEEdgeToCLineJunction1D2D( 
                        Feature parentFeature,
                        QName propQName,
                        String gmlID)
  {
    this(
      org.kalypso.kalypsosimulationmodel.core.Util.createFeatureWithId( 
          Kalypso1D2DSchemaConstants.WB1D2D_F_JUNCTION1D2D_EDGE_CLINE,
          parentFeature, 
          propQName, 
          gmlID ));
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DContinuityLine#recalculateGeometry()
   */
  public GM_Object recalculateElementGeometry( ) throws GM_Exception
  {
    final IFE1D2DEdge edge = getEdge();
    if(edge==null)
    {
      return null;
    }
    return ModelGeometryBuilder.computeEgdeGeometry( edge );
  }

  public IFE1D2DEdge getEdge( )
  {
    return getEdge(Kalypso1D2DSchemaConstants.WB1D2D_PROP_DIRECTEDEDGE);
  }


  private final IFE1D2DEdge getEdge(QName propToEdgeQName )
  {
    final Feature wrappedFeature = getWrappedFeature();
    Object property = wrappedFeature.getProperty( propToEdgeQName);
    Feature edge1Dfeature;
    if(property==null)
    {
      return null;
    }
    else if(property instanceof String)
    {
     edge1Dfeature=wrappedFeature.getWorkspace().getFeature( (String)property); 
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


  public void setEdge( IFE1D2DEdge newEdge )
  {
//    if(!TypeInfo.is1DEdge( newEdge ))
//    {
//      throw new IllegalArgumentException(
//          "1D edge Expected:"+newEdge);
//    }
    unrergisterAsContainer(getEdge());
    setEdge( 
        newEdge, 
        Kalypso1D2DSchemaConstants.WB1D2D_PROP_DIRECTEDEDGE );
  }

  private void unrergisterAsContainer( IFE1D2DEdge oldEdge )
  {
    if(oldEdge==null)
    {
      return;
    }  
    oldEdge.getContainers().getWrappedList().remove( getGmlID() );
  }

  private final void setEdge(IFE1D2DEdge edge,QName propToEdgeQName )
  {
    final Feature wrappedFeature = getWrappedFeature();
    wrappedFeature.setProperty(propToEdgeQName, edge.getGmlID());
    edge.getContainers().getWrappedList().add( getGmlID() );
    wrappedFeature.invalidEnvelope();
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




  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement#addEdge(java.lang.String)
   */
  public void addEdge( String edgeID )
  {
    throw new UnsupportedOperationException();    
  }




  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement#getNodes()
   */
  public List<IFE1D2DNode> getNodes( )
  {
    throw new UnsupportedOperationException();
  }




}
