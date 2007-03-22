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

import org.kalypso.kalypsomodel1d2d.geom.ModelGeometryBuilder;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Gernot Belger
 */
public class Element1D extends FE1D2DElement implements IElement1D<IFE1D2DComplexElement, IFE1D2DEdge>
{
  public Element1D( final Feature featureToBind )
  {
    super( featureToBind, QNAME );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IElement1D#getEdge()
   */
  public IFE1D2DEdge getEdge( )
  {
    final Feature feature = getFeature();
    final Object property = feature.getProperty( QNAME_PROPS_DIRECTED_EDGE );
    final Feature edgeFeature = FeatureHelper.getFeature( feature.getWorkspace(), property );
    if( edgeFeature == null )
      return null;

    return (IFE1D2DEdge) edgeFeature.getAdapter( IFE1D2DEdge.class );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IElement1D#setEdge(org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge)
   */
  public void setEdge( final IFE1D2DEdge edge )
  {
    IFE1D2DEdge oldEdge = getEdge();
    final String gmlID = getGmlID();
    if(oldEdge!=null)
    {
      for(;oldEdge.getContainers().remove( gmlID );)
      {
        //removing all links
      }
    }
    
    final Feature feature = getWrappedFeature();
    if( edge == null )
    {
      feature.setProperty( QNAME_PROPS_DIRECTED_EDGE, null );
    }
    else
    {
      final String linkToEdge  = edge.getGmlID();
      feature.setProperty( QNAME_PROPS_DIRECTED_EDGE, linkToEdge );
      
      final IFeatureWrapperCollection containers = edge.getContainers();
      FeatureList wrappedList = containers.getWrappedList();
      // TODO: only add if not already present. 
      // May the containers contain me twice?
      
      // TODO: this is a potential performance problem, because this is a linear list search
        if( !wrappedList.contains( gmlID ) )
        {
          wrappedList.add( gmlID );
        }
     }
    // Setting the edge causes the envelope to become invalid
    feature.invalidEnvelope();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement#getNodes()
   */
  @SuppressWarnings("unchecked")
  public List<IFE1D2DNode> getNodes( )
  {
    final IFE1D2DEdge edge = getEdge();
    if( edge == null )
      return null;

    return edge.getNodes();
  }

  /**
   * Recalculates the geometry of this element. Used by the corresponding property function.
   */
  public GM_Object recalculateElementGeometry( ) throws GM_Exception
  {
      return ModelGeometryBuilder.computeEgdeGeometry( getEdge() );
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IElement1D#getGeometry()
   */
  public GM_Curve getGeometry( )
  {
    return (GM_Curve) getWrappedFeature().getProperty( QNAME_PROPS_GEOMETRY );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IElement1D#setGeometry(org.kalypsodeegree.model.geometry.GM_Curve)
   */
  public void setGeometry( final GM_Curve curve )
  {
    getWrappedFeature().setProperty( QNAME_PROPS_GEOMETRY, curve );
  }
}
