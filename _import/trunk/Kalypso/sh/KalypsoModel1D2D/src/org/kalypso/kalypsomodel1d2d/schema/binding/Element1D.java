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

import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsosimulationmodel.core.FeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

/**
 * @author Gernot Belger
 */
public class Element1D extends AbstractFeatureBinder implements IElement1D<IFE1D2DComplexElement, IFE1D2DEdge>
{
  private static final QName QNAME_PROPS_DIRECTED_EDGE = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "directedEdge" );

  private FeatureWrapperCollection<IFE1D2DComplexElement> m_containers;

  public Element1D( Feature featureToBind )
  {
    super( featureToBind, QNAME );

    m_containers = new FeatureWrapperCollection<IFE1D2DComplexElement>( featureToBind, IFE1D2DComplexElement.class, Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENT_CONTAINERS );
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
    final String linkToEdge;
    if( edge == null )
      linkToEdge = null;
    else
      linkToEdge = edge.getGmlID();

    final Feature feature = getFeature();
    feature.setProperty( QNAME_PROPS_DIRECTED_EDGE, linkToEdge );

    final IFeatureWrapperCollection containers = edge.getContainers();
    if( linkToEdge == null )
      containers.remove( this );
    else
    {
      // TODO: only add if not already present. 
      // May the containers contain me twice?
      if( !containers.contains( this ) )
        containers.add( this );
    }
  }

  /**
   * Not supported. Use {@link #setEdge(IFE1D2DEdge)} instead.
   * <p>
   * TODO: this method is only here, because it is needed by IFE1D2DElement and above. The corresponding gml-type does
   * not contain this property.
   * 
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement#addEdge(java.lang.String)
   */
  public void addEdge( final String edgeID )
  {
    throw new UnsupportedOperationException( "Add edges not supported. Use setEdge instead." );
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
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEElement#getContainers()
   */
  public IFeatureWrapperCollection<IFE1D2DComplexElement> getContainers( )
  {
    return m_containers;
  }

  /**
   * Do not use. Use {@link #getEdge()} instead.
   * <p>
   * TODO: this method is only here, because it is needed by IFE1D2DElement and above. The corresponding gml-type does
   * not contina this property.
   * 
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEElement#getEdges()
   */
  public IFeatureWrapperCollection<IFE1D2DEdge> getEdges( )
  {
    throw new UnsupportedOperationException();
  }
}
