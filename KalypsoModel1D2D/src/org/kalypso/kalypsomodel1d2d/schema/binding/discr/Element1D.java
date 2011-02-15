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

import org.kalypso.core.KalypsoCorePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Gernot Belger
 */
public class Element1D<CT extends IFE1D2DComplexElement, ET extends IFE1D2DEdge> extends FE1D2DElement<CT, ET> implements IElement1D<CT, ET>
{
  public Element1D( final Feature featureToBind )
  {
    this( featureToBind, IElement1D.QNAME, (Class<CT>) IFE1D2DComplexElement.class/* IRiverChannel1D.class */);
  }

  public Element1D( final Feature featureToBind, final QName featureQName, final Class<CT> complexElementClass )
  {
    super( featureToBind, featureQName, complexElementClass );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IElement1D#getEdge()
   */
  @Override
  public ET getEdge( )
  {
    final Feature feature = getFeature();
    final Object property = feature.getProperty( FE1D2DElement.WB1D2D_PROP_DIRECTEDEDGE/* QNAME_PROPS_DIRECTED_EDGE */);
    final Feature edgeFeature = FeatureHelper.getFeature( feature.getWorkspace(), property );
    if( edgeFeature == null )
      return null;

    return (ET) edgeFeature.getAdapter( IFE1D2DEdge.class );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IElement1D#setEdge(org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge)
   */
  @Override
  public void setEdge( final IFE1D2DEdge edge )
  {
    final IFE1D2DEdge oldEdge = getEdge();
    final String gmlID = getGmlID();
    if( oldEdge != null )
    {
      for( ; oldEdge.getContainers().remove( gmlID ); )
      {
        // removing all links
      }
    }

    final Feature feature = getFeature();
    if( edge == null )
    {
      feature.setProperty( FE1D2DElement.WB1D2D_PROP_DIRECTEDEDGE, null );
    }
    else
    {
      final String linkToEdge = edge.getGmlID();
      feature.setProperty( FE1D2DElement.WB1D2D_PROP_DIRECTEDEDGE, linkToEdge );

      final IFeatureWrapperCollection containers = edge.getContainers();
      final FeatureList wrappedList = containers.getWrappedList();
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
  @Override
  @SuppressWarnings("unchecked")//$NON-NLS-1$
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
  @Override
  public GM_Object recalculateElementGeometry( ) throws GM_Exception
  {
    final ET edge = getEdge();
    if( edge == null )
    {
      return null;
    }

    final List<IFE1D2DNode> nodes = edge.getNodes();

    final int SIZE = nodes.size();
    if( SIZE != 2 )
    {
      return null;
    }

    String crs = nodes.get( 0 ).getPoint().getCoordinateSystem();
    if( crs == null )
      crs = KalypsoCorePlugin.getDefault().getCoordinatesSystem();

    final GM_Position positions[] = new GM_Position[SIZE];
    GM_Point point;

    for( int i = 0; i < SIZE; i++ )
    {
      point = nodes.get( i ).getPoint();
      positions[i] = point.getPosition();
    }

    return GeometryFactory.createGM_Curve( positions, crs );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#getRoughnessClsID()
   */
  @Override
  public String getRoughnessClsID( )
  {
    final Object property = getFeature().getProperty( IFE1D2DElement.PROP_ROUGHNESS_CLS_ID );
    if( property == null )
      return ""; //$NON-NLS-1$
    return property.toString();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#getRoughnessCorrectionAxAy()
   */
  @Override
  public Double getRoughnessCorrectionAxAy( )
  {
    return (Double) getFeature().getProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_AXAY );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#getRoughnessCorrectionDP()
   */
  @Override
  public Double getRoughnessCorrectionDP( )
  {
    return (Double) getFeature().getProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_DP );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#getRoughnessCorrectionKS()
   */
  @Override
  public Double getRoughnessCorrectionKS( )
  {
    return (Double) getFeature().getProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_KS );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#getRoughnessStyle()
   */
  @Override
  public String getRoughnessStyle( )
  {
    return getFeature().getProperty( IFE1D2DElement.PROP_ROUGHNESS_STYLE ).toString();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#setRoughnessClsID(java.lang.String)
   */
  @Override
  public void setRoughnessClsID( final String value )
  {
    getFeature().setProperty( IFE1D2DElement.PROP_ROUGHNESS_CLS_ID, value );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#setRoughnessCorrectionAxAy(java.lang.String)
   */
  @Override
  public void setRoughnessCorrectionAxAy( final Double value )
  {
    getFeature().setProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_AXAY, value );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#setRoughnessCorrectionDP(java.lang.String)
   */
  @Override
  public void setRoughnessCorrectionDP( final Double value )
  {
    getFeature().setProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_DP, value );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#setRoughnessCorrectionKS(java.lang.String)
   */
  @Override
  public void setRoughnessCorrectionKS( final Double value )
  {
    getFeature().setProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_KS, value );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#setRoughnessStyle(java.lang.String)
   */
  @Override
  public void setRoughnessStyle( final String value )
  {
    getFeature().setProperty( IFE1D2DElement.PROP_ROUGHNESS_STYLE, value );
  }

}
