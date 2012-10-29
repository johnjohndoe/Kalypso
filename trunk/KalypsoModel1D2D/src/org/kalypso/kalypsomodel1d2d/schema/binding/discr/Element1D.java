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

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Gernot Belger
 */
public class Element1D extends FE1D2DElement implements IElement1D
{
  public Element1D( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public IFE1D2DEdge getEdge( )
  {
    final Object property = getProperty( FE1D2DElement.WB1D2D_PROP_DIRECTEDEDGE );
    final IFE1D2DEdge edgeFeature = (IFE1D2DEdge)FeatureHelper.getFeature( this.getWorkspace(), property );
    return edgeFeature;
  }

  @Override
  public void setEdge( final IFE1D2DEdge edge )
  {
    Assert.throwIAEOnNullParam( edge, "edge" );
    final IFE1D2DEdge oldEdge = getEdge();
    if( oldEdge != null )
      oldEdge.removeLinkedElement( this );
    setProperty( FE1D2DElement.WB1D2D_PROP_DIRECTEDEDGE, edge.getId() );
    edge.addLinkedElement( this );
    setEnvelopesUpdated();
  }

  @Override
  public IFE1D2DNode[] getNodes( )
  {
    final IFE1D2DEdge edge = getEdge();
    return edge.getNodes();
  }

  @Override
  public String getRoughnessClsID( )
  {
    final Object property = getProperty( IFE1D2DElement.PROP_ROUGHNESS_CLS_ID );
    if( property == null )
      return ""; //$NON-NLS-1$
    return property.toString();
  }

  @Override
  public Double getRoughnessCorrectionAxAy( )
  {
    return (Double)getProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_AXAY );
  }

  @Override
  public Double getRoughnessCorrectionDP( )
  {
    return (Double)getProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_DP );
  }

  @Override
  public Double getRoughnessCorrectionKS( )
  {
    return (Double)getProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_KS );
  }

  @Override
  public String getRoughnessStyle( )
  {
    return getProperty( IFE1D2DElement.PROP_ROUGHNESS_STYLE ).toString();
  }

  @Override
  public void setRoughnessClsID( final String value )
  {
    setProperty( IFE1D2DElement.PROP_ROUGHNESS_CLS_ID, value );
  }

  @Override
  public void setRoughnessCorrectionAxAy( final Double value )
  {
    setProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_AXAY, value );
  }

  @Override
  public void setRoughnessCorrectionDP( final Double value )
  {
    setProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_DP, value );
  }

  @Override
  public void setRoughnessCorrectionKS( final Double value )
  {
    setProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_KS, value );
  }

  @Override
  public void setRoughnessStyle( final String value )
  {
    setProperty( IFE1D2DElement.PROP_ROUGHNESS_STYLE, value );
  }
}
