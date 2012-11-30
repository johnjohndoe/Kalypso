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
package org.kalypso.kalypsomodel1d2d.schema.binding.flowrel;

import java.math.BigDecimal;
import java.util.List;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Gernot Belger
 */
public class TeschkeFlowRelation extends AbstractFlowRelation1D implements ITeschkeFlowRelation
{
  private final IFeatureBindingCollection<IPolynomial1D> m_polynomes = new FeatureBindingCollection<>( this, IPolynomial1D.class, QNAME_PROP_POLYNOMES );

  public TeschkeFlowRelation( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public BigDecimal getStation( )
  {
    return (BigDecimal) getProperty( QNAME_PROP_STATION );
  }

  @Override
  public void setStation( final BigDecimal station )
  {
    setProperty( QNAME_PROP_STATION, station );
  }

  @Override
  public List<IPolynomial1D> getPolynomials( )
  {
    return m_polynomes;
  }

  @Override
  public double getSlope( )
  {
    final BigDecimal slope = (BigDecimal) getProperty( QNAME_PROP_SLOPE );
    if( slope == null )
      return Double.NaN;

    return slope.doubleValue();
  }

  @Override
  public void setSlope( final double slope )
  {
    final BigDecimal slopeDec = new BigDecimal( slope ).setScale( 5, BigDecimal.ROUND_HALF_UP );
    setProperty( QNAME_PROP_SLOPE, slopeDec );
  }

  @Override
  public IProfileFeature getProfile( )
  {
    return (IProfileFeature) FeatureHelper.resolveLink( this, QNAME_PROP_PROFILE, true );
  }

  @Override
  public void setProfileLink( final String profileRef )
  {
    setLink( QNAME_PROP_PROFILE, profileRef );
  }
}