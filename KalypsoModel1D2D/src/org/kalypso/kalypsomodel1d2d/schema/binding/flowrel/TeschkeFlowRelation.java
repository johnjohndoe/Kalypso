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
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * @author Gernot Belger
 */
public class TeschkeFlowRelation extends AbstractFlowRelation1D implements ITeschkeFlowRelation
{
  private final IFeatureWrapperCollection<IPolynomial1D> m_polynomes = new FeatureWrapperCollection<IPolynomial1D>( getFeature(), IPolynomial1D.class, QNAME_PROP_POLYNOMES );

  public TeschkeFlowRelation( final Feature featureToBind )
  {
    super( featureToBind, ITeschkeFlowRelation.QNAME );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation#getStation()
   */
  @Override
  public BigDecimal getStation( )
  {
    return (BigDecimal) getFeature().getProperty( QNAME_PROP_STATION );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation#setStation(java.math.BigDecimal)
   */
  @Override
  public void setStation( final BigDecimal station )
  {
    getFeature().setProperty( QNAME_PROP_STATION, station );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation#getPolynomials()
   */
  @Override
  public List<IPolynomial1D> getPolynomials( )
  {
    return m_polynomes;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation#getSlope()
   */
  @Override
  public double getSlope( )
  {
    final BigDecimal slope = (BigDecimal) getFeature().getProperty( QNAME_PROP_SLOPE );
    return slope.doubleValue();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation#setSlope(double)
   */
  @Override
  public void setSlope( final double slope )
  {
    final BigDecimal slopeDec = new BigDecimal( slope ).setScale( 5, BigDecimal.ROUND_HALF_UP );
    getFeature().setProperty( QNAME_PROP_SLOPE, slopeDec );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation#getProfile()
   */
  @Override
  public IProfileFeature getProfile( )
  {
    final IProfileFeature profileFeature = (IProfileFeature) FeatureHelper.resolveLink( getFeature(), QNAME_PROP_PROFILE, true );

    return profileFeature;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation#setProfileLink(java.lang.String)
   */
  @Override
  public void setProfileLink( final String profileRef )
  {
    final Feature feature = getFeature();

    final IRelationType profileRelation = (IRelationType) feature.getFeatureType().getProperty( QNAME_PROP_PROFILE );
    final IFeatureType profileFT = profileRelation.getTargetFeatureType();
    final Feature profileLinkFeature = new XLinkedFeature_Impl( feature, profileRelation, profileFT, profileRef, "", "", "", "", "" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    feature.setProperty( profileRelation, profileLinkFeature );
  }

}
