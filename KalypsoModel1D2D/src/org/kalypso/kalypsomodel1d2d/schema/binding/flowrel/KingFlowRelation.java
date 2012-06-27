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

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.core.gml.IProfileFeature;

/**
 * @author Gernot Belger
 */
public class KingFlowRelation extends AbstractFlowRelation1D implements IKingFlowRelation
{

  public KingFlowRelation( Object parent, IRelationType parentRelation, IFeatureType ft, String id, Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IKingFlowRelation#getWidth()
   */
  @Override
  public BigDecimal getWidth( )
  {
    return (BigDecimal) getProperty( QNAME_PROP_WIDTH );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IKingFlowRelation#getBankSlopeLeft()
   */
  @Override
  public BigDecimal getBankSlopeLeft( )
  {
    final BigDecimal slopeLeft = (BigDecimal) (getProperty( QNAME_PROP_SS1 ));
    if( slopeLeft == null )
      return new BigDecimal( 0.0 );

    return slopeLeft;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IKingFlowRelation#getBankSlopeRight()
   */
  @Override
  public BigDecimal getBankSlopeRight( )
  {
    final BigDecimal slopeRight = (BigDecimal) (getProperty( QNAME_PROP_SS2 ));
    if( slopeRight == null )
    {
      return new BigDecimal( 0.0 );
    }
    return slopeRight;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IKingFlowRelation#getHeightStorage()
   */
  @Override
  public BigDecimal getHeightStorage( )
  {
    return (BigDecimal) getProperty( QNAME_PROP_WSS );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IKingFlowRelation#getSlopeStorage()
   */
  @Override
  public BigDecimal getSlopeStorage( )
  {
    final BigDecimal slopeStorage = (BigDecimal) (getProperty( QNAME_PROP_WIDBS ));
    if( slopeStorage == null )
    {
      return new BigDecimal( 0.0 );
    }

    return slopeStorage;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IKingFlowRelation#getWidthStorage()
   */
  @Override
  public BigDecimal getWidthStorage( )
  {
    return (BigDecimal) getProperty( QNAME_PROP_WIDS );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation1D#getProfile()
   */
  @Override
  public IProfileFeature getProfile( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation1D#getStation()
   */
  @Override
  public BigDecimal getStation( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation1D#setProfileLink(java.lang.String)
   */
  @Override
  public void setProfileLink( final String profileRef )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation1D#setStation(java.math.BigDecimal)
   */
  @Override
  public void setStation( final BigDecimal station )
  {
    throw new UnsupportedOperationException();
  }
}
