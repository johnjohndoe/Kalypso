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
package org.kalypso.model.wspm.pdb.internal.gaf;

import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.GetPdbList;
import org.kalypso.model.wspm.pdb.db.mapping.Roughness;
import org.kalypso.model.wspm.pdb.db.mapping.StyleArray;
import org.kalypso.model.wspm.pdb.db.mapping.Vegetation;
import org.kalypso.model.wspm.pdb.db.utils.CrossSectionPartTypes;
import org.kalypso.model.wspm.pdb.gaf.ICoefficients;

/**
 * Helper class that allows to access {@link org.kalypso.model.wspm.pdb.db.mapping.Roughness} and
 * {@link org.kalypso.model.wspm.pdb.db.mapping.Vegetation}.
 *
 * @author Gernot Belger
 */
public class Coefficients implements ICoefficients
{
  private final RoughnessInfo m_roughnessInfo;

  private final VegetationInfo m_vegetationInfo;

  private final CrossSectionPartTypes m_partTypes;

  private StyleArray[] m_styleArrays;

  public Coefficients( final Session session, final String kind ) throws PdbConnectException
  {
    try
    {
      m_roughnessInfo = new RoughnessInfo( session, kind );
      m_vegetationInfo = new VegetationInfo( session, kind );
      m_partTypes = new CrossSectionPartTypes( session );
      m_styleArrays = GetPdbList.getArray( session, StyleArray.class );
    }
    catch( final HibernateException e )
    {
      throw new PdbConnectException( "Failed to load classifications from database", e ); //$NON-NLS-1$
    }
  }

  @Override
  public Roughness getRoughness( final String roughnessClass )
  {
    return m_roughnessInfo.getCoefficient( roughnessClass );
  }

  @Override
  public Vegetation getVegetation( final String vegetationClass )
  {
    return m_vegetationInfo.getCoefficient( vegetationClass );
  }

  @Override
  public Roughness getRoughnessOrUnknown( final String roughnessClass )
  {
    final Roughness roughness = getRoughness( roughnessClass );
    if( roughness == null )
      return getRoughness( UNKNOWN_ROUGHNESS );
    return roughness;
  }

  @Override
  public Vegetation getVegetationOrUnknown( final String vegetationClass )
  {
    final Vegetation vegetation = getVegetation( vegetationClass );
    if( vegetation == null )
      return getVegetation( UNKNOWN_VEGETATION );
    return vegetation;
  }

  @Override
  public Roughness[] getAllRoughness( )
  {
    return m_roughnessInfo.getAllCoefficients();
  }

  @Override
  public Vegetation[] getAllVegetation( )
  {
    return m_vegetationInfo.getAllCoefficients();
  }

  @Override
  public Vegetation getUnknownVegetation( )
  {
    return getVegetation( UNKNOWN_VEGETATION );
  }

  @Override
  public Roughness getUnknownRoughness( )
  {
    return getRoughness( UNKNOWN_ROUGHNESS );
  }

  @Override
  public CrossSectionPartTypes getPartTypes( )
  {
    return m_partTypes;
  }

  @Override
  public StyleArray[] getAllStyleArrays( )
  {
    return m_styleArrays;
  }
}