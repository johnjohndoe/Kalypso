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
package org.kalypso.model.hydrology.binding.parameter;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * @author Dirk Kuch
 */
public class DRWBMSoiltype extends Feature_Impl implements ISoilType
{
  private static final String NS_NAPARAMETER = NaModelConstants.NS_NAPARAMETER;

  public static final QName FEATURE_SOILTYPE = new QName( NS_NAPARAMETER, "DRWBMSoiltype" ); //$NON-NLS-1$

  public static final QName MEMBER_SOIL_LAYER_PARAMETER = new QName( NS_NAPARAMETER, "soilLayerParameterMember" ); //$NON-NLS-1$

  private IFeatureBindingCollection<DRWBMSoilLayerParameter> m_parameters = null;

  public DRWBMSoiltype( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public synchronized IFeatureBindingCollection<DRWBMSoilLayerParameter> getParameters( )
  {
    if( m_parameters == null )
      m_parameters = new FeatureBindingCollection<>( this, DRWBMSoilLayerParameter.class, MEMBER_SOIL_LAYER_PARAMETER );

    return m_parameters;
  }

  public boolean hasDrainageFunction( )
  {
    final IFeatureBindingCollection<DRWBMSoilLayerParameter> parameters = getParameters();
    for( final DRWBMSoilLayerParameter parameter : parameters )
    {
      final boolean isDrainage = parameter.isDrainageFunction();
      if( isDrainage )
        return true;
    }

    return false;
  }
}