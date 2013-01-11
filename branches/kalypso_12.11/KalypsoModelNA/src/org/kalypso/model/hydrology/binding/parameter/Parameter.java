/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import org.kalypso.afgui.model.UnversionedModel;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;

/**
 * Binding class for {http://www.tuhh.de/parameter}Parameter
 * 
 * @author Gernot Belger
 */
public class Parameter extends UnversionedModel
{
  private static final String NS_NAPARAMETER = NaModelConstants.NS_NAPARAMETER;

  public static final QName FEATURE_PARAMETER = new QName( NS_NAPARAMETER, "Parameter" ); //$NON-NLS-1$

  public static final QName MEMBER_SOILTYPE = new QName( NS_NAPARAMETER, "soiltypeMember" ); //$NON-NLS-1$

  public static final QName MEMBER_SOIL_LAYER = new QName( NS_NAPARAMETER, "soilLayerMember" ); //$NON-NLS-1$

  public static final QName MEMBER_DRWBM_SOILTYPE = new QName( NS_NAPARAMETER, "drwbmSoiltypeMember" ); //$NON-NLS-1$

  public static final QName MEMBER_DRWBM_DEFINITION = new QName( NS_NAPARAMETER, "drwbmDefinitionMember" ); //$NON-NLS-1$

  public static final QName MEMBER_SNOW = new QName( NS_NAPARAMETER, "snowMember" ); //$NON-NLS-1$

  private IFeatureBindingCollection<Soiltype> m_soiltypes;

  private IFeatureBindingCollection<DRWBMSoiltype> m_drwbmSoilTypes;

  private IFeatureBindingCollection<DRWBMDefinition> m_drwbmDefinitions;

  private IFeatureBindingCollection<Snow> m_snow;

  public Parameter( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  public synchronized IFeatureBindingCollection<Soiltype> getSoiltypes( )
  {
    if( m_soiltypes == null )
      m_soiltypes = new FeatureBindingCollection<>( this, Soiltype.class, MEMBER_SOILTYPE );

    return m_soiltypes;
  }

  public synchronized IFeatureBindingCollection<DRWBMSoiltype> getDRWBMSoiltypes( )
  {
    if( m_drwbmSoilTypes == null )
      m_drwbmSoilTypes = new FeatureBindingCollection<>( this, DRWBMSoiltype.class, MEMBER_DRWBM_SOILTYPE );

    return m_drwbmSoilTypes;
  }

  public synchronized IFeatureBindingCollection<DRWBMDefinition> getDRWBMDefinitions( )
  {
    if( m_drwbmDefinitions == null )
      m_drwbmDefinitions = new FeatureBindingCollection<>( this, DRWBMDefinition.class, MEMBER_DRWBM_DEFINITION );

    return m_drwbmDefinitions;
  }

  public synchronized IFeatureBindingCollection<Snow> getSnow( )
  {
    if( m_snow == null )
      m_snow = new FeatureBindingCollection<>( this, Snow.class, MEMBER_SNOW );

    return m_snow;
  }

  public Soiltype findSoiltypeByID( final String featureID )
  {
    final Feature feature = getWorkspace().getFeature( featureID );
    if( feature instanceof Soiltype )
      return (Soiltype)feature;

    return null;
  }

}
