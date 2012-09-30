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
package org.kalypso.model.hydrology.binding.model;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.model.channels.Channel;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;

/**
 * Binding class for {http://www.tuhh.de/kalypsoNA}KMChannel.
 *
 * @author Gernot Belger
 */
public class KMChannel extends Channel
{
  public static final QName FEATURE_KM_CHANNEL = new QName( NaModelConstants.NS_NAMODELL, "KMChannel" ); //$NON-NLS-1$

  private static final QName PROP_FAKTOR_RKF = new QName( NS_NAMODELL, "faktorRkf" ); //$NON-NLS-1$

  private static final QName PROP_FAKTOR_RNF = new QName( NS_NAMODELL, "faktorRnf" ); //$NON-NLS-1$

  public static final QName MEMBER_PARAMETER = new QName( NS_NAMODELL, "KMParameterMember" ); //$NON-NLS-1$

  public static final QName PROP_KMSTART = new QName( NS_NAMODELL, "startkm" ); //$NON-NLS-1$

  public static final QName PROP_KMEND = new QName( NS_NAMODELL, "endkm" ); //$NON-NLS-1$

  private IFeatureBindingCollection<KMParameter> m_parameters = null;

  public KMChannel( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  public synchronized IFeatureBindingCollection<KMParameter> getParameters( )
  {
    if( m_parameters == null )
      m_parameters = new FeatureBindingCollection<>( this, KMParameter.class, MEMBER_PARAMETER );

    return m_parameters;
  }

  public double getFaktorRkf( )
  {
    return getDoubleProperty( PROP_FAKTOR_RKF, 1.0 );
  }

  public double getFaktorRnf( )
  {
    return getDoubleProperty( PROP_FAKTOR_RNF, 1.0 );
  }

  public Double getKMStart( )
  {
    return getProperty( PROP_KMSTART, Double.class );
  }

  public Double getKMEnd( )
  {
    return getProperty( PROP_KMEND, Double.class );
  }

  public void setFaktorRkf( final Double factorRkf )
  {
    setProperty( PROP_FAKTOR_RKF, factorRkf );
  }

  public void setFaktorRnf( final Double factorRnf )
  {
    setProperty( PROP_FAKTOR_RNF, factorRnf );
  }
}
