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
package org.kalypso.model.hydrology.binding.initialValues;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * Binding class for {http://www.tuhh.de/initialValues}Catchment.
 * 
 * @author Gernot Belger
 */
public class Catchment extends Feature_Impl
{
  private static final String NS_INIVALUES = NaModelConstants.NS_INIVALUES;

  public static final QName FEATURE_CATCHMENT = new QName( NS_INIVALUES, "Catchment" ); //$NON-NLS-1$

  private static final QName MEMBER_HYD = new QName( NaModelConstants.NS_INIVALUES, "hyd" ); //$NON-NLS-1$ 

  private static final QName PROP_FEATURE_ID = new QName( NS_INIVALUES, "featureId" );//$NON-NLS-1$

  private static final QName PROP_H = new QName( NS_INIVALUES, "h" );//$NON-NLS-1$

  private static final QName PROP_WS = new QName( NaModelConstants.NS_INIVALUES, "ws" ); //$NON-NLS-1$

  private static final QName PROP_HWGS = new QName( NaModelConstants.NS_INIVALUES, "hgws" ); //$NON-NLS-1$

  private static final QName PROP_QB = new QName( NaModelConstants.NS_INIVALUES, "qb" );//$NON-NLS-1$

  private FeatureBindingCollection<IniHyd> m_iniHyds;

  public Catchment( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  /**
   * Returns the feature id of the coresponding catchment in the namodel
   */
  public String getNaCatchmentID( )
  {
    return getProperty( PROP_FEATURE_ID, String.class );
  }

  public void setNaCatchmentID( final org.kalypso.model.hydrology.binding.model.Catchment catchment )
  {
    setProperty( PROP_FEATURE_ID, catchment.getId() );
  }

  public Double getH( )
  {
    return getProperty( PROP_H, Double.class );
  }

  public Double getWS( )
  {
    return getProperty( PROP_WS, Double.class );
  }

  public Double getHwgs( )
  {
    return getProperty( PROP_HWGS, Double.class );
  }

  public Double getQb( )
  {
    return getProperty( PROP_QB, Double.class );
  }

  public synchronized IFeatureBindingCollection<IniHyd> getIniHyds( )
  {
    if( m_iniHyds == null )
      m_iniHyds = new FeatureBindingCollection<IniHyd>( this, IniHyd.class, MEMBER_HYD );

    return m_iniHyds;
  }

}
