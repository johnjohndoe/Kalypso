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
package org.kalypso.model.rcm.internal.binding;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.rcm.RcmConstants;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * The catchment generator.
 * 
 * @author Holger Albert
 */
public class CatchmentGenerator extends Feature_Impl
{
  /**
   * The qname of the area name property.
   */
  public static final QName QNAME_AREA_NAME_PROPERTY = new QName( RcmConstants.NS_CM, "areaNameProperty" );

  /**
   * The qname of the area property.
   */
  public static final QName QNAME_AREA_PROPERTY = new QName( RcmConstants.NS_CM, "areaProperty" );

  /**
   * The qname of the catchment member.
   */
  public static final QName QNAME_CATCHMENT_MEMBER = new QName( RcmConstants.NS_CM, "catchmentMember" );

  /**
   * The qname of the catchment.
   */
  public static final QName QNAME_CATCHMENT = new QName( RcmConstants.NS_CM, "Catchment" );

  /**
   * The constructor.
   * 
   * @param parent
   * @param parentRelation
   * @param ft
   * @param id
   * @param propValues
   */
  public CatchmentGenerator( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  /**
   * This function returns the area name property.
   * 
   * @return The area name property.
   */
  public String getAreaNameProperty( )
  {
    return getProperty( QNAME_AREA_NAME_PROPERTY, String.class );
  }

  /**
   * This function returns the area property.
   * 
   * @return The area property.
   */
  public String getAreaProperty( )
  {
    return getProperty( QNAME_AREA_PROPERTY, String.class );
  }

  /**
   * This function returns all catchments.
   * 
   * @return All catchments.
   */
  public Catchment[] getCatchments( )
  {
    /* Memory for the results. */
    final List<Catchment> results = new ArrayList<Catchment>();

    /* Get all catchments. */
    final FeatureList catchments = (FeatureList) getProperty( QNAME_CATCHMENT_MEMBER );
    for( int i = 0; i < catchments.size(); i++ )
      results.add( (Catchment) catchments.get( i ) );

    return results.toArray( new Catchment[] {} );
  }
}