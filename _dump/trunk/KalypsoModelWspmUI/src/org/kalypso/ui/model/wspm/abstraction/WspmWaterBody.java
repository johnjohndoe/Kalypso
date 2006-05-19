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
package org.kalypso.ui.model.wspm.abstraction;

import javax.xml.namespace.QName;

import org.kalypso.contribs.javax.xml.namespace.QNameUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ui.model.wspm.IWspmConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_LineString;

/**
 * @author belger
 */
public class WspmWaterBody implements IWspmConstants
{
  private final Feature m_water;

  public WspmWaterBody( final Feature water )
  {
    if( !QNameUtilities.equals( water.getFeatureType().getQName(), IWspmConstants.NS_WSPM, "WaterBody" ) )
      throw new IllegalStateException( "Feature is of wrong type: " + water );

    m_water = water;
  }

  public String getName( )
  {
    return NamedFeatureHelper.getName( m_water );
  }

  public void setName( final String name )
  {
    NamedFeatureHelper.setName( m_water, name );
  }

  public String getDescription( )
  {
    return NamedFeatureHelper.getDescription( m_water );
  }

  public void setDescription( final String desc )
  {
    NamedFeatureHelper.setDescription( m_water, desc );
  }

  public TuhhReach createNewReach( )
  {
    final GMLWorkspace workspace = m_water.getWorkspace();

    final IFeatureType reachFT = workspace.getFeatureType( new QName( NS_WSPM_TUHH, "ReachWspmTuhhSteadyState" ) );

    final Feature newTuhhReach = workspace.createFeature( m_water, reachFT );

    final FeatureList reachList = (FeatureList) m_water.getProperty( new QName( NS_WSPM, "reachMember" ) );
    reachList.add( newTuhhReach );

    final TuhhReach tuhhReach = new TuhhReach( newTuhhReach );

    tuhhReach.setWaterBody( this );

    return tuhhReach;
  }

  public Feature getFeature( )
  {
    return m_water;
  }

  public WspmProfileReference createNewProfile( final String hrefHint )
  {
    final FeatureList profileMembers = (FeatureList) m_water.getProperty( new QName( NS_WSPM, "profileMember" ) );

    final String href = "Profile/" + hrefHint;
    // TODO: check if this reference is already in use

    profileMembers.add( href );

    return new WspmProfileReference( href );
  }

  public void setRefNr( final String refNr )
  {
    final IPropertyType prop = m_water.getFeatureType().getProperty( new QName( NS_WSPM, "waterBodyRefNr" ) );
    m_water.setProperty( prop, refNr );
  }

  public void setCenterLine( final GM_LineString lineString )
  {
    // does not work, because schema is bad
//    final IPropertyType prop = m_water.getFeatureType().getProperty( new QName( NS_WSPM, "waterBodyCenterLine" ) );
//    m_water.setProperty( prop, lineString );
  }

  public void setDirectionUpstreams( final boolean directionIsUpstream )
  {
    m_water.setProperty( new QName( NS_WSPM, "isDirectionUpstream" ), new Boolean( directionIsUpstream ) );
  }

  public Feature createRunOffEvent(  )
  {
    final FeatureList runOffMembers = (FeatureList) m_water.getProperty( new QName( NS_WSPM, "runOffEventMember" ) );

    final IRelationType parentFeatureTypeProperty = runOffMembers.getParentFeatureTypeProperty();
    final IFeatureType targetFeatureType = parentFeatureTypeProperty.getTargetFeatureType();
    
    final Feature runOffFeature = m_water.getWorkspace().createFeature( runOffMembers.getParentFeature(), targetFeatureType );
    runOffMembers.add( runOffFeature );
    
    return runOffFeature;
  }

  public boolean isDirectionUpstreams( )
  {
    return (Boolean) m_water.getProperty( new QName( NS_WSPM, "isDirectionUpstream" ) );
  }
}
