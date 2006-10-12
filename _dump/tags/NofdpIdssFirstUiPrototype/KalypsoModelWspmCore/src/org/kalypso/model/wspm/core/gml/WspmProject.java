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
package org.kalypso.model.wspm.core.gml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.contribs.javax.xml.namespace.QNameUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.binding.NamedFeatureHelper;

/**
 * This is an abstraction layer over an wspmproje gml instance.
 * <p>
 * It has NO own member variables, everything is backed by the given feature instance.
 * </p>
 * 
 * @author belger
 */
public class WspmProject implements IWspmConstants
{
  private final Feature m_wspProject;

  public WspmProject( final Feature wspProject )
  {
    if( !QNameUtilities.equals( wspProject.getFeatureType().getQName(), IWspmConstants.NS_WSPMPROJ, "WspmProject" ) )
      throw new IllegalStateException( "wspmProject ist no wspmProject" );

    m_wspProject = wspProject;
  }

  public Feature getFeature( )
  {
    return m_wspProject;
  }

  public String getName( )
  {
    return NamedFeatureHelper.getName( m_wspProject );
  }

  public void setName( final String name )
  {
    NamedFeatureHelper.setName( m_wspProject, name );
  }

  public String getDescription( )
  {
    return NamedFeatureHelper.getDescription( m_wspProject );
  }

  public void setDescription( final String desc )
  {
    NamedFeatureHelper.setDescription( m_wspProject, desc );
  }

  protected WspmWaterBody[] getWaterBodies( )
  {
    final FeatureList waters = getWaterBodyList();
    final List<WspmWaterBody> waterList = new ArrayList<WspmWaterBody>( waters.size() );
    for( final Object object : waters )
    {
      final Feature f = (Feature) object;
      waterList.add( new WspmWaterBody( f ) );
    }

    return waterList.toArray( new WspmWaterBody[waterList.size()] );
  }

  private FeatureList getWaterBodyList( )
  {
    return (FeatureList) m_wspProject.getProperty( new QName( NS_WSPM, "waterBodyMember" ) );
  }

  public WspmWaterBody findWater( final String waterName )
  {
    final WspmWaterBody[] waters = getWaterBodies();
    for( final WspmWaterBody body : waters )
    {
      if( waterName.equals( body.getName() ) )
        return body;
    }

    return null;
  }

  /**
   * Creates a new water body and adds it to this project.
   * <p>
   * If a waterBody with the same name is already present, this will be retuned instead.
   * </p>
   */
  public WspmWaterBody createWaterBody( final String name, final boolean isDirectionUpstreams ) throws GMLSchemaException
  {
    final WspmWaterBody water = findWater( name );
    if( water != null )
      return water;

    final Feature feature = FeatureHelper.addFeature( getFeature(), new QName( NS_WSPM, "waterBodyMember" ), null );

    final WspmWaterBody wspmWaterBody = new WspmWaterBody( feature );

    // set default values
    wspmWaterBody.setName( name );
    wspmWaterBody.setDescription( "" );
    wspmWaterBody.setRefNr( "" );
    wspmWaterBody.setDirectionUpstreams( isDirectionUpstreams );

    return wspmWaterBody;
  }

}
