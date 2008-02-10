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

import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * This is an abstraction layer over an wspmproje gml instance.
 * <p>
 * It has NO own member variables, everything is backed by the given feature instance.
 * </p>
 * 
 * @author Gernot Belger
 */
public class WspmProject extends AbstractFeatureBinder implements IWspmConstants
{
  private static final QName QNAME_WATER_BODY_MEMBER = new QName( NS_WSPM, "waterBodyMember" );

  public final static QName QNAME = new QName( IWspmConstants.NS_WSPMPROJ, "WspmProject" );

  public WspmProject( final Feature wspProject )
  {
    super( wspProject, QNAME );
  }

  public WspmWaterBody[] getWaterBodies( )
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
    return getProperty( QNAME_WATER_BODY_MEMBER, FeatureList.class );
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

    final Feature feature = FeatureHelper.addFeature( getFeature(), QNAME_WATER_BODY_MEMBER, null );

    final WspmWaterBody wspmWaterBody = new WspmWaterBody( feature );

    // set default values
    wspmWaterBody.setName( name );
    wspmWaterBody.setDescription( "" );
    wspmWaterBody.setRefNr( "" );
    wspmWaterBody.setDirectionUpstreams( isDirectionUpstreams );

    return wspmWaterBody;
  }

}
