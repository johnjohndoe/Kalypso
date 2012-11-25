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

import java.net.MalformedURLException;
import java.net.URL;

import org.apache.commons.lang.ObjectUtils;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.rcm.binding.IOmbrometer;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * @author Gernot Belger
 */
public class Ombrometer extends Feature_Impl implements IOmbrometer
{
  public Ombrometer( final Object parent, final IRelationType parentRelation, final IFeatureType featureType, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, featureType, id, propValues );
  }

  @SuppressWarnings("unchecked")
  @Override
  public GM_Surface<GM_SurfacePatch> getAffectedArea( )
  {
    return getProperty( PROPERTY_AFFECTEDAREA, GM_Surface.class );
  }

  @Override
  public void setAffectedArea( final GM_Surface< ? > area )
  {
    setProperty( PROPERTY_AFFECTEDAREA, area );
  }

  @Override
  public String getExternalId( )
  {
    return getProperty( PROPERTY_EXTERNALID, String.class );
  }

  @Override
  public String getShortName( )
  {
    return getProperty( PROPERTY_SHORTNAME, String.class );
  }

  @Override
  public GM_Point getStationLocation( )
  {
    return getProperty( PROPERTY_STATIONLOCATION, GM_Point.class );
  }

  @Override
  public boolean isUsed( )
  {
    final Boolean isUsed = getProperty( PROPERTY_ISUSED, Boolean.class );
    return isUsed == null ? true : isUsed.booleanValue();
  }

  @Override
  public void setUsed( final boolean used )
  {
    setProperty( PROPERTY_ISUSED, used );
  }

  @Override
  public FeatureChange changeIsUsed( final Boolean isUsed )
  {
    /* If geometry is not set, never activate this ombro. */
    final GM_Point geom = getStationLocation();
    if( geom == null )
      return null;

    final Boolean oldIsUsed = getProperty( PROPERTY_ISUSED, Boolean.class );
    if( ObjectUtils.equals( oldIsUsed, isUsed ) )
      return null;

    return new FeatureChange( this, PROPERTY_ISUSED, isUsed );
  }

  @Override
  public void setExternalId( final String externalId )
  {
    setProperty( PROPERTY_EXTERNALID, externalId );
  }

  @Override
  public void setShortName( final String shortName )
  {
    setProperty( PROPERTY_SHORTNAME, shortName );
  }

  @Override
  public void setStationLocation( final GM_Point location )
  {
    setProperty( PROPERTY_STATIONLOCATION, location );
  }

  @Override
  public IObservation getTimeserie( ) throws MalformedURLException, SensorException
  {
    final TimeseriesLinkType link = getProperty( IOmbrometer.PROPERTY_PRECIPITATION1, TimeseriesLinkType.class );
    final URL context = getWorkspace().getContext();
    final String href = link.getHref();
    final URL linkUrl = UrlResolverSingleton.resolveUrl( context, href );

    return ZmlFactory.parseXML( linkUrl );
  }

  @Override
  public ZmlLink getTimeserieLink( )
  {
    return new ZmlLink( this, IOmbrometer.PROPERTY_PRECIPITATION1 );
  }
}