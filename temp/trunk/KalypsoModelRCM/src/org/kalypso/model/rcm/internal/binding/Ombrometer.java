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
package org.kalypso.model.rcm.internal.binding;

import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.namespace.QName;

import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.rcm.binding.IOmbrometer;
import org.kalypso.model.rcm.internal.UrlCatalogRcm;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.gml.binding.AbstractFeatureBinding;

/**
 * @author Gernot Belger
 */
public class Ombrometer extends AbstractFeatureBinding implements IOmbrometer
{
  public Ombrometer( final Object parent, final IRelationType parentRelation, final IFeatureType featureType, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, featureType, id, propValues );
  }

  /**
   * @see org.kalypso.model.rcm.binding.IOmbrometer#getAffectedArea()
   */
  @SuppressWarnings("unchecked")
  @Override
  public GM_Surface<GM_SurfacePatch> getAffectedArea( )
  {
    return getProperty( QNAME_PROP_AFFECTEDAREA, GM_Surface.class );
  }

  /**
   * @see org.kalypso.model.rcm.binding.IOmbrometer#getAffectedArea(org.kalypsodeegree.model.geometry.GM_Surface)
   */
  @Override
  public void setAffectedArea( final GM_Surface<GM_SurfacePatch> area )
  {
    setProperty( QNAME_PROP_AFFECTEDAREA, area );
  }

  /**
   * @see org.kalypso.model.rcm.binding.IOmbrometer#getExternalId()
   */
  @Override
  public String getExternalId( )
  {
    return getProperty( QNAME_PROP_EXTERNALID, String.class );
  }

  /**
   * @see org.kalypso.model.rcm.binding.IOmbrometer#getShortName()
   */
  @Override
  public String getShortName( )
  {
    return getProperty( QNAME_PROP_SHORTNAME, String.class );
  }

  /**
   * @see org.kalypso.model.rcm.binding.IOmbrometer#getStationLocation()
   */
  @Override
  public GM_Point getStationLocation( )
  {
    return getProperty( QNAME_PROP_STATIONLOCATION, GM_Point.class );
  }

  /**
   * @see org.kalypso.model.rcm.binding.IOmbrometer#isUsed()
   */
  @Override
  public boolean isUsed( )
  {
    final Boolean isUsed = getProperty( QNAME_PROP_ISUSED, Boolean.class );
    return isUsed == null ? true : isUsed.booleanValue();
  }

  /**
   * @see org.kalypso.model.rcm.binding.IOmbrometer#isUsed(boolean)
   */
  @Override
  public void setUsed( final boolean used )
  {
    setProperty( QNAME_PROP_ISUSED, used );
  }

  /**
   * @see org.kalypso.model.rcm.binding.IOmbrometer#setExternalId(java.lang.String)
   */
  @Override
  public void setExternalId( final String externalId )
  {
    setProperty( QNAME_PROP_EXTERNALID, externalId );
  }

  /**
   * @see org.kalypso.model.rcm.binding.IOmbrometer#setShortName(java.lang.String)
   */
  @Override
  public void setShortName( final String shortName )
  {
    setProperty( QNAME_PROP_SHORTNAME, shortName );
  }

  /**
   * @see org.kalypso.model.rcm.binding.IOmbrometer#setStationLocation(org.kalypsodeegree.model.geometry.GM_Point)
   */
  @Override
  public void setStationLocation( final GM_Point location )
  {
    setProperty( QNAME_PROP_STATIONLOCATION, location );
  }

  /**
   * @see org.kalypso.model.rcm.binding.IOmbrometer#getTimeserie(java.lang.String)
   */
  @Override
  public IObservation getTimeserie( final String property ) throws MalformedURLException, SensorException
  {
    final URL context = getFeature().getWorkspace().getContext();
    final TimeseriesLinkType link = getProperty( new QName( UrlCatalogRcm.NS_OMBROMETER, property ), TimeseriesLinkType.class );
    final String href = link.getHref();
    final URL linkUrl = UrlResolverSingleton.resolveUrl( context, href );

    return ZmlFactory.parseXML( linkUrl, link.toString() );
  }
}
