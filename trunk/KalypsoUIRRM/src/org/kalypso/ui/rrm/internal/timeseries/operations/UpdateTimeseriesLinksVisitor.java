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
package org.kalypso.ui.rrm.internal.timeseries.operations;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.lang3.ArrayUtils;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;

/**
 * Update outdated time series links to the new one.
 * 
 * @author Dirk Kuch
 */
public class UpdateTimeseriesLinksVisitor implements FeatureVisitor
{
  private final URL m_old;

  private final String m_href;

  public UpdateTimeseriesLinksVisitor( final ITimeseries old, final ITimeseries current )
  {
    m_old = toUrl( old );
    m_href = current.getDataLink().getHref();
  }

  private URL toUrl( final ITimeseries timeseries )
  {
    return timeseries.getDataLink().getLocation();
  }

  public UpdateTimeseriesLinksVisitor( final URL old, final String href )
  {
    m_old = old;
    m_href = href;
  }

  @Override
  public boolean visit( final Feature feature )
  {
    final IPropertyType[] links = doInspect( feature );
    if( ArrayUtils.isNotEmpty( links ) )
      doUpdate( feature, links );

    return true;
  }

  private IPropertyType[] doInspect( final Feature feature )
  {
    final Set<IPropertyType> equal = new LinkedHashSet<>();

    final IFeatureType featureType = feature.getFeatureType();
    final IPropertyType[] properties = featureType.getProperties();
    for( final IPropertyType property : properties )
    {
      final Object obj = feature.getProperty( property );
      if( obj instanceof TimeseriesLinkType )
      {
        try
        {
          final TimeseriesLinkType link = (TimeseriesLinkType) obj;
          final String href = link.getHref();
          if( href == null )
            continue;

          final URL url = UrlResolverSingleton.resolveUrl( feature.getWorkspace().getContext(), href );
          if( m_old.equals( url ) )
            equal.add( property );
        }
        catch( final MalformedURLException e )
        {
          e.printStackTrace();
        }
      }
    }

    return equal.toArray( new IPropertyType[] {} );
  }

  private void doUpdate( final Feature feature, final IPropertyType[] properties )
  {
    for( final IPropertyType property : properties )
    {
      final TimeseriesLinkType link = (TimeseriesLinkType) feature.getProperty( property );
      link.setHref( m_href );
    }

    feature.getWorkspace().fireModellEvent( new FeaturesChangedModellEvent( feature.getWorkspace(), new Feature[] { feature } ) );
  }
}