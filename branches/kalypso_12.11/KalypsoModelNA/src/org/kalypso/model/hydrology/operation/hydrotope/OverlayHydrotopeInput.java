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
package org.kalypso.model.hydrology.operation.hydrotope;

import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.binding.OverlayCollection;
import org.kalypso.model.hydrology.binding.OverlayElement;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.util.PolygonExtracter;

/**
 * @author Gernot Belger
 */
class OverlayHydrotopeInput extends AbstractHydrotopeInput<OverlayElement>
{
  private final NaModell m_naModel;

  public OverlayHydrotopeInput( final OverlayCollection overlay, final NaModell naModel )
  {
    super( overlay.getOverlayElements() );

    m_naModel = naModel;
  }

  @Override
  public String getLabel( )
  {
    return Messages.getString("OverlayHydrotopeInput_0"); //$NON-NLS-1$
  }

  @Override
  protected Polygon[] buildInverseMask( )
  {
    /* Build area of catchments */
    final GeometryFactory factory = new GeometryFactory();

    Geometry catchmentArea = factory.createPolygon( null, null );

    final IFeatureBindingCollection<Catchment> catchments = m_naModel.getCatchments();
    for( final Catchment catchment : catchments )
    {
      try
      {
        final GM_Polygon geometry = catchment.getGeometry();
        final Polygon catchmentPolygon = (Polygon) JTSAdapter.export( geometry );
        catchmentArea = catchmentArea.union( catchmentPolygon );
      }
      catch( final GM_Exception e )
      {
        e.printStackTrace();
      }
    }

    /* Cleanup catchment area */
    if( catchmentArea.getNumGeometries() > 1 )
    {
      catchmentArea = catchmentArea.union();
      catchmentArea = catchmentArea.buffer( 0.0 );
    }

    /* Substract all overlay */
    Geometry mask = catchmentArea;

    final IFeatureBindingCollection<OverlayElement> overlayElements = getFeatures();
    for( final OverlayElement overlayElement : overlayElements )
    {
      try
      {
        final GM_Object geometry = overlayElement.getGeometry();
        final Geometry overlayGeometry = JTSAdapter.export( geometry );

        final List<Polygon> overlayPolygons = PolygonExtracter.getPolygons( overlayGeometry );

        for( final Polygon overlayPolygon : overlayPolygons )
        {
          mask = mask.difference( overlayPolygon );
        }
      }
      catch( final GM_Exception e )
      {
        e.printStackTrace();
      }
    }

    /* Cleanup */
    if( mask.getNumGeometries() > 1 )
    {
      mask = mask.union();
      mask = mask.buffer( 0.0 );
    }

    /* Return result */
    final List<Polygon> polygons = PolygonExtracter.getPolygons( mask );
    return polygons.toArray( new Polygon[polygons.size()] );
  }

  @Override
  public IStatus validateInput( )
  {
    final IStatusCollector log = new StatusCollector( ModelNA.PLUGIN_ID );

// final IFeatureBindingCollection<OverlayElement> features = getFeatures();
// for( final OverlayElement overlay : features )
// {
    // FIXME: what to check?
// overlay.getDRWBMDefinition();

// final Double gwFactor = overlay.getGWFactor();
// if( gwFactor == null )
// log.add( IStatus.ERROR, formatMessage( "groundwater factor is not set", overlay ) );
// else if( gwFactor < 0.0 || gwFactor > 1.0 )
// log.add( IStatus.ERROR, formatMessage( "groundwater factor is outside it's valid range [0.0 - 1.0]", overlay ) );
//
// final Double maxPerkRate = overlay.getMaxPerkulationsRate();
// if( maxPerkRate == null )
// log.add( IStatus.ERROR, formatMessage( "maximal perkolation rate is not set", overlay ) );
// // TODO: range check?
// }

    return log.asMultiStatus( STR_ATTRIBUTES );
  }
}