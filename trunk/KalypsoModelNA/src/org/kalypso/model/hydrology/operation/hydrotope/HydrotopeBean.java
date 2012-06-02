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

import java.util.Arrays;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.hydrology.binding.Geology;
import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.Landuse;
import org.kalypso.model.hydrology.binding.SoilType;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IXLinkedFeature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Temporary representation of a hydrotope during the intersection.
 * 
 * @author Gernot Belger
 */
public class HydrotopeBean
{
  private final Polygon[] m_polygons;

  private final Feature[] m_features;

  private String m_hydrotopeHash;

  public HydrotopeBean( final Polygon polygon, final Feature feature )
  {
    this( new Polygon[] { polygon }, new Feature[] { feature } );
  }

  private HydrotopeBean( final Polygon[] polygons, final Feature[] features )
  {
    m_polygons = polygons;
    m_features = features;
  }

  public HydrotopeBean mergeGeometries( final HydrotopeBean other )
  {
    Arrays.equals( m_features, other.m_features );

    final Polygon[] newPolyogns = ArrayUtils.addAll( m_polygons, other.m_polygons );
    return new HydrotopeBean( newPolyogns, m_features );
  }

  public HydrotopeBean merge( final HydrotopeBean other, final Polygon newGeometry )
  {
    final Feature[] newFeatures = ArrayUtils.addAll( m_features, other.m_features );

    return new HydrotopeBean( new Polygon[] { newGeometry }, newFeatures );
  }

  public String getHydrotopeHash( )
  {
    if( m_hydrotopeHash == null )
      m_hydrotopeHash = buildHash();

    return m_hydrotopeHash;
  }

  private String buildHash( )
  {
// return ObjectUtils.identityToString( this );

    final StringBuilder buffer = new StringBuilder();

    for( final Feature feature : m_features )
    {
      buffer.append( feature.getQualifiedName() );
      buffer.append( '#' );
      buffer.append( feature.getId() );
      buffer.append( '#' );
    }

    return buffer.toString();
  }

  public Polygon[] getGeometry( )
  {
    return m_polygons;
  }

  public String toErrorString( )
  {
    final String[] labels = new String[m_features.length];

    for( int i = 0; i < labels.length; i++ )
      labels[i] = m_features[i].getName();

    return StringUtils.join( labels, '/' );
  }

  public void configureHydrotope( final IHydrotope hydrotope, final int count ) throws CoreException
  {
    final GM_MultiSurface lGeometry = toMultiSurface();
    hydrotope.setGeometry( lGeometry );

    copyAttributes( hydrotope, count );
  }

  private GM_MultiSurface toMultiSurface( )
  {
    try
    {
      final GeometryFactory factory = m_polygons[0].getFactory();

      final MultiPolygon multiPolygon = factory.createMultiPolygon( m_polygons );

      final String srs = JTSAdapter.toSrs( m_polygons[0].getSRID() );
      return (GM_MultiSurface) JTSAdapter.wrap( multiPolygon, srs );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  private void copyAttributes( final IHydrotope hydrotop, final int count ) throws CoreException
  {
    Catchment catchment = null;
    Landuse landuse = null;
    Geology geology = null;
    SoilType pedology = null;
    // TODO: add suds

    for( final Feature feature : m_features )
    {
      if( feature instanceof Catchment )
        catchment = (Catchment) feature;
      else if( feature instanceof Landuse )
        landuse = (Landuse) feature;
      else if( feature instanceof Geology )
        geology = (Geology) feature;
      else if( feature instanceof SoilType )
        pedology = (SoilType) feature;
    }

    if( catchment == null || landuse == null || geology == null || pedology == null )
    {
      final IStatus status = new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, "Data missing" ); //$NON-NLS-1$
      // this internal error should never happen.
      throw new CoreException( status );
    }

    /* name */
    final String name = String.format( "Hydrotope %d", count );
    hydrotop.setName( name );

    // sealing correction factor is now the product of factors from catchment and landuse
    final double corrSealing = catchment.getCorrSealing() * landuse.getCorrSealing();
    hydrotop.setCorrSealing( corrSealing );

    final String href = String.format( "modell.gml#%s", catchment.getId() ); //$NON-NLS-1$
    hydrotop.setCatchmentMember( href );

    // FIXME: check all input data if all values have been set

    final IXLinkedFeature landuseClass = landuse.getLanduse();
    if( landuseClass != null )
      hydrotop.setLanduse( landuseClass.getName() );

    final IXLinkedFeature soiltypeClass = pedology.getSoilType();
    if( soiltypeClass != null )
      // FIXME: mega ugly
      hydrotop.setSoilType( soiltypeClass.toString().substring( soiltypeClass.toString().indexOf( "#" ) + 1 ) ); //$NON-NLS-1$

    hydrotop.setMaxPerkolationRate( geology.getMaxPerkulationsRate() );
    hydrotop.setGWFactor( geology.getGWFactor() );
  }
}