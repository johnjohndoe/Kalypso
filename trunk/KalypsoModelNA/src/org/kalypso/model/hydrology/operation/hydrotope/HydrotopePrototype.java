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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.hydrology.binding.Geology;
import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.Landuse;
import org.kalypso.model.hydrology.binding.OverlayElement;
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
 * Holds the data needed in order to build a hydrotope
 * 
 * @author Gernot Belger
 */
public class HydrotopePrototype
{
  private final Polygon[] m_polygons;

  Catchment m_catchment = null;

  Landuse m_landuse = null;

  Geology m_geology = null;

  SoilType m_pedology = null;

  OverlayElement m_overlay = null;

  public HydrotopePrototype( final Feature[] features, final Polygon[] polygons ) throws CoreException
  {
    m_polygons = polygons;

    /* Initialize data */
    for( final Feature feature : features )
    {
      if( feature instanceof Catchment )
        m_catchment = (Catchment) feature;
      else if( feature instanceof Landuse )
        m_landuse = (Landuse) feature;
      else if( feature instanceof Geology )
        m_geology = (Geology) feature;
      else if( feature instanceof SoilType )
        m_pedology = (SoilType) feature;
      else if( feature instanceof OverlayElement )
        m_overlay = (OverlayElement) feature;
    }

    // REMARK: not checking for overlay: may be null
    if( m_catchment == null || m_landuse == null || m_geology == null || m_pedology == null )
    {
      final IStatus status = new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, "Data missing" ); //$NON-NLS-1$
      // this internal error should never happen.
      throw new CoreException( status );
    }
  }

  public void copyAttributes( final IHydrotope hydrotope, final int count )
  {
    final GM_MultiSurface lGeometry = toMultiSurface();
    hydrotope.setGeometry( lGeometry );

    /* name */
    final String name = String.format( "Hydrotope %d", count );
    hydrotope.setName( name );

    final String catchmentId = getCatchmentId();
    final String catchmentRef = String.format( "modell.gml#%s", catchmentId ); //$NON-NLS-1$
    hydrotope.setCatchmentMember( catchmentRef );

    final String landuseClassName = getLanduseClass();
    if( landuseClassName != null )
      hydrotope.setLanduse( landuseClassName );

    final String soilTypeClassName = getSoilTypeClass();
    if( soilTypeClassName != null )
      hydrotope.setSoilType( soilTypeClassName );

    hydrotope.setCorrSealing( getSealingfactor() );

    hydrotope.setMaxPerkolationRate( getMaxPerkulationRate() );

    hydrotope.setGWFactor( getGroundwaterFactor() );

    final String drwbDefinitionRef = getDRWBMDefinition();
    hydrotope.setDRWBMDefinition( drwbDefinitionRef );
  }

  public String buildAttributeHashKey( )
  {
    final StringBuilder buffer = new StringBuilder();

    /* catchment */
    final String catchmentId = getCatchmentId();
    buffer.append( catchmentId );

    buffer.append( '#' );

    /* landuse */
    final String landuseClassName = getLanduseClass();
    buffer.append( landuseClassName );
    buffer.append( '#' );

    /* pedology */
    final String soilTypeClassName = getSoilTypeClass();
    buffer.append( soilTypeClassName );
    buffer.append( '#' );

    /* overlay */
    final String drwbDefinitionRef = getDRWBMDefinition();
    buffer.append( drwbDefinitionRef );
    buffer.append( '#' );

    /* numeric values */
    // FIXME: REMARK: using 3 decimal points for all values to build hash.
    buffer.append( String.format( "%.3f", getSealingfactor() ) );
    buffer.append( '#' );
    buffer.append( String.format( "%.3f", getMaxPerkulationRate() ) );
    buffer.append( '#' );
    buffer.append( String.format( "%.3f", getGroundwaterFactor() ) );

    return buffer.toString();
  }

  private String getDRWBMDefinition( )
  {
    if( m_overlay == null )
      return null;

    final IXLinkedFeature drwbmDefinition = m_overlay.getDRWBMDefinition();
    if( drwbmDefinition == null )
      return null;

    return drwbmDefinition.getHref();
  }

  private String getSoilTypeClass( )
  {
    final IXLinkedFeature soiltypeClass = m_pedology.getSoilType();
    if( soiltypeClass == null )
      return null;

    return soiltypeClass.getName();
  }

  private String getCatchmentId( )
  {
    return m_catchment.getId();
  }

  /**
   * sealing correction factor is now the product of factors from catchment and landuse
   */
  private double getSealingfactor( )
  {
    return m_catchment.getCorrSealing() * m_landuse.getCorrSealing();
  }

  private double getGroundwaterFactor( )
  {
    return m_geology.getGWFactor();
  }

  private double getMaxPerkulationRate( )
  {
    final double maxPerkulationsRate = m_geology.getMaxPerkulationsRate();
    return maxPerkulationsRate;
  }

  private String getLanduseClass( )
  {
    final IXLinkedFeature landuseClass = m_landuse.getLanduse();
    if( landuseClass == null )
      return null;

    return landuseClass.getName();
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
}