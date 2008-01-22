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
package org.kalypso.ogc.gml;

import java.awt.Graphics;
import java.net.URL;

import javax.media.jai.JAI;
import javax.media.jai.RenderedOp;
import javax.media.jai.TiledImage;

import ogc31.www.opengis.net.gml.FileType;
import ogc31.www.opengis.net.gml.RangeSetType;

import org.apache.commons.lang.NotImplementedException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.template.types.StyledLayerType;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridCoverage;

/**
 * @author kuch
 */
public class KalypsoPictureThemeGml extends KalypsoPictureTheme
{
  private final ICoverageCollection m_coverages;

  public KalypsoPictureThemeGml( final StyledLayerType layerType, final URL context, final IMapModell modell, final String legendIcon ) throws Exception
  {
    super( layerType, context, modell, legendIcon );

    // TODO: botch... find a better way of loading gml workspace!
    // maybe it could be treated as normal gm with a special display element?
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( UrlResolverSingleton.resolveUrl( getURLContext(), getStyledLayerType().getHref() ), null );
    final Feature fRoot = workspace.getRootFeature();

    m_coverages = (ICoverageCollection) fRoot.getAdapter( ICoverageCollection.class );
    if( m_coverages.size() != 1 )
      throw new NotImplementedException( "Collection of Images not implemented!" );

    for( final ICoverage coverage : m_coverages )
    {
      final RectifiedGridCoverage coverage2 = (RectifiedGridCoverage) coverage;

      /* recGridDomain */
      setRectifiedGridDomain( coverage2.getGridDomain() );

      // HACK: we assume, that we only have exactly ONE coverage per picture-theme
      break;
    }

  }

  /**
   * @see org.kalypso.ogc.gml.KalypsoPictureTheme#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, double,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, boolean, org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public void paint( final Graphics g, final GeoTransform p, final double scale, final GM_Envelope bbox, final boolean selected, final IProgressMonitor monitor )
  {
    /** image creation removed from constructor, so not visible themes will not be loaded! */
    if( getImage() == null )
      try
      {
        for( final ICoverage coverage : m_coverages )
        {
          final RectifiedGridCoverage coverage2 = (RectifiedGridCoverage) coverage;

          /* imgFile */
          final RangeSetType rangeSet = coverage2.getRangeSet();
          final FileType type = rangeSet.getFile();

          final URL imageContext = UrlResolverSingleton.resolveUrl( getURLContext(), getStyledLayerType().getHref() );

          final URL imageUrl = UrlResolverSingleton.resolveUrl( imageContext, type.getFileName() );
          final RenderedOp image = JAI.create( "url", imageUrl );
          setImage( new TiledImage( image, true ) );
          image.dispose();

          // HACK: we assume, that we only have exactly ONE coverage per picture-theme

          break;
        }
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }

    super.paint( g, p, scale, bbox, selected, monitor );
  }
}
