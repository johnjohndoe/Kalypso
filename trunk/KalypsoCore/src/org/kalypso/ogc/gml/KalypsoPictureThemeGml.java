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

import java.net.URL;

import javax.media.jai.JAI;
import javax.media.jai.RenderedOp;
import javax.media.jai.TiledImage;

import ogc31.www.opengis.net.gml.FileType;
import ogc31.www.opengis.net.gml.RangeSetType;

import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.template.types.StyledLayerType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage2;

/**
 * @author kuch
 */
public class KalypsoPictureThemeGml extends KalypsoPictureTheme
{
  public KalypsoPictureThemeGml( final StyledLayerType layerType, final URL context, final IMapModell modell ) throws Exception
  {
    super( layerType, context, modell );

    // TODO: botch... find a better way of loading gml workspace!
    // maybe it could be treated as normal gm with a special display element?
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( UrlResolverSingleton.resolveUrl( context, layerType.getHref() ), null );
    final Feature fRoot = workspace.getRootFeature();

    final RectifiedGridCoverage2 coverage2 = new RectifiedGridCoverage2( fRoot );

    /* imgFile */
    final RangeSetType rangeSet = coverage2.getRangeSet();
    final FileType type = rangeSet.getFile();

    final URL imageContext = UrlResolverSingleton.resolveUrl( context, layerType.getHref() );

    final URL imageUrl = UrlResolverSingleton.resolveUrl( imageContext, type.getFileName() );
    final RenderedOp image = JAI.create( "url", imageUrl );
    m_image = new TiledImage( image, true );

    /* recGridDomain */
    m_domain = coverage2.getGridDomain();
  }

}
