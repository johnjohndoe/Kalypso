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
package org.kalypsodeegree_impl.graphics.sld.awt;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.TexturePaint;
import java.awt.image.BufferedImage;

import org.kalypso.contribs.java.awt.ColorUtilities;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.GraphicFill;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.graphics.sld.Symbolizer_Impl.UOM;

/**
 * @author Thomas Jung
 */
public class FillPainter
{
  private final Color m_fillColor;

  private final BufferedImage m_texture;

  private final GeoTransform m_world2screen;

  public FillPainter( final Fill fill, final Feature feature, final UOM uom, final GeoTransform world2screen ) throws FilterEvaluationException
  {
    m_world2screen = world2screen;
    m_fillColor = fill == null ? null : ColorUtilities.createTransparent( fill.getFill( feature ), fill.getOpacity( feature ) );

    final GraphicFill gFill = fill == null ? null : fill.getGraphicFill();

    if( gFill != null && uom != null && world2screen != null )
      m_texture = gFill.getGraphic().getAsImage( feature, uom, world2screen );
    else
      m_texture = null;
  }

  public GeoTransform getWorld2Screen( )
  {
    return m_world2screen;
  }

  public boolean isVisible( )
  {
    return m_fillColor != null && m_fillColor.getAlpha() > 0;
  }

  public void prepareGraphics( final Graphics2D g2 )
  {
    if( m_texture == null )
      g2.setColor( m_fillColor );
    else
    {
      // TODO: rotation is not considered here
      final Rectangle anchor = new Rectangle( 0, 0, m_texture.getWidth( null ), m_texture.getHeight( null ) );
      g2.setPaint( new TexturePaint( m_texture, anchor ) );
    }
  }

}
