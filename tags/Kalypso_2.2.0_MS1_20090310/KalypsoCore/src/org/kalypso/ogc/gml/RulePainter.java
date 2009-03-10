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
package org.kalypso.ogc.gml;

import java.awt.Rectangle;
import java.awt.image.BufferedImage;

import org.eclipse.swt.graphics.GC;
import org.kalypsodeegree.graphics.sld.ExternalGraphic;
import org.kalypsodeegree.graphics.sld.Graphic;
import org.kalypsodeegree.graphics.sld.LegendGraphic;
import org.kalypsodeegree.graphics.sld.ParameterValueType;
import org.kalypsodeegree.graphics.sld.PointSymbolizer;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.graphics.sld.Symbolizer_Impl;

/**
 * @author Gernot Belger
 */
public class RulePainter
{
  public static void paint( final Rule rule, final GC gc ) throws Exception
  {
    final Feature feature = Symbolizer_Impl.createPseudoFeature();

    final LegendGraphic legendGraphic = rule.getLegendGraphic();
    if( legendGraphic != null )
    {
      final Graphic graphic = legendGraphic.getGraphic();
      graphic.paint( gc, feature );
    }
    else
    {
      final Symbolizer[] symbolizers = rule.getSymbolizers();
      for( final Symbolizer symbolizer : symbolizers )
      {
        symbolizer.paint( gc, feature );
      }
    }
  }

  /**
   * This function returns the size an image should have, for showing the complete symbol without scaling. It can only
   * check for legend graphics and point symbolizers.
   * 
   * @param rule
   *          The rule.
   * @return The size an image should have, for showing the complete symbol without scaling. It can only check for
   *         legend graphics and point symbolizers.
   */
  public static Rectangle getSize( final Rule rule )
  {
    try
    {
      final LegendGraphic legendGraphic = rule.getLegendGraphic();
      if( legendGraphic != null )
      {
        /* Get the legend graphic. */
        final Graphic graphic = legendGraphic.getGraphic();

        return getSize( graphic );
      }

      final Symbolizer[] symbolizers = rule.getSymbolizers();

      int maxWidth = 0;
      int maxHeight = 0;
      for( final Symbolizer symbolizer : symbolizers )
      {
        if( symbolizer instanceof PointSymbolizer )
        {
          final PointSymbolizer pointSymbolizer = (PointSymbolizer) symbolizer;
          final Graphic graphic = pointSymbolizer.getGraphic();
          if( graphic != null )
          {
            final Rectangle size = getSize( graphic );
            maxWidth = Math.max( maxWidth, size.width );
            maxHeight = Math.max( maxHeight, size.height );
          }
        }

        // TODO How to find out the size from the other symbolizers?
      }

      return new Rectangle( maxWidth, maxHeight );
    }
    catch( final Exception ex )
    {
      ex.printStackTrace();
      return new Rectangle( 0, 0 );
    }
  }

  /**
   * This function returns the original size of the image inside this graphic (not the destination size).
   * 
   * @param graphic
   *          The graphic, which normally contains an image or some marks.
   * @return The original size of the image.
   */
  private static Rectangle getSize( final Graphic graphic )
  {
    int maxWidth = 0;
    int maxHeight = 0;

    final Object[] marksAndExtGraphics = graphic.getMarksAndExtGraphics();
    for( final Object o : marksAndExtGraphics )
    {
      if( o instanceof ExternalGraphic )
      {
        final ExternalGraphic externalGraphic = (ExternalGraphic) o;
        final ParameterValueType sizeParameter = graphic.getSizeParameter();
        final Object[] components = sizeParameter == null ? null : sizeParameter.getComponents();

        // TODO: check: we now force the size of the image, so creating it should not be necessary any more?!
        final BufferedImage asImage;
        if( components != null && components.length > 0 )
        {
          final Integer size = Integer.valueOf( components[0].toString() );
          asImage = externalGraphic.getAsImage( size, size );
        }
        else
        {
          // TODO: check: why 10? if no size is given, probably the original image size should be taken?
          asImage = externalGraphic.getAsImage( 10, 10 );
        }

        if( asImage != null )
        {
          maxWidth = Math.max( maxWidth, asImage.getWidth() );
          maxHeight = Math.max( maxHeight, asImage.getWidth() );
        }
      }

      /* Ignore the marks. */
      // TODO: why?
      // Mark mark = (Mark) o;
    }

    return new Rectangle( maxWidth, maxHeight );
  }
}