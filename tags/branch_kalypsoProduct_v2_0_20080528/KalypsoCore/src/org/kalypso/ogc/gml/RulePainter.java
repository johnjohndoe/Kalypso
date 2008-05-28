/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
  public static void paint( Rule rule, GC gc ) throws Exception
  {
    Feature feature = Symbolizer_Impl.createPseudoFeature();

    LegendGraphic legendGraphic = rule.getLegendGraphic();
    if( legendGraphic != null )
    {
      Graphic graphic = legendGraphic.getGraphic();
      graphic.paint( gc, feature );
    }
    else
    {
      Symbolizer[] symbolizers = rule.getSymbolizers();
      for( Symbolizer symbolizer : symbolizers )
        symbolizer.paint( gc, feature );
    }
  }

  /**
   * This function returns the size an image should have, for showing the complete symbol without scaling. It can only
   * check for legend graphics and point symbolizers.
   * 
   * @param rule
   *            The rule.
   * @return The size an image should have, for showing the complete symbol without scaling. It can only check for
   *         legend graphics and point symbolizers.
   */
  public static Rectangle getSize( Rule rule )
  {
    try
    {
      LegendGraphic legendGraphic = rule.getLegendGraphic();
      if( legendGraphic != null )
      {
        /* Get the legend graphic. */
        Graphic graphic = legendGraphic.getGraphic();

        return getSize( graphic );
      }

      Symbolizer[] symbolizers = rule.getSymbolizers();

      int maxWidth = 0;
      int maxHeight = 0;
      for( Symbolizer symbolizer : symbolizers )
      {
        if( symbolizer instanceof PointSymbolizer )
        {
          PointSymbolizer pointSymbolizer = (PointSymbolizer) symbolizer;
          Graphic graphic = pointSymbolizer.getGraphic();
          if( graphic != null )
          {
            Rectangle size = getSize( graphic );

            if( maxWidth < size.width )
              maxWidth = size.width;

            if( maxHeight < size.height )
              maxHeight = size.height;
          }

          continue;
        }

        // TODO How to find out the size from the other symbolizers?
      }

      return new Rectangle( maxWidth, maxHeight );
    }
    catch( Exception ex )
    {
      ex.printStackTrace();
      return new Rectangle( 0, 0 );
    }
  }

  /**
   * This function returns the original size of the image inside this graphic (not the destination size).
   * 
   * @param graphic
   *            The graphic, which normally contains an image or some marks.
   * @return The original size of the image.
   */
  private static Rectangle getSize( Graphic graphic )
  {
    int maxWidth = 0;
    int maxHeight = 0;

    Object[] marksAndExtGraphics = graphic.getMarksAndExtGraphics();
    for( int i = 0; i < marksAndExtGraphics.length; i++ )
    {
      Object o = marksAndExtGraphics[i];
      if( o instanceof ExternalGraphic )
      {
        ExternalGraphic externalGraphic = (ExternalGraphic) o;
        BufferedImage asImage = externalGraphic.getAsImage();

        if( asImage == null )
          continue;

        int width = asImage.getWidth();
        int height = asImage.getWidth();

        if( maxWidth < width )
          maxWidth = width;

        if( maxHeight < height )
          maxHeight = height;

        continue;
      }

      /* Ignore the marks. */
      // Mark mark = (Mark) o;
    }

    return new Rectangle( maxWidth, maxHeight );
  }
}