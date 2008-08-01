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
package org.kalypso.ogc.gml.map.widgets.providers.tooltips;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.geom.Rectangle2D;

import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.widgets.mapfunctions.MapfunctionHelper;
import org.kalypso.ogc.gml.map.widgets.providers.IFeaturesProvider;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypsodeegree.model.feature.Feature;

/**
 * This class should create a tooltip for the feature, the mouse hovers over.
 * 
 * @author Holger Albert
 */
public class PropertyTooltipProvider implements ITooltipProvider
{
  /**
   * The default radius for selecting features.
   */
  public static final int DEFAULT_RADIUS = 20;

  /**
   * Features within this radius are selected.
   */
  private final int m_radius;

  /**
   * Generates the tooltip.
   */
  private final ITooltipGenerator m_generator;

  /**
   * This class provides the features. It could do it on several ways (e.g. via QName or if it has geometrys).
   */
  private final IFeaturesProvider m_featuresProvider;

  /**
   * The constructor.
   * 
   * @param radius
   *          The radius, in which the feature could be found.
   * @param generator
   *          A generator class, which provides a tooltip generated out of a object.
   * @param featuresProvider
   *          Provides the features, from which the tooltips can be build, if the mouse is hovering one.
   */
  public PropertyTooltipProvider( int radius, ITooltipGenerator generator, IFeaturesProvider featuresProvider )
  {
    m_radius = radius;
    m_generator = generator;
    m_featuresProvider = featuresProvider;
  }

  /**
   * @see org.kalypso.informdss.manager.util.widgets.providers.tooltips.ITooltipProvider#getTooltip(org.kalypso.ogc.gml.map.MapPanel,
   *      org.eclipse.swt.graphics.Rectangle)
   */
  public String getTooltip( MapPanel mapPanel, Rectangle rectangle )
  {
    final EasyFeatureWrapper[] wrappers = m_featuresProvider.getFeatures( mapPanel );
    final EasyFeatureWrapper[] wrappersToSelect = MapfunctionHelper.findFeatureToSelect( mapPanel, rectangle, wrappers, m_radius );
    if( wrappersToSelect == null || wrappersToSelect.length == 0 )
      return ""; //$NON-NLS-1$

    final Feature selectedFeature = wrappersToSelect[0].getFeature();
    return m_generator.generate( selectedFeature );
  }

  /**
   * @see org.kalypso.informdss.manager.util.widgets.providers.tooltips.ITooltipProvider#paintTooltip(java.awt.Graphics,
   *      java.lang.String)
   */
  public void paintTooltip( Graphics g, Point p, String tooltip )
  {
    Rectangle2D rectangle = g.getFontMetrics().getStringBounds( tooltip, g );

    g.setColor( new Color( 255, 255, 225 ) );
    g.fillRect( (int) p.getX(), (int) p.getY() + 20, (int) rectangle.getWidth() + 10, (int) rectangle.getHeight() + 5 );

    g.setColor( Color.BLACK );
    g.drawRect( (int) p.getX(), (int) p.getY() + 20, (int) rectangle.getWidth() + 10, (int) rectangle.getHeight() + 5 );

    /* Tooltip zeichnen. */
    g.drawString( tooltip, (int) p.getX() + 5, (int) p.getY() + 35 );
  }
}
