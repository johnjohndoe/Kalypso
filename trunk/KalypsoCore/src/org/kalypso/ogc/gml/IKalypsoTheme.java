/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml;

import java.awt.Graphics;

import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.feature.event.ModellEventProvider;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager;

/**
 * @author Katharina <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 *  
 */
public interface IKalypsoTheme extends ModellEventProvider, ModellEventListener
{
  public void dispose();

  public void paintSelected( final Graphics g, final GeoTransform p, final double scale, final GM_Envelope bbox );

  public void paintUnSelected( final Graphics g, final GeoTransform p, final double scale, final GM_Envelope bbox );

  /**
   * returns the name of the layer
   */
  public String getName();

  public String getType();

  public void setName( final String name );

  public GM_Envelope getBoundingBox();

  public void paintSelected( Graphics gr, Graphics hg, GeoTransform p, double scale, GM_Envelope bbox );

  public IFeatureSelectionManager getSelectionManager();
}