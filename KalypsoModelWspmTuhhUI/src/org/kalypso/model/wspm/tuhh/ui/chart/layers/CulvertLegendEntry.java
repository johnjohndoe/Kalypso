/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.ui.chart.layers;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.ICulvertBuilding;
import org.kalypso.model.wspm.tuhh.ui.internal.gml.WspmBuildingDecorator;

import de.openali.odysseus.chart.framework.model.layer.impl.LegendEntry;

/**
 * @author Gernot Belger
 */
public class CulvertLegendEntry extends LegendEntry
{
  private final ICulvertBuilding m_culvert;

  public CulvertLegendEntry( final CulvertLayer layer, final ICulvertBuilding culvert )
  {
    super( layer, culvert.getTypeLabel() );

    m_culvert = culvert;
  }

  @Override
  public void paintSymbol( final GC gc, final Point size )
  {
    final ImageDescriptor descriptor = WspmBuildingDecorator.getBuildingImage( m_culvert );

    final Image image = descriptor.createImage();

    try
    {
      final Rectangle imageBounds = image.getBounds();

      gc.drawImage( image, 0, 0, imageBounds.width, imageBounds.height, 0, 0, size.x, size.y );
    }
    finally
    {
      image.dispose();
    }
  }
}