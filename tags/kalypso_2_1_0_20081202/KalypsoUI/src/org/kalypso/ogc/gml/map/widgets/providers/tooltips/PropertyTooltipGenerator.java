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

import javax.xml.namespace.QName;

import org.kalypsodeegree.model.feature.Feature;

/**
 * This class generates a tooltip with a given feature and a property QName.
 * 
 * @author Holger Albert
 */
public class PropertyTooltipGenerator implements ITooltipGenerator
{
  /**
   * The QName of the property, which should be used for generating the tooltip.
   */
  private final QName m_property;

  /**
   * The constructor.
   */
  public PropertyTooltipGenerator( QName property )
  {
    m_property = property;
  }

  /**
   * @see org.kalypso.informdss.manager.util.widgets.providers.ITooltipGenerator#generate()
   */
  public String generate( Object object )
  {
    /* We know, that it must be a feature, we get here. */
    Feature feature = (Feature) object;

    Object property = feature.getProperty( m_property );

    return property.toString();
  }
}
