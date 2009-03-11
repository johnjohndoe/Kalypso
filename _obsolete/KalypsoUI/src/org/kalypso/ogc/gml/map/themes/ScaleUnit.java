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
package org.kalypso.ogc.gml.map.themes;

/**
 * This class represents a storage for unit name and its factor.
 * 
 * @author Holger Albert
 */
public class ScaleUnit
{
  /**
   * The name of the unit (e.g. km).
   */
  private String m_name;

  /**
   * The factor of the unit (e.g. 1000).
   */
  private double m_factor;

  /**
   * The constructor.
   * 
   * @param name
   *            The name of the unit (e.g. km).
   * @param factor
   *            The factor of the unit (e.g. 1000).
   */
  public ScaleUnit( String name, double d )
  {
    m_name = name;
    m_factor = d;
  }

  /**
   * This function returns the name of the unit (e.g. km).
   * 
   * @return The name of the unit (e.g. km).
   */
  public String getName( )
  {
    return m_name;
  }

  /**
   * This function returns the factor of the unit (e.g. 1000).
   * 
   * @return The factor of the unit (e.g. 1000).
   */
  public double getFactor( )
  {
    return m_factor;
  }
}