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
package org.kalypso.model.wspm.tuhh.core.profile.objects;

import org.kalypso.model.wspm.core.profil.objects.AbstractProfileObjectData;

/**
 * @author Holger Albert
 */
public class KreisProfileObjectData extends AbstractProfileObjectData
{
  private final double m_bezugspunktX;

  private final double m_bezugspunktY;

  private final double m_breite;

  private final double m_sohlGefaelle;

  private final double m_rauhheit;

  public KreisProfileObjectData( final double bezugspunktX, final double bezugspunktY, final double breite, final double sohlGefaelle, final double rauhheit )
  {
    m_bezugspunktX = bezugspunktX;
    m_bezugspunktY = bezugspunktY;
    m_breite = breite;
    m_sohlGefaelle = sohlGefaelle;
    m_rauhheit = rauhheit;
  }

  public double getBezugspunktX( )
  {
    return m_bezugspunktX;
  }

  public double getBezugspunktY( )
  {
    return m_bezugspunktY;
  }

  public double getBreite( )
  {
    return m_breite;
  }

  public double getSohlGefaelle( )
  {
    return m_sohlGefaelle;
  }

  public double getRauhheit( )
  {
    return m_rauhheit;
  }
}