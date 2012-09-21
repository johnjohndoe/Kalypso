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
public class WehrProfileObjectData extends AbstractProfileObjectData
{
  private final String m_wehrArt;

  private final double m_formBeiwert;

  public WehrProfileObjectData( final String wehrArt, final double formBeiwert )
  {
    m_wehrArt = wehrArt;
    m_formBeiwert = formBeiwert;
  }

  public String getWehrArt( )
  {
    return m_wehrArt;
  }

  public double getFormBeiwert( )
  {
    return m_formBeiwert;
  }
}