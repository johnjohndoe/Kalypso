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
package org.kalypso.observation.phenomenon;


/**
 * An {@link IPhenomenon} implementation based on a dictionary entry.
 * 
 * @author dirk Kuch
 */
public class DictionaryPhenomenon implements IPhenomenon
{
  private final String m_urn;

  private final String m_name;

  private final String m_description;

  public DictionaryPhenomenon( final String urn, final String name, final String description )
  {
    m_urn = urn;
    m_name = name;
    m_description = description;
  }

  /**
   * @see org.kalypso.observation.IPhenomenon#getDescription()
   */
  public String getDescription( )
  {
    return m_description;
  }

  /**
   * @see org.kalypso.observation.IPhenomenon#getID()
   */
  public String getID( )
  {
    throw new UnsupportedOperationException( "TODO Load dict-feature and retrieve data from it" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.observation.IPhenomenon#getName()
   */
  public String getName( )
  {
    return m_name;
  }

  public String getDictionaryUrn( )
  {
    return m_urn;
  }

}
