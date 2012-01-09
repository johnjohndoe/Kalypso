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
package org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans;

import org.apache.commons.lang3.ArrayUtils;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.KnaufReach;

/**
 * @author Dirk Kuch
 */
public class KnaufSA14Bean extends AbstractKnaufProjectBean
{
  private int m_nhyd;

  private final KnaufReach m_reach;

  private Integer m_numberOfProfiles;

  public KnaufSA14Bean( final KnaufReach reach )
  {
    m_reach = reach;

    // FIXME for later processing values should be initialized from the outside
    m_nhyd = m_reach.getFliessgesetz().toInt();
    m_numberOfProfiles = ArrayUtils.getLength( m_reach.getProfiles() );
  }

  @Override
  public Integer getSatzart( )
  {
    return 14;
  }

  public int getNHyd( )
  {
    return m_nhyd;
  }

  public void setNHyd( final int nhyd )
  {
    m_nhyd = nhyd;
  }

  public Integer getNumberOfProfiles( )
  {
    return m_numberOfProfiles;
  }

  public void setNumberOfProfiles( final Integer profiles )
  {
    m_numberOfProfiles = profiles;
  }

}
