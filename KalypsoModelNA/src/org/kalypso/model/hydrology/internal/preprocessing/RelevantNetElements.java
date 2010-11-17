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
package org.kalypso.model.hydrology.internal.preprocessing;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.Channel;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Collects the relevant net elements, that needs to be written to ascii files.
 * 
 * @author Gernot Belger
 */
public class RelevantNetElements
{
  private final List<Feature> m_markedChannels = new ArrayList<Feature>();

  private final List<Feature> m_markedCatchments = new ArrayList<Feature>();

  public void addChannel( final Channel channel )
  {
    if( !m_markedChannels.contains( channel ) )
      m_markedChannels.add( channel );
  }

  public void addCatchment( final Catchment channel )
  {
    if( !m_markedCatchments.contains( channel ) )
      m_markedCatchments.add( channel );
  }

  public boolean containsChannel( final Channel channel )
  {
    return m_markedChannels.contains( channel );
  }

  public boolean containsCatchment( final Catchment catchment )
  {
    return m_markedCatchments.contains( catchment );
  }
}