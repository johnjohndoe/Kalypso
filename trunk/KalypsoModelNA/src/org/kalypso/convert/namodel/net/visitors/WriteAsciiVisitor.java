package org.kalypso.convert.namodel.net.visitors;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.convert.namodel.manager.AsciiBuffer;
import org.kalypso.convert.namodel.net.NetElement;
import org.kalypsodeegree.model.feature.Feature;

/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

public class WriteAsciiVisitor extends NetElementVisitor
{
  private final AsciiBuffer m_asciiBuffer;

  private final List<Feature> m_nodeCollector;

  private final List<NetElement> m_visitedElements = new ArrayList<NetElement>();

  /*
   * @author doemming
   */
  public WriteAsciiVisitor( final AsciiBuffer asciiBuffer )
  {
    m_asciiBuffer = asciiBuffer;
    m_nodeCollector = new ArrayList<Feature>();
  }

  /**
   * @see org.kalypso.convert.namodel.net.visitors.NetElementVisitor#visit(org.kalypso.convert.namodel.net.NetElement)
   */
  @Override
  public boolean visit( final NetElement netElement )
  {
    netElement.write( m_asciiBuffer, m_nodeCollector );
    try
    {
      netElement.generateTimeSeries();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      log( e.getLocalizedMessage() );
    }
    m_visitedElements.add( netElement );
    return true;
  }

  public List<NetElement> getVisitedElements( )
  {
    return m_visitedElements;
  }

  public List<Feature> getNodeCollector( )
  {
    return m_nodeCollector;
  }
}