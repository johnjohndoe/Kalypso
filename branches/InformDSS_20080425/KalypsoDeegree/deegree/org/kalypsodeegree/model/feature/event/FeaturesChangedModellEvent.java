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
package org.kalypsodeegree.model.feature.event;

import java.util.Arrays;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author doemming
 */
public class FeaturesChangedModellEvent extends ModellEvent implements IGMLWorkspaceModellEvent
{
  private final Feature[] m_features;

  private final GMLWorkspace m_workspace;

  public FeaturesChangedModellEvent( final GMLWorkspace workspace, final Feature[] features )
  {
    super( workspace, FEATURE_CHANGE );
    m_workspace = workspace;
    m_features = features;
  }

  public Feature[] getFeatures( )
  {
    return m_features;
  }

  public GMLWorkspace getGMLWorkspace( )
  {
    return m_workspace;
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEvent#toString()
   */
  @Override
  public String toString( )
  {
    StringBuffer buf = new StringBuffer( 128 );
    buf.append( "FeatureChangedModellEvent[" );
    if( m_features != null )
    {
      buf.append( Arrays.asList( m_features ) );
    }
    else
    {
      buf.append( "null" );
    }
    buf.append( ']' );
    return buf.toString();
  }
}
