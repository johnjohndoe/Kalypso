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
package org.kalypso.kalypsomodel1d2d.schema.binding.results;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Thomas Jung
 * 
 */
public class ElementResult
{
  final private int m_elemID;

  private int m_currentRougthnessClassID;

  private int m_previousRoughnessClassID;

  private int m_eleminationNumber;

  private final HashMap<Integer, ArcResult> m_arcResult = new HashMap<Integer, ArcResult>();

  private final List<NodeResult> m_cornerNodes = new LinkedList<NodeResult>();

  private final List<NodeResult> m_midsideNodes = new LinkedList<NodeResult>();
  
  private INodeResult m_centerNode = null;

  public ElementResult( int id, int currentRougthnessClass, int previousRoughnessClass, int eleminationNum )
  {
    m_elemID = id;
    m_currentRougthnessClassID = currentRougthnessClass;
    m_previousRoughnessClassID = previousRoughnessClass;
    m_eleminationNumber = eleminationNum;
  }

  public void setNewArc( ArcResult arcresult )
  {
    m_arcResult.put( m_arcResult.size(), arcresult );
  }

  public ArcResult getArc( int id )
  {
    return m_arcResult.get( id );
  }

  public int getNumArcs( )
  {
    return m_arcResult.size();
  }

  public int getCurrentRougthnessClassID( )
  {
    return m_currentRougthnessClassID;
  }

  public void setCurrentRougthnessClassID( int currentRougthnessClassID )
  {
    m_currentRougthnessClassID = currentRougthnessClassID;
  }

  public int getPreviousRoughnessClassID( )
  {
    return m_previousRoughnessClassID;
  }

  public void setPreviousRoughnessClassID( int previousRoughnessClassID )
  {
    m_previousRoughnessClassID = previousRoughnessClassID;
  }

  public int getEleminationNumber( )
  {
    return m_eleminationNumber;
  }

  public void setEleminationNumber( int eleminationNumber )
  {
    m_eleminationNumber = eleminationNumber;
  }

  public int getElemID( )
  {
    return m_elemID;
  }

  public INodeResult getCornerNodes( int index )
  {
    return m_cornerNodes.get( index );
  }

  public void setCornerNodes( NodeResult nodeResult )
  {

    m_cornerNodes.add( nodeResult );
  }

  public INodeResult getMidsideNodes( int index )
  {
    return m_midsideNodes.get( index );
  }

  public void setMidsideNodes( NodeResult nodeResult )
  {
    m_midsideNodes.add( nodeResult );
  }

  public int getNumCornerNodes( )
  {
    return m_cornerNodes.size();
  }

  public int getNumMidsideNodes( )
  {
    return m_midsideNodes.size();
  }

  public INodeResult createCenterNode( )
  {
    interpolateCenterNode();
    
    return m_centerNode;
  }
  /**
   * creates a center node for the element by using the corner and midside nodes. 
   */
  private void interpolateCenterNode( )
  {
    /* get the center point location by using the corner nodes */
    for( int i = 0; i < m_cornerNodes.size(); i++ )
    {
      GM_Point p = m_cornerNodes.get( i ).getPoint();
    }
    

    /* interpolate the data */
    
  }

}
