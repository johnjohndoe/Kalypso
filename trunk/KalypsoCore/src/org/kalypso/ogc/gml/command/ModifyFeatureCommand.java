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
package org.kalypso.ogc.gml.command;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.model.feature.FeatureFactory;
import org.kalypso.util.command.ICommand;

/**
 * @author belger
 */
public class ModifyFeatureCommand implements ICommand
{
  private final Feature m_feature;
  private final Map m_newMap;
  private final Map m_oldMap = new HashMap();
  private final GMLWorkspace m_workspace;
  

  public ModifyFeatureCommand( final GMLWorkspace workspace, final Feature feature, final Map map )
  {
    m_workspace = workspace;
    m_feature = feature;
    m_newMap = map;
    
    for( Iterator iter = map.keySet().iterator(); iter.hasNext(); )
    {
      final String propName = (String)iter.next();
      m_oldMap.put( propName, feature.getProperty(propName) );
    }
  }

  /**
   * @see org.kalypso.util.command.ICommand#isUndoable()
   */
  public boolean isUndoable()
  {
    return true;
  }

  /**
   * @see org.kalypso.util.command.ICommand#process()
   */
  public void process() throws Exception
  {
    setFeatureProperty( m_newMap );
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    setFeatureProperty( m_newMap );  
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    setFeatureProperty( m_oldMap );  
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Wert ?ndern";
  }

  private void setFeatureProperty( final Map map )
  {
    for( Iterator iter = map.entrySet().iterator(); iter.hasNext(); )
    {
      final Map.Entry entry = (Entry)iter.next();
      
      final FeatureProperty property = FeatureFactory.createFeatureProperty( (String)entry.getKey(), entry.getValue() );
      m_feature.setProperty( property );
    }

    m_workspace.fireModellEvent( null );
  }
}
