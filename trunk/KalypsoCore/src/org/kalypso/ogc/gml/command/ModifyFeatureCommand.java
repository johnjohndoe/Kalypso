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

import org.kalypso.util.command.ICommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author belger
 */
public class ModifyFeatureCommand implements ICommand
{
  private final Feature[] m_features;

  private final Map m_newMap;

  private final Map[] m_oldMap;

  private final GMLWorkspace m_workspace;

  /**
   * @param workspace
   * @param features
   *          features to modify
   * @param map
   *          propertyname/value map of properties to modify
   */
  public ModifyFeatureCommand( final GMLWorkspace workspace, final Feature features[], final Map map )
  {
    m_workspace = workspace;
    m_features = features;
    m_newMap = map;
    m_oldMap = new HashMap[features.length];
    for( int i = 0; i < features.length; i++ )
    {
      Feature feature = features[i];
      m_oldMap[i] = new HashMap();
      for( Iterator iter = map.keySet().iterator(); iter.hasNext(); )
      {
        final String propName = (String)iter.next();
        m_oldMap[i].put( propName, feature.getProperty( propName ) );
      }
    }
  }

  public ModifyFeatureCommand( final GMLWorkspace workspace, final Feature feature, final Map map )
  {
    this( workspace, new Feature[]
    { feature }, map );
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
    for( Iterator iter = m_newMap.entrySet().iterator(); iter.hasNext(); )
    {
      final Map.Entry entry = (Entry)iter.next();
      final String propName = (String)entry.getKey();
      final String propType = m_features[0].getFeatureType().getProperty( propName ).getType();
      for( int i = 0; i < m_features.length; i++ )
      {
        try
        {
          final Object value = FeatureHelper.cloneData( entry.getValue(), propType );
          final FeatureProperty property;
          property = FeatureFactory.createFeatureProperty( propName, value );
          m_features[i].setProperty( property );
        }
        catch( Exception e )
        {
          // ignore exception and copy next features property
        }
      }
    }
    FeatureList list = FeatureFactory.createFeatureList( null, null, m_features );
    m_workspace.fireModellEvent( new FeaturesChangedModellEvent( m_workspace, list ) );
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    process();
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    for( int i = 0; i < m_features.length; i++ )
    {
      for( Iterator iter = m_oldMap[i].entrySet().iterator(); iter.hasNext(); )
      {
        final Map.Entry entry = (Entry)iter.next();
        final FeatureProperty property = FeatureFactory
            .createFeatureProperty( (String)entry.getKey(), entry.getValue() );
        m_features[i].setProperty( property );
      }
    }
    FeatureList list = FeatureFactory.createFeatureList( null, null, m_features );
    m_workspace.fireModellEvent( new FeaturesChangedModellEvent( m_workspace, list ) );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Wert ändern";
  }
}
