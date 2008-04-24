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
package org.kalypso.ui.editor.gmleditor.util.command;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * This command moves an arbitrary element within a list property of a feature.
 * 
 * @author Gernot Belger
 */
public class MoveFeatureCommand implements ICommand
{
  private final Feature m_parentFeature;

  private final Object m_moveItem;

  public static int UP = 0;

  public static int DOWN = 1;

  private final IPropertyType m_pt;

  private final int m_step;

  /**
   * @param prop
   *          Must be a list property.
   * @param moveItem
   *          The element of the list which is to be moved
   * @param step
   *          The amount by which the element is moved (<0 for backwards)
   */
  public MoveFeatureCommand( final Feature parentFeature, final IPropertyType pt, final Object moveItem, final int step )
  {
    m_parentFeature = parentFeature;
    m_pt = pt;
    m_moveItem = moveItem;
    m_step = step;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    return true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process( ) throws Exception
  {
    move( m_step );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    move( m_step );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    move( -m_step );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return "Reihenfolge verändern";
  }

  @SuppressWarnings("unchecked")
  private void move( final int step )
  {
    final List<Object> list = (List<Object>) m_parentFeature.getProperty( m_pt );
    final int currentIndex = list.indexOf( m_moveItem );
    final int newIndex = currentIndex + step;

    Collections.swap( list, currentIndex, newIndex );

    final List<Feature> feList = new ArrayList<Feature>();
    feList.add( m_parentFeature );
    final GMLWorkspace workspace = m_parentFeature.getWorkspace();
    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, m_parentFeature, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_MOVE ) );
  }
}