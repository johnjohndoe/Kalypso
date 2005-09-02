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

import org.kalypso.commons.command.ICommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.selection.IFeatureSelectionManager;

/**
 * Setzt die selektierten Features innerhalb eines Workspace
 * 
 * @author belger TODO rename to highlight...something
 */
public class SelectFeaturesCommand implements ICommand
{
  private final Feature[] m_selection;

  private final IFeatureSelectionManager m_selectionManager;

  private Feature[] m_selectedFeaturesOriginal;

  private Object m_eventSource;

  public SelectFeaturesCommand( final CommandableWorkspace workspace, final Feature selection,
      final IFeatureSelectionManager selectionManager, final Object eventSource )
  {
    this( workspace, new Feature[]
    { selection }, selectionManager, eventSource );
  }

  public SelectFeaturesCommand( final CommandableWorkspace workspace, final Feature[] selection,
      final IFeatureSelectionManager selectionManager, final Object eventSource )
  {
    m_selection = selection;
    m_eventSource = eventSource;
    if( selectionManager != null )
      m_selectionManager = selectionManager;
    else
      m_selectionManager = workspace.getSelectionManager();
    
    m_selectedFeaturesOriginal = m_selectionManager.getFeatureSelection();
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable()
  {
    return true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process() throws Exception
  {
    m_selectionManager.setSelection( m_eventSource, m_selection );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    process();
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    m_selectionManager.setSelection( m_eventSource, m_selectedFeaturesOriginal );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Objekte selektieren";
  }

}