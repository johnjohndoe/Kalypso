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

import java.util.Iterator;
import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureVisitor;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventProvider;
import org.deegree_impl.model.feature.visitors.GetSelectionVisitor;
import org.deegree_impl.model.feature.visitors.UnselectFeatureVisitor;
import org.kalypso.util.command.ICommand;

/**
 * @author doemming
 */
public class SingleSelectCommand implements ICommand
{
  private final Feature m_feature;

  private final int mySelectionId;

  private final ModellEventProvider m_modellEventProvider;

  private final GMLWorkspace m_workspace;

  private final FeatureVisitor m_unselectVisitor;

  private List m_selectedFeatures;

  public SingleSelectCommand( final GMLWorkspace workspace, final Feature feature, int selectionId,
      ModellEventProvider eventProvider )
  {
    m_workspace = workspace;
    m_feature = feature;
    mySelectionId = selectionId;

    m_modellEventProvider = eventProvider;
    m_unselectVisitor = new UnselectFeatureVisitor( selectionId );
    
    m_selectedFeatures = GetSelectionVisitor.getSelectedFeatures( workspace, selectionId );
  }

  public boolean isUndoable()
  {
    return true;
  }

  public void process() throws Exception
  {
    redo();
  }

  public void redo() throws Exception
  {
    m_workspace.accept( m_unselectVisitor, m_workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );
    m_feature.select( mySelectionId );

    m_modellEventProvider.fireModellEvent( new ModellEvent( m_modellEventProvider, ModellEvent.SELECTION_CHANGED ) );
  }

  public void undo() throws Exception
  {
    m_feature.unselect( mySelectionId );
    for( final Iterator iter = m_selectedFeatures.iterator(); iter.hasNext(); )
      ((Feature)iter.next()).select( mySelectionId );
    
    m_modellEventProvider.fireModellEvent( new ModellEvent( m_modellEventProvider, ModellEvent.SELECTION_CHANGED ) );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Objekt selektieren";
  }
}