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

import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * class RemoveRelationCommand Command to remove a normal relation created by
 * 
 * @author doemming (13.05.2005)
 */
public class RemoveRelationCommand implements ICommand
{
  private final Feature m_srcFE;

  private final Feature m_destFE;

  private final IRelationType m_linkPropName;

  private final boolean m_isComposition;

  private final int m_pos;

  public RemoveRelationCommand( final Feature srcFE, final IRelationType linkPropName, final Feature destFE )
  {
    m_srcFE = srcFE;
    m_linkPropName = linkPropName;
    m_destFE = destFE;
    // is composition
    m_isComposition = FeatureHelper.isCompositionLink( srcFE, linkPropName, destFE );
    m_pos = FeatureHelper.getPositionOfAssoziation( srcFE, linkPropName, destFE );
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
    final GMLWorkspace workspace = m_srcFE.getWorkspace();

    if( m_isComposition )
      workspace.removeLinkedAsCompositionFeature( m_srcFE, m_linkPropName, m_destFE );
    else
      workspace.removeLinkedAsAggregationFeature( m_srcFE, m_linkPropName, m_destFE.getId() );

    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, m_srcFE, m_destFE, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE ) );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    process();
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    final GMLWorkspace workspace = m_srcFE.getWorkspace();

    if( m_isComposition )
      workspace.addFeatureAsComposition( m_srcFE, m_linkPropName, m_pos, m_destFE );
    else
      workspace.addFeatureAsAggregation( m_srcFE, m_linkPropName, m_pos, m_destFE.getId() );

    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, m_srcFE, m_destFE, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return "Relation aufheben";
  }
}