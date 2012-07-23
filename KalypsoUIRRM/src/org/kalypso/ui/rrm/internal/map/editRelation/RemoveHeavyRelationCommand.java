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
package org.kalypso.ui.rrm.internal.map.editRelation;

import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;

/**
 * class RemoveHeavyRelationCommand Command to remove a normal relation created by
 *
 * @author doemming (13.05.2005)
 */
public class RemoveHeavyRelationCommand implements ICommand
{
  private final GMLWorkspace m_workspace;

  private final Feature m_srcFE;

  private final Feature m_bodyFE;

  private final Feature m_destFE;

  private final IRelationType m_linkName1;

  private final IRelationType m_linkName2;

  private final int m_pos;

  public RemoveHeavyRelationCommand( final GMLWorkspace workspace, final Feature srcFE, final IRelationType linkName1, final Feature bodyFE, final IRelationType linkName2, final Feature destFE )
  {
    m_workspace = workspace;
    m_srcFE = srcFE;
    m_bodyFE = bodyFE;
    m_destFE = destFE;
    m_linkName1 = linkName1;
    m_linkName2 = linkName2;

    m_pos = indexOfMember( srcFE, linkName1, bodyFE );
  }

  private int indexOfMember( final Feature srcFE, final IRelationType linkRelation, final Feature inlineElement )
  {
    if( !linkRelation.isList() )
      return -1;

    final IFeatureBindingCollection<Feature> memberList = srcFE.getMemberList( linkRelation );
    return memberList.indexOf( inlineElement );
  }

  @Override
  public boolean isUndoable( )
  {
    return true;
  }

  @Override
  public void process( ) throws Exception
  {
    // first remove 2. normal relation
    m_bodyFE.setProperty( m_linkName2, null );
    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_bodyFE, m_destFE, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE ) );

    // then remove 1. normal relation
    m_srcFE.removeMember( m_linkName1, m_bodyFE );

    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_srcFE, m_bodyFE, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE ) );

    // HACK: Normally, the first two events should be enough; however the virtual-relation geometries don't get updated
    // this way.
    // In order to enforce a map redrawal, the next event is fired additionally.
    // TODO: This should (somehow) be done automatically be the feature framework...
    m_workspace.fireModellEvent( new FeaturesChangedModellEvent( m_workspace, new Feature[] { m_srcFE } ) );
  }

  @Override
  public void redo( ) throws Exception
  {
    process();
  }

  @Override
  public void undo( ) throws Exception
  {
    if( m_linkName1.isList() )
    {
      final IFeatureBindingCollection<Feature> memberList = m_srcFE.getMemberList( m_linkName1 );
      memberList.add( m_pos, m_bodyFE );
    }
    else
      m_srcFE.setProperty( m_linkName1, m_bodyFE );

    m_bodyFE.setLink( m_linkName2, "#" + m_destFE.getId() ); //$NON-NLS-1$

    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_bodyFE, m_destFE, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_srcFE, m_bodyFE, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

    // HACK: Normally, the first two events should be enough; however the virtual-relation geometries don't get updated
    // this way.
    // In order to enforce a map redrawal, the next event is fired additionally.
    // TODO: This should (somehow) be done automatically be the feature framework...
    m_workspace.fireModellEvent( new FeaturesChangedModellEvent( m_workspace, new Feature[] { m_srcFE } ) );
  }

  @Override
  public String getDescription( )
  {
    return "Remove link"; //$NON-NLS-1$
  }
}