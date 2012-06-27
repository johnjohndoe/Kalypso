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

package org.kalypso.ui.rrm.internal.map.editRelation;

import java.util.List;

import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;

/**
 * @author doemming
 */
public class AddHeavyRelationshipCommand implements ICommand
{
  private final Feature m_srcFE;

  private final Feature m_targetFE;

  private final GMLWorkspace m_workspace;

  private final IRelationType m_linkFT1;

  private final IRelationType m_linkFT2;

  private Feature m_bodyFeature;

  private final IFeatureType m_bodyFT;

  public AddHeavyRelationshipCommand( final GMLWorkspace workspace, final Feature srcFE, final IRelationType linkFT1, final IFeatureType bodyFT, final IRelationType linkFT2, final Feature targetFE )
  {
    m_workspace = workspace;
    m_srcFE = srcFE;
    m_linkFT1 = linkFT1;
    m_bodyFT = bodyFT;
    m_linkFT2 = linkFT2;
    m_targetFE = targetFE;
  }

  @Override
  public boolean isUndoable( )
  {
    return true;
  }

  @Override
  public void process( ) throws Exception
  {
    if( m_linkFT1.isList() )
    {
      final IFeatureBindingCollection<Feature> memberList = m_srcFE.getMemberList( m_linkFT1 );
      m_bodyFeature = memberList.addNew( m_bodyFT.getQName() );
    }
    else
      m_bodyFeature = m_srcFE.createSubFeature( m_linkFT1, m_bodyFT.getQName() );

    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_srcFE, m_bodyFeature, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

    // create link from body to second feature
    // REAMRK: does only work for inline links for now
    m_bodyFeature.setLink( m_linkFT2.getQName(), "#" + m_targetFE.getId() ); //$NON-NLS-1$

    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_bodyFeature, m_targetFE, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

    // HACK: Normally, the first two event shod be enough; however the virtual-relation geometries don't get updated
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
    // remove second link

    if( m_linkFT2.isList() )
    {
      ((List< ? >) m_bodyFeature.getProperty( m_linkFT2 )).remove( m_targetFE.getId() );
      m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_bodyFeature, m_targetFE, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE ) );
    }
    else
    {
      m_bodyFeature.setProperty( m_linkFT2, null );
      m_workspace.fireModellEvent( new FeaturesChangedModellEvent( m_workspace, new Feature[] { m_bodyFeature } ) );
    }

    // remove relation feature and also first link
    m_srcFE.removeMember( m_linkFT1, m_bodyFeature );
    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_srcFE, m_bodyFeature, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE ) );

    // HACK: Normally, the first two events should be enough; however the virtual-relation geometries don't get updated
    // this way. In order to enforce a map redrawal, the next event is fired additionally.
    // TODO: This should (somehow) be done automatically be the feature framework...
    m_workspace.fireModellEvent( new FeaturesChangedModellEvent( m_workspace, new Feature[] { m_srcFE } ) );
  }

  @Override
  public String getDescription( )
  {
    return "Create relation"; //$NON-NLS-1$
  }
}