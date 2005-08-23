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

package org.kalypso.ui.editor.gmleditor.util.command;

import java.util.List;

import org.kalypso.commons.command.ICommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/*
 * class AddHeavyRelationshipCommand
 * 
 * created by @author doemming (19.04.2005)
 */
public class AddHeavyRelationshipCommand implements ICommand
{
  private final Feature m_srcFE;

  private final Feature m_targetFE;

  private final GMLWorkspace m_workspace;

  private final FeatureAssociationTypeProperty m_linkFT1;

  private final FeatureAssociationTypeProperty m_linkFT2;

  private Feature m_newFeature;

  private final FeatureType m_bodyFT;

  /**
   *  
   */
  public AddHeavyRelationshipCommand( GMLWorkspace workspace, Feature srcFE, FeatureAssociationTypeProperty linkFT1,
      FeatureType bodyFT,FeatureAssociationTypeProperty linkFT2, Feature targetFE )
  {
    m_workspace = workspace;
    m_srcFE = srcFE;
    m_linkFT1 = linkFT1;
    m_bodyFT = bodyFT;
    m_linkFT2 = linkFT2;
    m_targetFE = targetFE;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable()
  {
    return true;
  }

  /**
   * 
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process() throws Exception
  {
    // create relation feature
    m_newFeature = m_workspace.createFeature( m_bodyFT );
    // create first link
    final String linkName1 = m_linkFT1.getName();
    m_workspace.addFeatureAsComposition( m_srcFE, linkName1, 0, m_newFeature );
    // create second link
    m_workspace.addFeatureAsAggregation( m_newFeature, m_linkFT2.getName(), 0, m_targetFE.getId() );
    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_srcFE,
        FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
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
    String linkName2 = m_linkFT2.getName();
    // remove second link
    final int max = m_newFeature.getFeatureType().getMaxOccurs( linkName2 );
    switch( max )
    {
    case 1:
      m_newFeature.setProperty( FeatureFactory.createFeatureProperty( linkName2, null ) );
      break;
    default:
      ( (List)m_newFeature.getProperty( linkName2 ) ).remove( m_targetFE.getId() );
      break;
    }
    // remove relation feature and also first link
    m_workspace.removeLinkedAsCompositionFeature( m_srcFE, m_linkFT1.getName(), m_newFeature );
    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_srcFE,
        FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE ) );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Relation erzeugen";
  }
}
