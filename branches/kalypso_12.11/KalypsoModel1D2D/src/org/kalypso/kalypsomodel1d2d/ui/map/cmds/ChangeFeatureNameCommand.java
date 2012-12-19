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
package org.kalypso.kalypsomodel1d2d.ui.map.cmds;

import org.eclipse.core.runtime.Assert;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * Undoable command that changes the name of of an {@link Feature}
 * 
 * @author Patrice Congo
 */
public class ChangeFeatureNameCommand implements IFeatureChangeCommand
{
  private String m_oldName;

  private final Feature m_featureToRename;

  private final String m_newName;

  /**
   * Sets to true to indicate process done
   */
  private boolean changed = false;

  /**
   * Creates a Calculation unit of the given q-name
   * 
   * @param cuFeatureQName
   *          the q-name of the calculation unit to create
   * @param model1d2d
   *          the model that should hold the new calculation unit
   * @param name
   *          a name for the calculation unit if one has to be set or null
   * @param description
   *          text describing the calculation unit or null
   * @throws IllegalArgumentException
   *           if cuFeatureQName or model1d2d is null
   */
  public ChangeFeatureNameCommand( final Feature featureToRename, final String newName )
  {
    Assert.isNotNull( featureToRename );
    Assert.isNotNull( newName );

    m_featureToRename = featureToRename;
    m_newName = newName;
  }

  @Override
  public Feature[] getChangedFeatures( )
  {
    if( changed )
    {
      return new Feature[] { m_featureToRename };
    }
    else
    {
      return new Feature[] {};
    }
  }

  @Override
  public String getDescription( )
  {
    return "Change Name"; //$NON-NLS-1$
  }

  @Override
  public boolean isUndoable( )
  {
    return true;
  }

  @Override
  public void process( ) throws Exception
  {
    try
    {
      changed = true;
      m_oldName = m_featureToRename.getName();
      m_featureToRename.setName( m_newName );
      fireProcessChanges();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw e;
    }
  }

  /**
   * @param calculationUnit
   *          the added or removed calculation unit
   * @param added
   *          true if the calculation unit was added false otherwise
   */
  private final void fireProcessChanges( )
  {
    final int changedType = FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE;

    final GMLWorkspace workspace = m_featureToRename.getWorkspace();
    final FeatureStructureChangeModellEvent event = new FeatureStructureChangeModellEvent( workspace, m_featureToRename.getOwner(), new Feature[] { m_featureToRename }, changedType );
    workspace.fireModellEvent( event );
  }

  @Override
  public void redo( ) throws Exception
  {
    if( !changed )
    {
      process();
    }
  }

  @Override
  public void undo( ) throws Exception
  {
    m_featureToRename.setDescription( m_oldName );
    fireProcessChanges();
  }
}