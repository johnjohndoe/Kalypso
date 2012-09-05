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
package org.kalypso.kalypsomodel1d2d.services;

import org.kalypso.commons.command.ICommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * Command used for assigning roughnesses to elements.<br/>
 * Overrides "applyChanges" method of its superclass (ChangeFeaturesCommand) to send RoughnessAssignServiceModellEvent
 * instead of FeatureChangeModellEvent. This is needed for RoughnessAssignListener to diferentiate between external and
 * own changes.
 *
 * @author Dejan Antanaskovic
 */
public class RoughnessAssignCommand implements ICommand
{
  private final GMLWorkspace m_workspace;

  private final FeatureChange[] m_changes;

  public RoughnessAssignCommand( final GMLWorkspace workspace, final FeatureChange[] changes )
  {
    m_workspace = workspace;
    m_changes = changes;
  }

  @Override
  public boolean isUndoable( )
  {
    return false;
  }

  @Override
  public void process( ) throws Exception
  {
    for( final FeatureChange change : m_changes )
    {
      final Feature feature = change.getFeature();
      feature.setProperty( change.getProperty(), change.getNewValue() );
    }

    if( m_workspace != null )
      m_workspace.fireModellEvent( new RoughnessAssignServiceModellEvent( m_workspace, m_changes ) );
  }

  @Override
  public void redo( ) throws Exception
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public void undo( ) throws Exception
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public String getDescription( )
  {
    return "Assign Rougness"; //$NON-NLS-1$
  }
}