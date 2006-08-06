/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ui.editor.gmleditor.ui;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Event;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypsodeegree.model.feature.Feature;

/**
 * TODO: insert type comment here
 * 
 * @author kuepfer
 */
public class NewFeatureAction extends Action
{
  private final IRelationType m_fatp;

  private final CommandableWorkspace m_workspace;

  private final Feature m_parentFeature;

  private final IFeatureType m_targetType;

  private final IFeatureSelectionManager m_selectionManager;

  public NewFeatureAction( final String text, final ImageDescriptor image, final CommandableWorkspace workspace, final Feature parentFeature, final IRelationType fatp, final IFeatureType targetType, final IFeatureSelectionManager selectionManager )
  {
    super( text, image );

    m_workspace = workspace;
    m_parentFeature = parentFeature;
    m_fatp = fatp;
    m_targetType = targetType;
    m_selectionManager = selectionManager;
  }

  /**
   * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
   */
  @Override
  public void runWithEvent( final Event event )
  {
    try
    {
      final ICommand command = new AddFeatureCommand( m_workspace, m_targetType, m_parentFeature, m_fatp, 0, null, m_selectionManager );
      m_workspace.postCommand( command );
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      ErrorDialog.openError( event.widget.getDisplay().getActiveShell(), getText(), "Fehler beim Hinzufügen", status );
    }

  }

}
