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
package org.kalypso.ui.editor.gmleditor.util.actions;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;

public final class AddFeatureAction extends Action
{
  private String m_propertyName;

  private int pos = 0;

  private CommandableWorkspace m_workspace;

  private FeatureType m_type;

  private Feature m_parentFeature;

  private Shell m_shell;

  public AddFeatureAction( FeatureType type, CommandableWorkspace workspace, Feature parentFeature,
      String propertyName, int i, Shell shell )
  {
    super( type.getName() );
    m_propertyName = propertyName;
    pos = i;
    m_workspace = workspace;
    m_type = type;
    m_parentFeature = parentFeature;
    m_shell = shell;
  }

  /**
   * @see org.eclipse.jface.action.IAction#run()
   */
  public void run()
  {
    AddFeatureCommand command = new AddFeatureCommand( m_workspace, m_type, m_parentFeature,
        m_propertyName, pos );
    try
    {
      m_workspace.postCommand( command );
      return;
    }
    catch( final Exception e )
    {
      final String msg = e.getMessage();
      final IStatus status = new Status( IStatus.ERROR, "org.kalypso.ui.editor.GMLEditor", 0, msg == null ? "" : msg, null );
      ErrorDialog.openError( m_shell, "ERROR", e.getMessage(), status );
      e.printStackTrace();
    }
  }
}