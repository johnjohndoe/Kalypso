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
import org.eclipse.jface.action.Action;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.ogc.gml.featureview.FeatureComposite;
import org.kalypso.ogc.gml.featureview.FeatureviewDialog;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.util.command.ICommandTarget;

public final class EditFeatureAction extends Action
{

  CommandableWorkspace m_workspace;

  private final Feature m_feature;

  final ICommandTarget m_commandTarget;

  final Shell m_shell;

  public EditFeatureAction( Feature feature, CommandableWorkspace workspace,
      ICommandTarget commandTarget, Shell shell )
  {
    super( "Bearbeiten" );
    m_feature = feature;
    m_workspace = workspace;
    m_commandTarget = commandTarget;
    m_shell = shell;
  }

  /**
   * @see org.eclipse.jface.action.IAction#run()
   */
  public void run()
  {
    final FeatureComposite helper = new FeatureComposite( m_feature );

    m_shell.getDisplay().asyncExec( new Runnable()
    {
      public void run()
      {
        final FeatureviewDialog dialog = new FeatureviewDialog( m_workspace, m_commandTarget, m_shell, helper );
        dialog.open();
      }
    } );
  }
}