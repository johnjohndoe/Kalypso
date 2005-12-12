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

import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.ICommand;
import org.kalypso.ogc.gml.featureTypeDialog.FeatureTypeSelectionDialog;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureType;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author kuepfer
 */
public class AddEmptyLinkAction extends Action
{
  private final FeatureAssociationTypeProperty m_fatp;

  private final CommandableWorkspace m_workspace;

  private final Feature m_parentFeature;

  private final IFeatureSelectionManager m_selectionManager;

  public AddEmptyLinkAction( String text, ImageDescriptor image, FeatureAssociationTypeProperty fatp,
      Feature parentFeature, CommandableWorkspace workspace, final IFeatureSelectionManager selectionManager )
  {
    super( text, image );
    m_fatp = fatp;
    m_workspace = workspace;
    m_parentFeature = parentFeature;
    m_selectionManager = selectionManager;
  }

  public void run()
  {
    FeatureType[] featureTypes = m_fatp.getAssociationFeatureTypes();
    Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
    ICommand command = null;
    FeatureTypeSelectionDialog dialog = null;
    FeatureType ft = null;
    if( featureTypes.length > 1 )
    {
      dialog = new FeatureTypeSelectionDialog( shell, featureTypes, SWT.MULTI );
      int open = dialog.open();
      if( open == Window.OK )
      {
        FeatureType[] types = dialog.getSelectedFeatureTypes();
        ft = types[0];
        if( ft == null )
          return;
      }
    }
    else
      ft = featureTypes[0];
    command = new AddFeatureCommand( m_workspace, ft, m_parentFeature, m_fatp.getName(), 0, null, m_selectionManager );
    try
    {
      m_workspace.postCommand( command );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

  }

}
