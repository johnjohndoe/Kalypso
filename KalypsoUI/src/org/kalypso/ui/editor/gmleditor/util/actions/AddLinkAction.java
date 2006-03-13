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
package org.kalypso.ui.editor.gmleditor.util.actions;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.util.command.AddRelationCommand;
import org.kalypsodeegree.model.feature.Feature;

public class AddLinkAction extends Action
{
  private IRelationType m_propertyName;

  private int pos = 0;

  private CommandableWorkspace m_workspace;

  private IFeatureType m_type;

  private Feature m_parentFeature;

  private Shell m_shell;

  public AddLinkAction( IFeatureType type, CommandableWorkspace workspace, Feature parentFeature, IRelationType propertyName,
      int i, Shell shell )
  {
    super( type.getName() + " Link" );
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
  @Override
  public void run()
  {
    AddRelationCommand command = null;
    Feature[] features = m_workspace.getFeatures( m_type );
    AddLinkDialog dialog = new AddLinkDialog( m_shell, features );
    int open = dialog.open();
    if( open == Window.OK )
    {
      String result = dialog.getFeatureListSelection();
      if( result != null )
      {
        for( int i = 0; i < features.length; i++ )
        {
          if( result.equals( features[i].getId() ) )
          {
            Feature linkFeature = features[i];
            command = new AddRelationCommand( m_workspace, m_parentFeature, m_propertyName, pos, linkFeature );
            break;
          }
        }
      }
    }

    try
    {
      if( command != null )
        m_workspace.postCommand( command );
      //m_commandTarget.postCommand( command, null );
      return;
    }
    catch( final Exception e )
    {
      final String msg = e.getMessage();
      final IStatus status = new Status( IStatus.ERROR, "org.kalypso.ui.editor.GmlEditor", 0, msg == null ? "" : msg,
          null );
      ErrorDialog.openError( m_shell, "ERROR", e.getMessage(), status );
      e.printStackTrace();
    }
  }

  class AddLinkDialog extends Dialog
  {

    Feature[] m_features = null;

    List featureList = null;

    String[] listSelections = null;

    public AddLinkDialog( Shell shell, Feature[] features )
    {
      super( shell );
      m_features = features;
    }

    @Override
    protected Control createDialogArea( final Composite parent )
    {
      final Composite area = new Composite( parent, SWT.NULL );

      area.getShell().setText( "Select a Feature" );

      GridLayout gridLayout = new GridLayout();
      gridLayout.marginWidth = 15;
      gridLayout.marginHeight = 10;
      area.setLayout( gridLayout );

      featureList = new List( area, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL | SWT.H_SCROLL );
      GridData gridData = new GridData();
      gridData.heightHint = 200;
      featureList.setLayoutData( gridData );
      for( int i = 0; i < m_features.length; i++ )
      {
        featureList.add( m_features[i].getId() );
      }
      featureList.addSelectionListener( new SelectionListener()
      {
        public void widgetSelected( SelectionEvent e )
        {
          listSelections = ( (List)e.getSource() ).getSelection();
        }

        public void widgetDefaultSelected( SelectionEvent e )
        {//nothing
        }
      } );

      return area;
    }

    public String getFeatureListSelection()
    {
      String result = null;
      if( featureList != null )
      {
        if( listSelections != null )
        {
          result = listSelections[0];
        }
      }
      return result;
    }
  }
}