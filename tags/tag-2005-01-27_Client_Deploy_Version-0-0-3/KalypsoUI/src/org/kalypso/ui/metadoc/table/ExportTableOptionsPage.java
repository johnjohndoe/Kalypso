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
package org.kalypso.ui.metadoc.table;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;

/**
 * @author belger
 */
public class ExportTableOptionsPage extends WizardPage
{
  private final static String STORE_ONLYSELECTION_ID = "ExportTableWizardPage.STORE_ONLYSELECTION_ID"; //$NON-NLS-1$

  protected static final int SIZING_TEXT_FIELD_WIDTH = 250;

  private Button m_radioSelection;

  private Button m_radioAll;

  public ExportTableOptionsPage( final String pageName, final String title,
      final ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout() );

    panel.setLayoutData( new GridData( GridData.VERTICAL_ALIGN_FILL
        | GridData.HORIZONTAL_ALIGN_FILL ) );
    panel.setFont( parent.getFont() );

    createExportOptionsGroup( panel );

    setControl( panel );

    restoreWidgetValues();
  }

  /**
   * Returns the name of a container with a location that encompasses
   * targetDirectory. Returns null if there is no conflict.
   * 
   * @param targetDirectory
   *          the path of the directory to check.
   * @return the conflicting container name or <code>null</code>
   */
  protected boolean isContainerConflicting( String targetDirectory )
  {
    IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
    IPath testPath = new Path( targetDirectory );

    if( root.getLocation().isPrefixOf( testPath ) )
      return true;

    IProject[] projects = root.getProjects();

    for( int i = 0; i < projects.length; i++ )
    {
      if( projects[i].getLocation().isPrefixOf( testPath ) )
        return true;
    }

    return false;
  }

  private void createExportOptionsGroup( final Composite parent )
  {
    final Group optionsGroup = new Group( parent, SWT.NONE );
    final GridLayout layout = new GridLayout();
    optionsGroup.setLayout( layout );
    optionsGroup.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_FILL
        | GridData.GRAB_HORIZONTAL ) );
    optionsGroup.setText( "Export Optionen" );
    optionsGroup.setFont( parent.getFont() );

    m_radioAll = new Button( optionsGroup, SWT.RADIO );
    m_radioAll.setText( "&alles exportieren" );
    m_radioAll.setSelection( true );

    m_radioSelection = new Button( optionsGroup, SWT.RADIO );
    m_radioSelection.setText( "nur &selektierte Zeilen exportieren" );
  }

  public boolean getOnlySelected()
  {
    return m_radioSelection.getSelection();
  }

  /**
   * Hook method for restoring widget values to the values that they held last
   * time this wizard was used to completion.
   */
  protected void restoreWidgetValues()
  {
    final IDialogSettings settings = getDialogSettings();
    if( settings != null )
    {
      final boolean onlySelection = settings.getBoolean( STORE_ONLYSELECTION_ID );
      m_radioSelection.setSelection( onlySelection );
      m_radioAll.setSelection( !onlySelection );
    }
  }

  public void saveWidgetValues()
  {
    final IDialogSettings settings = getDialogSettings();
    if( settings != null )
      settings.put( STORE_ONLYSELECTION_ID, m_radioSelection.getSelection() );
  }

}