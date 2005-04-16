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
package org.kalypso.ui.view;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.eclipse.swt.graphics.FontUtilities;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.calcwizard.CalcWizard;
import org.kalypso.ui.calcwizard.CalcWizardDialog;
import org.kalypso.util.synchronize.ModelSynchronizer;

/**
 * @author belger
 */
public class PrognoseView extends ViewPart
{
  private Button m_button;

  private final PrognosePanel m_panel;

  private final FontUtilities m_fontUtils = new FontUtilities();

  public PrognoseView()
  {
    final URL location = KalypsoGisPlugin.getDefault().getModellistLocation();
    m_panel = location == null ? null : new PrognosePanel( location );
  }

  public void dispose()
  {
    if( m_panel != null )
      m_panel.dispose();
    m_fontUtils.dispose();
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( final Composite parent )
  {
    final Display display = parent.getDisplay();
    final FormToolkit toolkit = new FormToolkit( display );
    final Form form = toolkit.createForm( parent );
    form.setBackground( display.getSystemColor( SWT.COLOR_WHITE ) );

    final GridLayout gridLayout = new GridLayout( 1, false );
//    gridLayout.horizontalSpacing = 20;
//    gridLayout.verticalSpacing = 20;
//    gridLayout.marginHeight = 20;
//    gridLayout.marginWidth = 20;
    form.getBody().setLayout( gridLayout );
    form.getBody().setBackground( display.getSystemColor( SWT.COLOR_WHITE ) );

    final GridData formGridData = new GridData( GridData.FILL_BOTH );
    formGridData.horizontalAlignment = GridData.CENTER;
    form.setLayoutData( formGridData );

    if( m_panel == null )
    {
      final Label label = toolkit.createLabel( form.getBody(),
          "Es konnte kein Kontakt zum Server hergestellt werden.\n"
              + "Hochwasser-Vorhersage nicht möglich.\n" );

      label.setBackground( display.getSystemColor( SWT.COLOR_WHITE ) );

      final GridData labelData = new GridData( GridData.FILL_BOTH );
      labelData.horizontalAlignment = GridData.CENTER;
      label.setLayoutData( labelData );

      final Font headingFont = m_fontUtils.createChangedFontData( label.getFont().getFontData(), 8,
          SWT.NONE, label.getDisplay() );
      label.setFont( headingFont );
    }
    else
    {
      final Composite panelControl = m_panel.createControl( form.getBody() );
      panelControl.setBackground( display.getSystemColor( SWT.COLOR_WHITE ) );
      panelControl.setLayoutData( new GridData( GridData.FILL_BOTH ) );

      m_button = toolkit.createButton( form.getBody(), "Hochwasser-Vorhersage starten", SWT.PUSH );

      final PrognosePanel panel = m_panel;
      m_button.addSelectionListener( new SelectionAdapter()
      {
        public void widgetSelected( final SelectionEvent e )
        {
          startModel( panel.getModel() );
        }
      } );
    }
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  public void setFocus()
  {
  // mir doch egal
  }

  protected void startModel( final String projectName )
  {
    final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();

    // Projekt updaten
    final File serverRoot = KalypsoGisPlugin.getDefault().getServerModelRoot();

    if( serverRoot == null )
    {
      // todo: error handling
      return;
    }

    final IProject project = root.getProject( projectName );
    final File serverProject = new File( serverRoot, projectName );

    if( !serverProject.exists() )
    {
      // todo: error message
      System.out.println( "Servermodel does not exist! Cannot start Prognose." );
      return;
    }

    final IRunnableWithProgress op = new IRunnableWithProgress()
    {
      public void run( IProgressMonitor monitor ) throws InvocationTargetException
      {
        try
        {
          final ModelSynchronizer synchronizer = new ModelSynchronizer( project, serverProject );
          synchronizer.updateLocal( monitor );
        }
        catch( final CoreException ce )
        {
          ce.printStackTrace();
          throw new InvocationTargetException( ce );
        }
      }
    };

    final IWorkbench workbench = PlatformUI.getWorkbench();
    try
    {
      workbench.getProgressService().busyCursorWhile( op );
    }
    catch( final InvocationTargetException e )
    {
      e.printStackTrace();

      final CoreException ce = (CoreException)e.getTargetException();
      ErrorDialog.openError( workbench.getDisplay().getActiveShell(), "Vorhersage starten",
          "Modell konnte nicht aktualisiert werden", ce.getStatus() );
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
    }

    final CalcWizard wizard = new CalcWizard( project );

    final WizardDialog dialog = new CalcWizardDialog( getSite().getShell(), wizard );
    dialog.open();
  }
}