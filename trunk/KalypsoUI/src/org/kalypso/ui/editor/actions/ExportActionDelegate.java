/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.ui.editor.actions;

import java.lang.reflect.InvocationTargetException;

import org.apache.commons.configuration.BaseConfiguration;
import org.apache.commons.configuration.Configuration;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowPulldownDelegate2;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.metadoc.IExportTarget;
import org.kalypso.metadoc.IExportableObject;
import org.kalypso.metadoc.IExportableObjectFactory;
import org.kalypso.metadoc.KalypsoMetaDocPlugin;

/**
 * @author schlienger
 */
public class ExportActionDelegate extends Action implements IMenuCreator, IWorkbenchWindowPulldownDelegate2,
    IPartListener2
{
  private Menu m_menu = null;
  private IWorkbenchPage m_page = null;
  private IAction m_action;

  /**
   * @see org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets.Control)
   */
  public Menu getMenu( Control parent )
  {
    if( m_menu != null )
      m_menu.dispose();

    m_menu = new Menu( parent );
    fillMenu( m_menu );

    return m_menu;
  }

  /**
   * @see org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets.Menu)
   */
  public Menu getMenu( Menu parent )
  {
    if( m_menu == null )
    {
      m_menu = new Menu( parent );
      fillMenu( m_menu );
    }

    return m_menu;
  }

  private void fillMenu( final Menu menu )
  {
    try
    {
      final IExportTarget[] targets = KalypsoMetaDocPlugin.getDefault().getTargets();
      for( int i = 0; i < targets.length; i++ )
      {
        final IExportTarget target = targets[i];
        final IAction action = new Action( target.getName(), target.getImage() )
        {
          public void run()
          {
            final IWorkbenchPart part = m_page.getActivePart();
            if( part == null )
              return;

            final IExportableObjectFactory factory = (IExportableObjectFactory)part
                .getAdapter( IExportableObjectFactory.class );
            if( factory == null )
              return;

            exportObjects( target, factory, part.getSite().getShell() );
          }
        };
        action.setToolTipText( target.getDescription() );

        new ActionContributionItem( action ).fill( menu, -1 );
      }
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      ErrorDialog.openError( menu.getShell(), "Export Targets laden", "Fehler beim Laden der Export Targets", e
          .getStatus() );
    }
  }

  protected void exportObjects( final IExportTarget target, final IExportableObjectFactory factory, final Shell shell )
  {
    final Configuration configuration = new BaseConfiguration();
    
    final IWizardPage[] factoryPages = factory.createWizardPages( configuration );
    final IWizardPage[] targetPages = target.createWizardPages( configuration );

    // show wizard
    final Wizard wizard = new Wizard()
    {
      public boolean performFinish()
      {
        final WorkspaceModifyOperation operation = new WorkspaceModifyOperation()
        {
          protected void execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException,
              InterruptedException
          {
            try
            {
              final IExportableObject[] objects = factory.createExportableObjects( configuration );

              monitor.beginTask( "", objects.length );

              for( int i = 0; i < objects.length; i++ )
                target.commitDocument( objects[i], configuration, new SubProgressMonitor( monitor, 1 ) );
            }
            finally
            {
              monitor.done();
            }
          }
        };

        final IStatus status = RunnableContextHelper.execute( getContainer(), true, true, operation );
        ErrorDialog.openError( shell, "Dokument exportieren", "Dokument konnte nicht exportiert werden", status );
        return status.isOK();
      }
    };

    for( int i = 0; i < factoryPages.length; i++ )
      wizard.addPage( factoryPages[i] );
    for( int i = 0; i < targetPages.length; i++ )
      wizard.addPage( targetPages[i] );

    // wizard: factory-pages + target-pages
    final WizardDialog dlg = new WizardDialog( shell, wizard );
    dlg.open();
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#dispose()
   */
  public void dispose()
  {
    if( m_menu != null )
    {
      m_menu.dispose();
      m_menu = null;
    }

    if( m_page != null )
    {
      m_page.removePartListener( this );
      m_page = null;
    }
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#init(org.eclipse.ui.IWorkbenchWindow)
   */
  public void init( final IWorkbenchWindow window )
  {
    m_page = window.getActivePage();
    m_page.addPartListener( this );
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
  // empty
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    m_action = action;
  }

  /**
   * @see org.eclipse.ui.IPartListener2#partActivated(org.eclipse.ui.IWorkbenchPartReference)
   */
  public void partActivated( IWorkbenchPartReference partRef )
  {
    if( m_action != null )
      m_action.setEnabled( partRef.getPart( false ).getAdapter( IExportableObjectFactory.class ) != null );
  }

  /**
   * @see org.eclipse.ui.IPartListener2#partBroughtToTop(org.eclipse.ui.IWorkbenchPartReference)
   */
  public void partBroughtToTop( IWorkbenchPartReference partRef )
  {
  // empty
  }

  /**
   * @see org.eclipse.ui.IPartListener2#partClosed(org.eclipse.ui.IWorkbenchPartReference)
   */
  public void partClosed( IWorkbenchPartReference partRef )
  {
  // empty
  }

  /**
   * @see org.eclipse.ui.IPartListener2#partDeactivated(org.eclipse.ui.IWorkbenchPartReference)
   */
  public void partDeactivated( IWorkbenchPartReference partRef )
  {
  // empty
  }

  /**
   * @see org.eclipse.ui.IPartListener2#partOpened(org.eclipse.ui.IWorkbenchPartReference)
   */
  public void partOpened( IWorkbenchPartReference partRef )
  {
  // empty
  }

  /**
   * @see org.eclipse.ui.IPartListener2#partHidden(org.eclipse.ui.IWorkbenchPartReference)
   */
  public void partHidden( IWorkbenchPartReference partRef )
  {
  // empty
  }

  /**
   * @see org.eclipse.ui.IPartListener2#partVisible(org.eclipse.ui.IWorkbenchPartReference)
   */
  public void partVisible( IWorkbenchPartReference partRef )
  {
  // empty
  }

  /**
   * @see org.eclipse.ui.IPartListener2#partInputChanged(org.eclipse.ui.IWorkbenchPartReference)
   */
  public void partInputChanged( IWorkbenchPartReference partRef )
  {
  // empty
  }
}
