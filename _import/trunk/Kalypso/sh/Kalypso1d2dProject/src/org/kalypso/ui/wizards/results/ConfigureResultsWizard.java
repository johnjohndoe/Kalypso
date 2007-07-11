/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.ui.wizards.results;

import java.lang.reflect.InvocationTargetException;
import java.net.URL;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectWizardPage;
import org.kalypso.kalypsomodel1d2d.schema.binding.metadata.IResultModelDescriptor;
import org.kalypso.kalypsomodel1d2d.schema.binding.metadata.ISimulationDescriptor;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ui.action.AddThemeCommand;
import org.kalypso.ui.editor.mapeditor.AbstractMapPart;

import de.renew.workflow.connector.cases.CaseHandlingSourceProvider;
import de.renew.workflow.contexts.IDialogWithResult;

/**
 * @author GernotBelger
 */
public class ConfigureResultsWizard extends Wizard implements IWorkbenchWizard, IDialogWithResult
{
  private RestartSelectWizardPage m_restartSelectWizardPage;

  private IFolder m_szenarioFolder;

  private AbstractMapPart m_part;

  private IStatus m_result;

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    setWindowTitle( "Ergebniskarte konfigurieren" );

    m_restartSelectWizardPage = new RestartSelectWizardPage( "" );
    m_restartSelectWizardPage.setTitle( "Ergebnisse auswählen" );
    m_restartSelectWizardPage.setDescription( "Hier können Sie ein oder mehrere Ergebnisse für die Ergebsnikarte auswählen." );

    addPage( m_restartSelectWizardPage );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final IResultModelDescriptor[] selectedResults = m_restartSelectWizardPage.getSelectedResults();

    final MapPanel panel = m_part == null ? null : (MapPanel) m_part.getAdapter( MapPanel.class );
    final GisTemplateMapModell modell = (GisTemplateMapModell) (panel == null ? null : panel.getMapModell());

    final ICoreRunnableWithProgress operation = new ICoreRunnableWithProgress()
    {
      public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
      {
        monitor.beginTask( "", selectedResults.length );

        try
        {
          // TODO: remove already existent results from list

          // add/remove selected results from/to map

          for( final IResultModelDescriptor result : selectedResults )
          {
            final String gmt = result.getGmt();
            final IFile file = m_szenarioFolder.getFile( new Path( gmt ) );
            final URL gmtUrl = ResourceUtilities.createURL( file );

            final ISimulationDescriptor simulationDescriptor = result.getParent();
            final String calcUnitName = simulationDescriptor == null ? "?" : simulationDescriptor.getName();
            final String themeLabel = "Ergebniskarte - " + calcUnitName + " -  " + result.getModelName();

            final AddThemeCommand cmd = new AddThemeCommand( modell, themeLabel, "gmt", "", gmtUrl.toExternalForm() );
            m_part.postCommand( cmd, null );

            monitor.worked( 1 );
          }
          return Status.OK_STATUS;
        }
        // catch( final CoreException ce )
        // {
        // throw ce;
        // }
        catch( final Throwable e )
        {
          e.printStackTrace();

          throw new InvocationTargetException( e );
        }
        finally
        {
          monitor.done();
        }
      }
    };

    if( modell == null )
      m_result = StatusUtilities.createErrorStatus( "Keine Karte vorhanden." );
    else
      m_result = RunnableContextHelper.execute( getContainer(), true, false, operation );
    ErrorDialog.openError( getShell(), getWindowTitle(), "Themen konnten nicht hinzugefügt werden.", m_result );

    return m_result.isOK();
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    // find currently open map
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext currentState = handlerService.getCurrentState();
    m_szenarioFolder = (IFolder) currentState.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
    m_part = (AbstractMapPart) currentState.getVariable( ISources.ACTIVE_PART_NAME );
  }

  /**
   * @see de.renew.workflow.contexts.IDialogWithResult#getResult()
   */
  public Object getResult( )
  {
    return m_result;
  }

}
