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
package org.kalypso.ui.wizards.lengthsection;

import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ui.wizards.results.SelectResultWizardPage;

/**
 * Wizard to show length sections to the chart view.
 * 
 * @author Thomas Jung
 */
public class SelectLengthSectionWizard extends Wizard
{
  private final static String PAGE_SELECT_RESULTS_NAME = "selectLengthSection";

  private final IScenarioResultMeta m_resultModel;

  private final IFolder m_scenarioFolder;

  private IFile m_selectedResultFile;

  public SelectLengthSectionWizard( IFolder scenarioFolder, final IScenarioResultMeta resultModel )
  {
    m_scenarioFolder = scenarioFolder;
    m_resultModel = resultModel;
    setWindowTitle( "1D2D-Ergebnisse" );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    final LengthSectionViewerFilter resultFilter = new LengthSectionViewerFilter();
    final SelectResultWizardPage selectResultWizardPage = new SelectResultWizardPage( PAGE_SELECT_RESULTS_NAME, "Ergebniss(e) zur Karte hinzufügen", null, resultFilter, null );

    selectResultWizardPage.setResultMeta( m_resultModel );

    addPage( selectResultWizardPage );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final SelectResultWizardPage page = (SelectResultWizardPage) getPage( PAGE_SELECT_RESULTS_NAME );
    final IResultMeta[] results = page.getSelectedResults();
    if( results.length == 0 )
    {
      MessageDialog.openInformation( getShell(), "Längsschnitt anzeigen", "Bitte wählen Sie einen Längsschnitt aus." );
      return false;
    }

    /* Start */
    final ICoreRunnableWithProgress op = new ICoreRunnableWithProgress()
    {
      @SuppressWarnings("synthetic-access")
      public IStatus execute( final IProgressMonitor monitor )
      {
        IResultMeta result = null;
        monitor.beginTask( "Längsschnittanzeige...", 1 );

        // get the first length section element
        for( IResultMeta resultMeta : results )
        {
          if( resultMeta instanceof IDocumentResultMeta )
          {
            IDocumentResultMeta docResult = (IDocumentResultMeta) resultMeta;
            if( docResult.getDocumentType() == IDocumentResultMeta.DOCUMENTTYPE.lengthSection )
              result = docResult;
          }
        }
        monitor.worked( 1 );
        if( result == null )
        {
          MessageDialog.openError( getShell(), "Längsschnitt anzeigen fehlgeschlagen", "Bitte wählen Sie einen Längsschnitt aus." );
          return StatusUtilities.createErrorStatus( "Längsschnitt konnte nicht angezeigt werden. " );
        }

        // make an IFile
        IPath fullPath = result.getFullPath();

        final String kodPath = fullPath.toPortableString().replace( ".gml", ".kod" );

        m_selectedResultFile = m_scenarioFolder.getFile( Path.fromPortableString( kodPath ) );

        try
        {
          FileUtils.copyURLToFile( getClass().getResource( "resources/lengthSection.kod" ), m_selectedResultFile.getLocation().toFile() );
          m_selectedResultFile.refreshLocal( IResource.DEPTH_ONE, new NullProgressMonitor() );
          monitor.worked( 1 );
        }
        catch( IOException e )
        {
          e.printStackTrace();
        }
        catch( CoreException e )
        {
          e.printStackTrace();
        }
        return StatusUtilities.createOkStatus( "Längsschnitt dargestellt." );
      }
    };

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, op );
    if( !status.isOK() )
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
    ErrorDialog.openError( getShell(), getWindowTitle(), "Fehler bei Längsschnittanzeige", status );

    return !status.matches( IStatus.ERROR );

  }

  public IFile getSelection( )
  {
    return m_selectedResultFile;

  }

}
