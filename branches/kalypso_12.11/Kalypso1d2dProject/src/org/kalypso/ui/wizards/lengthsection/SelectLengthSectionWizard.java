/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ui.wizards.results.Result1d2dMetaComparator;
import org.kalypso.ui.wizards.results.SelectResultWizardPage;

/**
 * Wizard to show length sections to the chart view.
 * 
 * @author Thomas Jung
 */
public class SelectLengthSectionWizard extends Wizard
{
  private final static String PAGE_SELECT_RESULTS_NAME = "selectLengthSection"; //$NON-NLS-1$

  private final IScenarioResultMeta m_resultModel;

  private String m_selectedLGmlResultPath;

  public SelectLengthSectionWizard( final IScenarioResultMeta resultModel )
  {
    m_resultModel = resultModel;
    setWindowTitle( Messages.getString( "org.kalypso.ui.wizards.lengthsection.SelectLengthSectionWizard.0" ) ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    final LengthSectionViewerFilter resultFilter = new LengthSectionViewerFilter();
    final Result1d2dMetaComparator comparator = new Result1d2dMetaComparator();

    final SelectResultWizardPage selectResultWizardPage = new SelectResultWizardPage( PAGE_SELECT_RESULTS_NAME, Messages.getString( "org.kalypso.ui.wizards.lengthsection.SelectLengthSectionWizard.2" ), null, resultFilter, comparator, null, null ); //$NON-NLS-1$

    selectResultWizardPage.setResultMeta( m_resultModel );

    addPage( selectResultWizardPage );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final SelectResultWizardPage page = (SelectResultWizardPage)getPage( PAGE_SELECT_RESULTS_NAME );
    final IResultMeta[] results = page.getSelectedResults();
    if( results.length == 0 )
    {
      MessageDialog.openInformation( getShell(), Messages.getString( "org.kalypso.ui.wizards.lengthsection.SelectLengthSectionWizard.3" ), Messages.getString( "org.kalypso.ui.wizards.lengthsection.SelectLengthSectionWizard.4" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      return false;
    }

    for( final IResultMeta result : results )
    {
      if( !(result instanceof IDocumentResultMeta) )
      {
        MessageDialog.openInformation( getShell(), Messages.getString( "org.kalypso.ui.wizards.lengthsection.SelectLengthSectionWizard.5" ), Messages.getString( "org.kalypso.ui.wizards.lengthsection.SelectLengthSectionWizard.6" ) ); //$NON-NLS-1$ //$NON-NLS-2$
        return false;
      }
    }

    /* Start */
    final ICoreRunnableWithProgress op = new ICoreRunnableWithProgress()
    {
      @Override
      @SuppressWarnings( "synthetic-access" )
      public IStatus execute( final IProgressMonitor monitor )
      {
        IResultMeta result = null;
        monitor.beginTask( Messages.getString( "org.kalypso.ui.wizards.lengthsection.SelectLengthSectionWizard.7" ), 2 ); //$NON-NLS-1$

        // get the first length section element

        for( final IResultMeta resultMeta : results )
        {
          if( resultMeta instanceof IDocumentResultMeta )
          {
            final IDocumentResultMeta docResult = (IDocumentResultMeta)resultMeta;
            if( docResult.getDocumentType() == IDocumentResultMeta.DOCUMENTTYPE.lengthSection )
            {
              result = docResult;
              break;
            }
          }
        }

        monitor.worked( 1 );
        if( result == null )
        {
          MessageDialog.openError( getShell(), Messages.getString( "org.kalypso.ui.wizards.lengthsection.SelectLengthSectionWizard.8" ), Messages.getString( "org.kalypso.ui.wizards.lengthsection.SelectLengthSectionWizard.9" ) ); //$NON-NLS-1$ //$NON-NLS-2$
          return new Status( IStatus.ERROR, Kalypso1d2dProjectPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.ui.wizards.lengthsection.SelectLengthSectionWizard.10" ) ); //$NON-NLS-1$
        }

        final IPath fullPath = result.getFullPath();
        m_selectedLGmlResultPath = fullPath.toPortableString();

        return new Status( IStatus.OK, Kalypso1d2dProjectPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.ui.wizards.lengthsection.SelectLengthSectionWizard.11" ) ); //$NON-NLS-1$
      }
    };

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, op );
    if( !status.isOK() )
      Kalypso1d2dProjectPlugin.getDefault().getLog().log( status );
    ErrorDialog.openError( getShell(), getWindowTitle(), Messages.getString( "org.kalypso.ui.wizards.lengthsection.SelectLengthSectionWizard.12" ), status ); //$NON-NLS-1$

    return !status.matches( IStatus.ERROR );
  }

  public String getSelectedLGmlResultPath( )
  {
    return m_selectedLGmlResultPath;
  }

}
