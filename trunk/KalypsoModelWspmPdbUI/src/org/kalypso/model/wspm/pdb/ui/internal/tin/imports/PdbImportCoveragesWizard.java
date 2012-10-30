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
package org.kalypso.model.wspm.pdb.ui.internal.tin.imports;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IPageChangingListener;
import org.eclipse.jface.dialogs.PageChangingEvent;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.ui.forms.IMessage;
import org.kalypso.commons.databinding.swt.FileAndHistoryData;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.ui.forms.MessageUtilitites;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.gml.ui.coverage.ImportCoverageData;
import org.kalypso.gml.ui.coverage.ImportCoveragesOperation;
import org.kalypso.model.wspm.pdb.db.mapping.DhmIndex;
import org.kalypso.model.wspm.pdb.ui.internal.checkout.ConnectionChooserPage;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

/**
 * This wizard shows a page to select from an entry (path) of the external storage location and imports the coverages.
 * 
 * @author Holger Albert
 */
public class PdbImportCoveragesWizard extends Wizard
{
  /**
   * This listener is notified, if the page has changed.
   */
  private final IPageChangingListener m_pageListener = new IPageChangingListener()
  {
    @Override
    public void handlePageChanging( final PageChangingEvent event )
    {
      pageChanging( event );
    }
  };

  /**
   * The data containing the connection settings.
   */
  private final PdbImportConnectionChooserData m_settingsData;

  /**
   * The coverages container.
   */
  private final ICoverageCollection m_coveragesContainer;

  /**
   * The data container.
   */
  private final IContainer m_dataContainer;

  /**
   * The import coverages data.
   */
  private final ImportCoverageData m_data;

  /**
   * The constructor.
   * 
   * @param settingsData
   *          The data containing the connection settings.
   * @param coveragesContainer
   *          The coverages container.
   * @param dataContainer
   *          The data container.
   */
  public PdbImportCoveragesWizard( final PdbImportConnectionChooserData settingsData, final ICoverageCollection coveragesContainer, final IContainer dataContainer )
  {
    m_settingsData = settingsData;
    m_coveragesContainer = coveragesContainer;
    m_dataContainer = dataContainer;
    m_data = new ImportCoverageData();

    setWindowTitle( Messages.getString("PdbImportCoveragesWizard_0") ); //$NON-NLS-1$
    setNeedsProgressMonitor( true );
  }

  @Override
  public boolean canFinish( )
  {
    final boolean canFinish = super.canFinish();
    final IWizardPage currentPage = getContainer().getCurrentPage();
    return canFinish && !(currentPage instanceof ConnectionChooserPage);
  }

  @Override
  public void setContainer( final IWizardContainer container )
  {
    final IWizardContainer oldContainer = getContainer();
    if( oldContainer instanceof WizardDialog )
      ((WizardDialog) oldContainer).removePageChangingListener( m_pageListener );

    super.setContainer( container );

    if( container instanceof WizardDialog )
      ((WizardDialog) container).addPageChangingListener( m_pageListener );
  }

  @Override
  public void addPages( )
  {
    if( m_settingsData.getConnection() == null )
      addPage( new ConnectionChooserPage( "connectionChooser", m_settingsData ) ); //$NON-NLS-1$

    addPage( new SearchDhmIndexPage( "searchDhmIndex", m_settingsData ) ); //$NON-NLS-1$
  }

  @Override
  public boolean performFinish( )
  {
    /* Get the selected dhm index. */
    final DhmIndex dhmIndex = m_settingsData.getDhmIndex();

    /* Build the path. */
    final String filename = dhmIndex.getFilename();
    final IPath demServerPath = m_settingsData.getDemServerPath();
    final IPath filePath = demServerPath.append( filename );

    /* The source files. */
    final FileAndHistoryData fileAndHistory = new FileAndHistoryData( "sourceFiles" ); //$NON-NLS-1$
    fileAndHistory.setFile( filePath.toFile() );

    /* Get the srid. */
    final String srid = dhmIndex.getSrid();

    /* Create the data object for the operation. */
    m_data.init( m_coveragesContainer, m_dataContainer, false );
    m_data.setSourceFile( fileAndHistory );
    m_data.setSourceSRS( JTSAdapter.toSrs( Integer.valueOf( srid ) ) );

    /* Create the operation and execute it. */
    final ImportCoveragesOperation operation = new ImportCoveragesOperation( m_data );
    final IStatus operationStatus = RunnableContextHelper.execute( getContainer(), true, true, operation );
    StatusDialog.open( getShell(), operationStatus, getWindowTitle() );

    return true;
  }

  protected void pageChanging( final PageChangingEvent event )
  {
    final Object currentPage = event.getCurrentPage();
    if( currentPage instanceof ConnectionChooserPage )
      doConnect( event );
  }

  private void doConnect( final PageChangingEvent event )
  {
    /* Connect to the database. */
    final IStatus result = m_settingsData.doConnect( getContainer() );
    if( result.isOK() )
    {
      /* Change the page. */
      event.doit = true;

      return;
    }

    /* Do not change the page. */
    event.doit = false;

    /* Set a message to the current page. */
    final IMessage message = MessageUtilitites.convertStatus( result );
    ((WizardPage) event.getCurrentPage()).setMessage( message.getMessage(), message.getMessageType() );
  }

  public ICoverage[] getNewCoverages( )
  {
    return m_data.getNewCoverages();
  }
}