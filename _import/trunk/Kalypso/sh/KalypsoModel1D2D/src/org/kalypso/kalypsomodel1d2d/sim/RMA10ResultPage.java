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
package org.kalypso.kalypsomodel1d2d.sim;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.util.swt.StatusComposite;

import de.renew.workflow.connector.cases.ICaseDataProvider;

/**
 * @author Gernot Belger
 */
public class RMA10ResultPage extends WizardPage implements IWizardPage, ISimulation1D2DConstants
{
  private final ResultManager m_resultManager;

  private IStatus m_simulationStatus;

  private final IContainer m_unitFolder;

  private IStatus m_resultStatus;

  private final ICaseDataProvider<IModel> m_caseDataProvider;

  private StatusComposite m_statusComp;

  protected boolean m_deleteAllResults;

  protected RMA10ResultPage( final String pageName, final ResultManager resultManager, final IContainer unitFolder, final ICaseDataProvider<IModel> caseDataProvider )
  {
    super( pageName );

    m_resultManager = resultManager;
    m_unitFolder = unitFolder;
    m_caseDataProvider = caseDataProvider;

    setTitle( "Ergebnisauswertung - " + resultManager.getControlModel().getCalculationUnit().getName() );

    if( m_unitFolder.exists() )
      setMessage( "Alte Ergebnisdaten liegen vor.\nWenn Sie fortfahren werden diese unwiderruflich gelöscht.", WARNING );
    else
      setMessage( "Drücken Sie 'Start', um die Ergebnisse auszuwerten." );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout() );

    /* Status composite */
    final Group statusGroup = new Group( composite, SWT.NONE );
    statusGroup.setLayout( new GridLayout() );
    statusGroup.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    statusGroup.setText( "Auswertungsergebnis" );

    m_statusComp = new StatusComposite( statusGroup, StatusComposite.DETAILS );
    m_statusComp.setLayoutData( new GridData( SWT.FILL, SWT.LEFT, true, false ) );
    m_statusComp.setStatus( StatusUtilities.createStatus( IStatus.INFO, "Ergebnisauswertung wurde noch nicht durchgeführt", null ) );

    /* Control flags */
    final Group tweakGroup = new Group( composite, SWT.NONE );
    tweakGroup.setLayout( new GridLayout() );
    tweakGroup.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    tweakGroup.setText( "Einstellungen" );

    final IControlModel1D2D controlModel = m_resultManager.getControlModel();
    m_deleteAllResults = !controlModel.getRestart();

    final Button deleteAllCheck = new Button( tweakGroup, SWT.CHECK );
    deleteAllCheck.setText( "Alle vorhandenen Ergebnisse löschen" );
    deleteAllCheck.setToolTipText( "Falls gesetzt, werden ALLE vorhandenen Ergebnisse gelöscht." );
    deleteAllCheck.setSelection( m_deleteAllResults );
    deleteAllCheck.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        m_deleteAllResults = deleteAllCheck.getSelection();
      }
    } );
    // If non-restart, always all results must be deleted.
    deleteAllCheck.setEnabled( !controlModel.getRestart() );

    // TODO: checklist with calculated steps to choose from

    /* Result chooser */
    final Composite resultChooserComp = new Composite( composite, SWT.NONE );
    resultChooserComp.setLayout( new GridLayout( 2, false ) );
    resultChooserComp.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    setControl( composite );
  }

  public void runResultProcessing( )
  {
    m_statusComp.setStatus( StatusUtilities.createStatus( IStatus.INFO, "Ergebnisauswertung läuft...", null ) );
    setMessage( "Ergebnisauswertung läuft..." );

    final ProcessResultsBean bean = new ProcessResultsBean();

    // Remark: if not restart, always delete everything, in that
    // case do not ask the user either
    // TODO: make ui for the bean
    bean.deleteAll = m_deleteAllResults;
    bean.deleteFollowers = true;
    bean.userCalculatedSteps = m_resultManager.findCalculatedSteps();

    /* Result processing */
    final ResultManagerOperation operation = new ResultManagerOperation( m_resultManager, m_unitFolder, m_simulationStatus, bean, m_caseDataProvider );

    final IWizardContainer container = getContainer();
    if( container instanceof WizardDialog2 )
    {
      final WizardDialog2 wd2 = (WizardDialog2) container;
      m_resultStatus = wd2.executeUnblocked( true, operation );
    }
    else
      m_resultStatus = RunnableContextHelper.execute( container, true, true, operation );

    /* The user may have changed the page meanwhile, return now to this page */
    if( container.getShell() != null )
    {
      container.showPage( this );

      m_statusComp.setStatus( m_resultStatus );

      setMessage( "Ergebnisauswertung beendet. Drücken Sie 'Beenden', um den Dialog zu schliesen." );
    }
  }

  public IStatus getResultStatus( )
  {
    return m_resultStatus;
  }

}
