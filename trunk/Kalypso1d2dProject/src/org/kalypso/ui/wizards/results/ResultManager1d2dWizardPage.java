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
package org.kalypso.ui.wizards.results;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemException;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.model.ICommandPoster;
import org.kalypso.commons.KalypsoCommonsPlugin;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsomodel1d2d.sim.ISimulation1D2DConstants;
import org.kalypso.kalypsomodel1d2d.sim.ProcessResultsBean;
import org.kalypso.kalypsomodel1d2d.sim.ResultManager;
import org.kalypso.kalypsomodel1d2d.sim.ResultManagerOperation;
import org.kalypso.kalypsomodel1d2d.sim.ResultProcessingOperation;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ui.wizards.i18n.Messages;

import de.renew.workflow.connector.cases.ICaseDataProvider;

/**
 * @author Thomas Jung
 * 
 */

@SuppressWarnings("unchecked")
public class ResultManager1d2dWizardPage extends SelectResultWizardPage
{

  protected IKalypsoLayerModell m_modell;

  protected ICommandTarget m_commandTarget;

  protected ICaseDataProvider m_modelProvider;

  protected Button m_reevaluateButton;

  protected IStatus resultStatus;

  public ResultManager1d2dWizardPage( final String pageName, final String title, final ImageDescriptor titleImage, final ViewerFilter filter, Result1d2dMetaComparator comparator, final IThemeConstructionFactory factory, final IGeoLog geoLog )
  {
    super( pageName, title, titleImage, filter, comparator, factory, geoLog );
  }

  /**
   * @see org.kalypso.ui.wizards.results.SelectResultWizardPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    // HACK: add an extra button 'Delete'
    final Composite panel = new Composite( parent, SWT.BORDER );
    panel.setLayout( new GridLayout() );

    super.createControl( panel );

    getControl().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    final Button deleteButton = new Button( panel, SWT.PUSH );
    deleteButton.setToolTipText( Messages.getString( "org.kalypso.ui.wizards.results.ResultManager1d2dWizardPage.0" ) ); //$NON-NLS-1$

    final PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();

    final Image deleteImage = imageProvider.getImage( KalypsoModel1D2DUIImages.IMGKEY.DELETE );
    deleteButton.setImage( deleteImage );
    deleteButton.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final CheckboxTreeViewer treeViewer = getTreeViewer();
        final IResultMeta[] selectedResults = getSelectedResults();
        for( final IResultMeta resultMeta : selectedResults )
        {
          if( resultMeta instanceof IStepResultMeta )
          {

            /* handle result meta entries */
            final IStepResultMeta stepResult = (IStepResultMeta) resultMeta;
            ResultMeta1d2dHelper.removeResult( stepResult );

            /* handle map themes */
            if( m_modell != null && m_commandTarget != null )
              ResultMeta1d2dHelper.deleteResultThemeFromMap( stepResult, m_modell, m_commandTarget );

            /* handle tree */
            ViewerUtilities.refresh( treeViewer, true );
          }
        }
      }
    } );

    m_reevaluateButton = new Button( panel, SWT.PUSH );
    m_reevaluateButton.setToolTipText( Messages.getString( "org.kalypso.ui.wizards.results.ResultManager1d2dWizardPage.7" ) ); //$NON-NLS-1$

    final Image reevaluateButtonImage = imageProvider.getImage( KalypsoModel1D2DUIImages.IMGKEY.GO );
    m_reevaluateButton.setImage( reevaluateButtonImage );
    m_reevaluateButton.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        reevaluateResults();
        /* handle tree */
        final CheckboxTreeViewer treeViewer = getTreeViewer();
        ViewerUtilities.refresh( treeViewer, true );
      }
    } );
    setControl( panel );
  }

  protected void reevaluateResults( )
  {
    try
    {
      m_reevaluateButton.setEnabled( false );
      final IResultMeta[] selectedResults = getSelectedResults();

      final IContainer scenarioFolder = KalypsoAFGUIFrameworkPlugin.getDefault().getActiveWorkContext().getCurrentCase().getFolder();

      // final ICoreRunnableWithProgress calculationOperation = new ICoreRunnableWithProgress()
      final IRunnableWithProgress calculationOperation = new IRunnableWithProgress()
      {
        public IStatus execute( final IProgressMonitor monitor )
        {
          monitor.beginTask( "", 100 * selectedResults.length );//$NON-NLS-1$
          for( final IResultMeta resultMeta : selectedResults )
          {
            if( resultMeta instanceof IStepResultMeta )
            {

              /* handle result meta entries */
              final IStepResultMeta stepResult = (IStepResultMeta) resultMeta;
              final ProcessResultsBean bean = new ProcessResultsBean();

              if( m_modell != null && m_commandTarget != null )
                ResultMeta1d2dHelper.deleteResultThemeFromMap( stepResult, m_modell, m_commandTarget );

              bean.deleteAll = false;
              bean.deleteFollowers = false;
              bean.evaluateFullResults = true;

              bean.userCalculatedSteps = new Date[] { stepResult.getStepTime() };
              FileObject actResult = null;
              FileObject fileObjSWANResult = null;
              ResultManager resultManager = null;
              final ILog lLog = KalypsoCommonsPlugin.getDefault().getLog();
              try
              {
                actResult = VFSUtilities.getNewManager().resolveFile( scenarioFolder.getFolder( stepResult.getFullPath() ).getLocationURI().toURL().toExternalForm() );
                fileObjSWANResult = actResult.resolveFile( ResultMeta1d2dHelper.getSavedSWANRawResultData( stepResult ).toOSString() );
              }
              catch( Exception e )
              {
                lLog.log( StatusUtilities.createWarningStatus( Messages.getString( "org.kalypso.ui.wizards.results.ResultManager1d2dWizardPage.8" ) ) );
              }

              try
              {
                resultManager = new ResultManager( actResult, fileObjSWANResult, m_modelProvider, m_geoLog );
              }
              catch( CoreException e )
              {
                lLog.log( StatusUtilities.statusFromThrowable( e ) );
              }

              try
              {
                resultManager.setStepsToProcess( bean.userCalculatedSteps, resultManager.getControlModel() );
              }
              catch( IOException e1 )
              {
                resultStatus = StatusUtilities.statusFromThrowable( e1 );
                return resultStatus;
              }
              final ResultProcessingOperation processingOperation = new ResultProcessingOperation( resultManager, bean );
              resultStatus = processingOperation.execute( monitor );
              // if anything happened during the processing, restore the original results db from disk
              if( resultStatus.isOK() != true )
              {
                lLog.log( resultStatus );
                try
                {
                  // set the dirty flag of the results model
                  ((ICommandPoster) m_modelProvider).postCommand( IScenarioResultMeta.class.getName(), new EmptyCommand( "", false ) ); //$NON-NLS-1$
                }
                catch( Exception e )
                {
                  lLog.log( StatusUtilities.statusFromThrowable( e ) );
                }

                m_modelProvider.reloadModel();
              }

              // if OK move the new results data to the results folder
              // this operation is not cancelable
              if( resultStatus.isOK() )
              {
                // processing finished without problems, prepare the data-operation
                // this is where the name of the result folder is actually set
                final ICalcUnitResultMeta calcUnitMeta = processingOperation.getCalcUnitMeta();
                final String calcUnitId = calcUnitMeta.getCalcUnit();
                List< String > lListResultsToRemove = new ArrayList<String>();
                lListResultsToRemove.addAll( Arrays.asList( processingOperation.getOriginalStepsToDelete() ) );
                if( lListResultsToRemove.size() == 0 ){
                  lListResultsToRemove.add( stepResult.getGmlID() );
                }
                lListResultsToRemove = removeAllOthersStepWithDate( lListResultsToRemove, stepResult.getGmlID() );
                  
                String[] lResultsToRemove = lListResultsToRemove.toArray( new String[ lListResultsToRemove.size() ] );

                final Path unitFolderRelativePath = new Path( "results/" + calcUnitId ); //$NON-NLS-1$
                // remove temporary unzipped swan data
                try
                {
                  FileObject unzippedSwanFile = VFSUtilities.getNewManager().resolveFile( processingOperation.getOutputDir(), ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE + "."
                      + ISimulation1D2DConstants.SIM_SWAN_MAT_RESULT_EXT );
                  FileObject unzippedShiftFile = VFSUtilities.getNewManager().resolveFile( processingOperation.getOutputDir(), ISimulation1D2DConstants.SIM_SWAN_COORD_SHIFT_FILE );
                  FileObject unzippedTabFile = VFSUtilities.getNewManager().resolveFile( processingOperation.getOutputDir(), ISimulation1D2DConstants.SIM_SWAN_TRIANGLE_FILE  + "_out.tab" );
                  unzippedSwanFile.delete();
                  unzippedShiftFile.delete();
                  unzippedTabFile.delete();
                }
                catch( FileSystemException e )
                {
                  lLog.log( StatusUtilities.statusFromThrowable( e ) );
                }
                IFolder unitFolder = scenarioFolder.getFolder( unitFolderRelativePath );
                final ResultManagerOperation dataOperation = new ResultManagerOperation( resultManager, unitFolder, Status.OK_STATUS, m_modelProvider, processingOperation.getOutputDir(), calcUnitMeta, lResultsToRemove ); // processingOperation.getOriginalStepsToDelete()
                dataOperation.setBoolRemoveRawResult( false );
                resultStatus = dataOperation.execute( monitor );
              }
            }
          }
          return resultStatus;
        }

        private List< String > removeAllOthersStepWithDate( List<String> lListResultsToRemove, final String stepId )
        {
          List< String > lListRes = new ArrayList<String>();
          for( final String lId: lListResultsToRemove ){
            if( lId.equals( stepId ) ){
              lListRes.add( lId );
              break;
            }
          }
          return lListRes;
        }

        @Override
        public void run( IProgressMonitor monitor )
        {
          execute( monitor );
        }
      };

      ProgressMonitorDialog dialog = new ProgressMonitorDialog( getShell() );
      try
      {
        dialog.run( true, true, calculationOperation );
      }
      catch( InvocationTargetException e1 )
      {
        e1.printStackTrace();
      }
      catch( InterruptedException e1 )
      {
        e1.printStackTrace();
      }

    }
    catch( CoreException e )
    {
      e.printStackTrace();
    }
    finally
    {
      m_reevaluateButton.setEnabled( true );
    }

  }

  public void setMapModel( final IKalypsoLayerModell modell )
  {
    m_modell = modell;
  }

  public void setCommandTarget( final ICommandTarget commandTarget )
  {
    m_commandTarget = commandTarget;
  }

  public void setCaseDataProvidet( final ICaseDataProvider modelProvider )
  {
    m_modelProvider = modelProvider;
  }

}
