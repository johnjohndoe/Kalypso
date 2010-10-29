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
package org.kalypso.kalypsomodel1d2d.ui.map.flowrel;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.IOpenListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.OpenEvent;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.console.IOConsole;
import org.eclipse.ui.console.IOConsoleOutputStream;
import org.eclipse.ui.console.TextConsole;
import org.eclipse.ui.console.TextConsoleViewer;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.jface.viewers.DefaultTableViewer;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.core.status.StatusLabelProvider;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation1D;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.schema.gml.QIntervallResult;
import org.kalypso.ogc.gml.featureview.control.FeatureComposite;
import org.kalypso.ogc.gml.featureview.maker.CachedFeatureviewFactory;
import org.kalypso.ogc.gml.featureview.maker.FeatureviewHelper;

/**
 * @author Gernot Belger
 */
public class FlowRelCalcSimulationPage extends WizardPage implements IWizardPage
{
  private static final String URN_QRESULT_GFT = "urn:ogc:gml:featuretype:org.kalypso.model.wspm.tuhh:QIntervallResult:featureview:default"; //$NON-NLS-1$

  protected static final String SETTING_SASH_LEFT = "settingsSimulationSashLeft"; //$NON-NLS-1$

  protected static final String SETTING_SASH_RIGHT = "settingsSimulationSashRight"; //$NON-NLS-1$

  private Runnable m_refreshConsoleRunnable = null;

  private static final class TextConsoleViewerExtension extends TextConsoleViewer
  {
    public TextConsoleViewerExtension( final Composite parent, final TextConsole console )
    {
      super( parent, console );
    }

    /**
     * @see org.eclipse.jface.text.source.SourceViewer#createControl(org.eclipse.swt.widgets.Composite, int)
     */
    @Override
    protected void createControl( final Composite parentControl, final int styles )
    {
      super.createControl( parentControl, styles | SWT.BORDER );
    }

    /**
     * Overwritten in order to make public.
     * @see org.eclipse.ui.console.TextConsoleViewer#revealEndOfDocument()
     */
    @Override
    public void revealEndOfDocument( )
    {
      super.revealEndOfDocument();
    }
  }

  private final List<FlowRelationshipCalcOperation> m_operations = new ArrayList<FlowRelationshipCalcOperation>();

  private StatusComposite m_statusComposite;

  private IOConsole m_console;

  private IDocumentListener m_documentListener;

  private DefaultTableViewer m_resultTableViewer;

  private IOConsoleOutputStream m_consoleOS;

  private boolean m_simulationWasRun = false;

  public FlowRelCalcSimulationPage( final String pageName )
  {
    super( pageName );
  }

  public FlowRelCalcSimulationPage( final String pageName, final String title, final ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );

    m_console = new IOConsole( "ioConsole", null ); //$NON-NLS-1$
    m_consoleOS = m_console.newOutputStream();
  }

  /**
   * @see org.eclipse.jface.dialogs.DialogPage#dispose()
   */
  @Override
  public void dispose( )
  {
    super.dispose();

    IOUtils.closeQuietly( m_consoleOS );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    final Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout() );

    m_statusComposite = new StatusComposite( composite, StatusComposite.DETAILS );
    m_statusComposite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final SashForm resultSash = new SashForm( composite, SWT.HORIZONTAL | SWT.SMOOTH );
    resultSash.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    // Extra composite needed in order to let table be smaller than max-widht
    final Composite tableComposite = new Composite( resultSash, SWT.NONE );
    final GridLayout tableCompLayout = new GridLayout();
    tableCompLayout.marginHeight = 0;
    tableCompLayout.marginWidth = 0;
    tableComposite.setLayout( tableCompLayout );

    m_resultTableViewer = new DefaultTableViewer( tableComposite, SWT.BORDER | SWT.SINGLE | SWT.FULL_SELECTION | SWT.H_SCROLL );
    m_resultTableViewer.setContentProvider( new ArrayContentProvider() );

    StatusLabelProvider.addSeverityColumn( m_resultTableViewer );
    StatusLabelProvider.addMessageColumn( m_resultTableViewer );
    StatusLabelProvider.addTimeColumn( m_resultTableViewer );

    m_resultTableViewer.setInput( m_operations );
    final GridData tableData = new GridData( SWT.FILL, SWT.FILL, true, true );
    tableData.widthHint = 250;
    m_resultTableViewer.getTable().setLayoutData( tableData );

    /* Right sash part */
    final TabFolder tabFolder = new TabFolder( resultSash, SWT.NONE );
    // m_tabFolder.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    final Composite consoleComposite = new Composite( tabFolder, SWT.NONE );
    final StackLayout consoleStackLayout = new StackLayout();
    consoleComposite.setLayout( consoleStackLayout );

    final TextConsoleViewerExtension textConsoleViewer = new TextConsoleViewerExtension( consoleComposite, m_console );

    final Text resultText = new Text( consoleComposite, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL );
    resultText.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    resultText.setFont( m_console.getFont() );

    final Composite featurePanel = new Composite( tabFolder, SWT.NONE );
    featurePanel.setLayout( new GridLayout() );

    final CachedFeatureviewFactory featureviewFactory = new CachedFeatureviewFactory( new FeatureviewHelper() );
    try
    {
      final String configurationUrl = KalypsoCorePlugin.getDefault().getCatalogManager().getBaseCatalog().resolve( URN_QRESULT_GFT, URN_QRESULT_GFT );
      featureviewFactory.addView( new URL( configurationUrl ) );
    }
    catch( final MalformedURLException e2 )
    {
      e2.printStackTrace();
    }

    final FeatureComposite featureComposite = new FeatureComposite( null, null, featureviewFactory );

    final TabItem tableTab = new TabItem( tabFolder, SWT.NONE );
    tableTab.setControl( consoleComposite );
    tableTab.setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelCalcSimulationPage.4") ); //$NON-NLS-1$

    final TabItem resultTab = new TabItem( tabFolder, SWT.NONE );
    resultTab.setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelCalcSimulationPage.5") ); //$NON-NLS-1$
    resultTab.setControl( featurePanel );

    m_resultTableViewer.addOpenListener( new IOpenListener()
    {
      @Override
      public void open( final OpenEvent event )
      {
        final FlowRelationshipCalcOperation op = (FlowRelationshipCalcOperation) ((IStructuredSelection) event.getSelection()).getFirstElement();
        if( op != null )
          new StatusDialog( parent.getShell(), op.getStatus(), Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelCalcSimulationPage.6") + op.getFlowRelation1D().getName() ).open(); //$NON-NLS-1$
      }
    } );

    final DefaultTableViewer resultTableViewer = m_resultTableViewer;
    final Runnable refreshConsoleRunnable = new Runnable()
    {
      @Override
      public void run( )
      {
        final IStructuredSelection selection = (IStructuredSelection) resultTableViewer.getSelection();
        final FlowRelationshipCalcOperation op = (FlowRelationshipCalcOperation) (selection).getFirstElement();
        if( op == null )
        {
          resultText.setText( "" ); //$NON-NLS-1$
          return;
        }

        featureComposite.disposeControl();

        if( op.isRunning() )
          consoleStackLayout.topControl = textConsoleViewer.getControl();
        else
        {
          consoleStackLayout.topControl = resultText;

          final String text = op.getConsoleText();
          if( text == null )
            resultText.setText( op.getStatus().getMessage() );
          else
            resultText.setText( op.getConsoleText() );

          // Scroll to bottom of text, as this is the intersting part
          final int lineCount = resultText.getLineCount();
          resultText.setTopIndex( lineCount - 1 );

          final QIntervallResult result = op.getResult();
          if( result != null )
          {
            featureComposite.setFeature( result.getFeature() );
            featureComposite.createControl( featurePanel, SWT.NONE );
          }
        }

        featurePanel.layout( true, true );
        consoleComposite.layout();

        resultTableViewer.refresh();
      }
    };
    m_refreshConsoleRunnable = refreshConsoleRunnable;

    m_resultTableViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        refreshConsoleRunnable.run();
      }
    } );

    m_documentListener = new IDocumentListener()
    {
      @Override
      public void documentAboutToBeChanged( final DocumentEvent event )
      {
      }

      @Override
      public void documentChanged( final DocumentEvent event )
      {
        textConsoleViewer.revealEndOfDocument();
      }
    };

    int weightLeft = 50;
    int weightRight = 50;
    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings != null )
    {
      try
      {
        weightLeft = dialogSettings.getInt( SETTING_SASH_LEFT );
        weightRight = dialogSettings.getInt( SETTING_SASH_RIGHT );
      }
      catch( final NumberFormatException e1 )
      {
        // ignore, use defaults
      }
    }

    resultSash.setWeights( new int[] { weightLeft, weightRight } );

    resultText.addControlListener( new ControlAdapter()
    {
      /**
       * @see org.eclipse.swt.events.ControlAdapter#controlResized(org.eclipse.swt.events.ControlEvent)
       */
      @Override
      public void controlResized( final ControlEvent e )
      {
        final int[] weights = resultSash.getWeights();
        if( dialogSettings != null )
        {
          dialogSettings.put( SETTING_SASH_LEFT, weights[0] );
          dialogSettings.put( SETTING_SASH_RIGHT, weights[1] );
        }
      }
    } );

    setControl( composite );
  }

  public boolean simulationWasRun( )
  {
    return m_simulationWasRun;
  }

  public void runSimulation( )
  {
    m_console.getDocument().addDocumentListener( m_documentListener );

    m_statusComposite.setStatus( StatusUtilities.createStatus( IStatus.INFO, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelCalcSimulationPage.8"), null ) ); //$NON-NLS-1$

    final List<FlowRelationshipCalcOperation> operations = m_operations;
    final IOConsole console = m_console;
    final Runnable refreshConsoleRunnable = m_refreshConsoleRunnable;

    if( m_operations.size() > 0 )
      m_resultTableViewer.setSelection( new StructuredSelection( m_operations.get( 0 ) ), true );

    final ICoreRunnableWithProgress runnable = new ICoreRunnableWithProgress()
    {
      @Override
      public IStatus execute( final IProgressMonitor monitor ) throws CoreException
      {
        final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelCalcSimulationPage.9"), operations.size() ); //$NON-NLS-1$

        final String pluginId = PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() );
        final MultiStatus multiStatus = new MultiStatus( pluginId, -1, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelCalcSimulationPage.10"), null ); //$NON-NLS-1$

        for( final FlowRelationshipCalcOperation op : operations )
        {
          progress.subTask( op.getFlowRelation1D().getName() );

          final SubMonitor childProgress = progress.newChild( 1 );
          getShell().getDisplay().asyncExec( refreshConsoleRunnable );
          // works because its running asynchronously, so the runnable gets called when the op already is running
          op.execute( childProgress );

          final IStatus opStatus = op.getStatus();
          multiStatus.add( opStatus );

          console.clearConsole();

          if( opStatus.matches( IStatus.CANCEL ) )
            throw new CoreException( opStatus );

          getShell().getDisplay().asyncExec( refreshConsoleRunnable );
        }

        /* Create nice message for multi-status */
        final String message;
        if( multiStatus.isOK() || multiStatus.matches( IStatus.INFO ) )
          message = Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelCalcSimulationPage.11"); //$NON-NLS-1$
        else if( multiStatus.matches( IStatus.WARNING ) )
          message = Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelCalcSimulationPage.12"); //$NON-NLS-1$
        else if( multiStatus.matches( IStatus.ERROR ) )
          message = Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelCalcSimulationPage.13"); //$NON-NLS-1$
        else
          message = Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelCalcSimulationPage.14"); //$NON-NLS-1$
        return new MultiStatus( pluginId, -1, multiStatus.getChildren(), message, null );
      }
    };

    final IStatus status;
    final IWizardContainer container = getContainer();
    if( container instanceof WizardDialog2 )
      status = ((WizardDialog2) container).executeUnblocked( true, false, runnable );
    else
      status = RunnableContextHelper.execute( getContainer(), true, true, runnable );

    m_console.getDocument().removeDocumentListener( m_documentListener );
    m_statusComposite.setStatus( status );
    m_simulationWasRun = true;
  }

  public IStatus getStatus( )
  {
    return m_statusComposite.getStatus();
  }

  public IStatus applyResults( )
  {
    try
    {
      for( final FlowRelationshipCalcOperation op : m_operations )
        op.applyResult();

      // Post an empty command to flowrelationship model in order to make it dirty
      KalypsoAFGUIFrameworkPlugin.getDefault().getDataProvider().postCommand( IFlowRelationshipModel.class, new EmptyCommand( "", false ) ); //$NON-NLS-1$

      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
      return status;
    }
  }

  public void reset( final TuhhCalculation templateCalculation, final IFlowRelation1D[] flowRels, final IFlowRelationshipModel flowModel, final IFEDiscretisationModel1d2d discModel )
  {
    m_simulationWasRun = false;
    m_statusComposite.setStatus( StatusUtilities.createStatus( IStatus.INFO, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelCalcSimulationPage.16"), null ) ); //$NON-NLS-1$
    m_console.clearConsole();
    m_operations.clear();

    final IWizardContainer container = getContainer();
    if( container instanceof WizardDialog2 )
      ((WizardDialog2) container).getButton( IDialogConstants.FINISH_ID ).setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelCalcSimulationPage.17") ); //$NON-NLS-1$

    for( final IFlowRelation1D flowRel : flowRels )
    {
      final FlowRelationshipCalcOperation op = new FlowRelationshipCalcOperation( templateCalculation, flowRel, flowModel, discModel, m_consoleOS );
      m_operations.add( op );
    }

    m_resultTableViewer.refresh();
    m_resultTableViewer.setSelection( StructuredSelection.EMPTY );
  }

}
