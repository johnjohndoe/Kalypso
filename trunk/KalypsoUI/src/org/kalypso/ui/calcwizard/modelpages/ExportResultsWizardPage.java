package org.kalypso.ui.calcwizard.modelpages;

import java.io.File;
import java.io.FileOutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.feature.event.ModellEventListener;
import org.deegree_impl.model.feature.visitors.GetSelectionVisitor;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.dialogs.ListSelectionDialog;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.featureview.modfier.StringModifier;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.util.FeatureLabelProvider;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.ObservationTemplateHelper;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.util.url.UrlResolver;

/**
 * @author Belger
 */
public class ExportResultsWizardPage extends AbstractCalcWizardPage implements
    ModellEventListener
{
  // Beispiel:
  //   <page className="org.kalypso.ui.calcwizard.ViewResultsWizardPage"
  // pageTitle="Kontrolle der Ergebnisse"
  // imageLocation="icons/calcwizard/boden.gif" >
  //        <arg name="mapTemplate" value=".modellTyp/vorlagen/rechenfall/karte2.gmt"/>
  //        <arg name="tableTemplate"
  // value=".modellTyp/vorlagen/rechenfall/table2.gtt"/>
  //        <arg name="timeseriesPropertyNames"
  // value="Wasserstand#Wasserstand_gerechnet"/>
  //        <arg name="mainSash" value="50"/>
  //        <arg name="rightSash" value="40"/>
  //        <arg name="grafikToolTemplate" value=".modellTyp/grafik.exe_"/>
  //    </page>
  //  

  /** Position des Haupt-Sash: Integer von 0 bis 100 */
  private final static String PROP_MAINSASH = "mainSash";

  /** Position des rechten Sash: Integer von 0 bis 100 */
  private final static String PROP_RIGHTSASH = "rightSash";

  /** Pfad auf die Vorlage für das Diagramm (.odt Datei) */
  private final static String PROP_DIAGTEMPLATE = "diagTemplate";

  protected ObsdiagviewType m_obsdiagviewType;

  private List m_calcCaseFolder = new ArrayList();

  private CheckboxTableViewer m_checklist;

  private static final String PROP_RESULT_TS_NAME = "resultProperty";

  private static final String PROP_PROGNOSE_TS_NAME = "prognoseProperty";

  private static final String PROP_PEGEL_NAME = "pegelNameProperty";

  public ExportResultsWizardPage( )
  {
    super( "<ViewResultsWizardPage>" );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#dispose()
   */
  public void dispose( )
  {
    super.dispose();
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    try
    {
      final SashForm sashForm = new SashForm( parent, SWT.HORIZONTAL );
      createMapPanel( sashForm );
      final SashForm rightSash = new SashForm( sashForm, SWT.VERTICAL );

      createExportPanel( rightSash );
      createDiagramPanel( rightSash );

      final int mainWeight = Integer.parseInt( getArguments().getProperty(
          PROP_MAINSASH, "50" ) );
      final int rightWeight = Integer.parseInt( getArguments().getProperty(
          PROP_RIGHTSASH, "50" ) );

      sashForm.setWeights( new int[] { mainWeight, 100 - mainWeight } );

      rightSash.setWeights( new int[] { rightWeight, 100 - rightWeight } );

      rightSash.addControlListener( getControlAdapter() );
      sashForm.addControlListener( getControlAdapter() );

      setControl( sashForm );

      // Load Template for Grafix.exe
      final String diagFileName = getArguments()
          .getProperty( PROP_DIAGTEMPLATE );
      final IFile diagFile = (IFile) getProject().findMember( diagFileName );
      try
      {
        m_obsdiagviewType = ObservationTemplateHelper
            .loadDiagramTemplateXML( diagFile.getContents() );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  private void createDiagramPanel( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout() );

    final Control diag = initDiagram( panel );
    diag.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    final Button button = new Button( panel, SWT.PUSH );
    button.setText( "Zeitreihe(n) bearbeiten" );
    button
        .setToolTipText( "Öffnet die im Diagram dargestellten Zeitreihen zur Bearbeitung" );
    button.addSelectionListener( new GraficToolStarter() );
  }

  private void createExportPanel( final Composite parent )
  {
    // noch einen ListViewer einfügen!
    final Composite topPanel = new Composite( parent, SWT.NONE );
    topPanel.setLayout( new GridLayout( 2, false ) );

    m_checklist = CheckboxTableViewer.newCheckList( topPanel, SWT.BORDER );
    m_checklist.getControl().setLayoutData( new GridData( GridData.FILL_BOTH ) );
    m_checklist.setContentProvider( new ArrayContentProvider() );
    m_checklist.setLabelProvider( new WorkbenchLabelProvider() );
    m_checklist.setInput( m_calcCaseFolder );

    m_checklist.addCheckStateListener( new ICheckStateListener()
    {
      public void checkStateChanged( CheckStateChangedEvent event )
      {
        refreshTimeseries();
      }
    } );

    final Button exportPrognoseTS = new Button( topPanel, SWT.PUSH );
    exportPrognoseTS.setText( "Export Prognosen" );
    exportPrognoseTS
        .setToolTipText( "Exportiert die Prognosen des selektierten Rechenfalls" );
    final GridData buttonGridData = new GridData();
    buttonGridData.verticalAlignment = GridData.VERTICAL_ALIGN_BEGINNING;
    exportPrognoseTS.setLayoutData( buttonGridData );
    exportPrognoseTS.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      public void widgetSelected( SelectionEvent e )
      {
        exportPrognoseTimeseries();
      }
    } );

    final Group exportGroup = new Group( topPanel, SWT.NONE );
    exportGroup.setText( "Berichtsablage" );
    exportGroup.setLayout( new GridLayout() );
    exportGroup.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    final Button exportQDiagramm = new Button( exportGroup, SWT.CHECK );
    exportQDiagramm.setText( "Durchflussgrafik" );

    final Button exportWRadio = new Button( exportGroup, SWT.CHECK );
    exportWRadio.setText( "Wasserstandsgrafik" );

    final Button exportTableRadio = new Button( exportGroup, SWT.CHECK );
    exportTableRadio.setText( "Tabelle" );

    final Button exportMap = new Button( exportGroup, SWT.CHECK );
    exportMap.setText( "Kartenansicht" );

    final Button doItButton = new Button( topPanel, SWT.PUSH );
    doItButton.setText( "Bericht(e) ablegen" );
    doItButton
        .setToolTipText( "Legt für alle aktivierten Rechenfälle und alle selektierten Pegel die ausgewählten Berichte ab." );
    doItButton.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      public void widgetSelected( SelectionEvent e )
      {
        exportSelectedDocuments();
      }
    } );
  }

  /**
   * Exports for all selected document types
   */
  protected void exportSelectedDocuments( )
  {
    //    final List selectedFeatures = getSelectedFeatures( false );
    //    final Object[] checkedCalcCases = getCheckedCalcCases();

    // welche exporte

    // doit
  }

  /**
   * Allows user to export selected timeseries into repository. Handles UI
   * selection and delegates call to performPrognoseExport.
   */
  protected void exportPrognoseTimeseries( )
  {
    // Timeserie-Links holen
    final List features = getFeatures( false );
    final List selectedFeatures = getSelectedFeatures( false );
    final IFolder selectedCalcCase = getSelectedCalcCase();

    // view it!
    final String nameProperty = getArguments().getProperty( PROP_PEGEL_NAME );
    final FeatureType featureType = ((IKalypsoFeatureTheme) getMapModell()
        .getActiveTheme()).getFeatureType();
    final FeatureTypeProperty ftp = featureType.getProperty( nameProperty );
    final ILabelProvider labelProvider = new FeatureLabelProvider(
        new StringModifier( ftp ) );
    final ListSelectionDialog dialog = new ListSelectionDialog( getContainer()
        .getShell(), features, new ArrayContentProvider(), labelProvider,
        "Bitte wählen Sie diejenigen Pegel, deren Zeitreihen exportiert werden sollen:" );
    dialog.setInitialElementSelections( selectedFeatures );
    dialog.setTitle( "Export Prognose-Zeitreihen: Rechenfall "
        + selectedCalcCase.getName() );
    if( dialog.open() != Window.OK )
      return;

    final String resultTsName = getArguments()
        .getProperty( PROP_RESULT_TS_NAME );
    final String prognoseTsName = getArguments().getProperty(
        PROP_PROGNOSE_TS_NAME );

    // TODO: eventuell noch mal filtern (letztes argument != null)
    final TSLinkWithName[] resultTss = getTimeseriesForProperty( "", Arrays
        .asList( dialog.getResult() ), resultTsName, null );
    final TSLinkWithName[] prognoseTss = getTimeseriesForProperty( "", Arrays
        .asList( dialog.getResult() ), prognoseTsName, null );

    try
    {
      final URL context = ResourceUtilities.createURL( selectedCalcCase );

      final WorkspaceModifyOperation op = new WorkspaceModifyOperation()
      {
        protected void execute( IProgressMonitor monitor )
        {
          performPrognoseExport( resultTss, prognoseTss, context, monitor );
        }
      };

      getContainer().run( false, true, op );
    }
    catch( final Exception e )
    {
      // TODO handling
      e.printStackTrace();
    }
  }

  /**
   * Performs the timeseries export
   * 
   * @param resultTss
   * @param prognoseTss
   * @param context
   * @param monitor
   */
  protected void performPrognoseExport( final TSLinkWithName[] resultTss,
      final TSLinkWithName[] prognoseTss, final URL context,
      final IProgressMonitor monitor )
  {
    if( resultTss.length != prognoseTss.length )
      throw new IllegalArgumentException( "Timeseries links not same length" );

    try
    {
      monitor.beginTask( "Prognosen zurück speichern", prognoseTss.length );

      final UrlResolver resolver = new UrlResolver();

      for( int i = 0; i < prognoseTss.length; i++ )
      {
        if( monitor.isCanceled() )
          return;

        final TSLinkWithName lnkRS = resultTss[i];
        final TSLinkWithName lnkPG = prognoseTss[i];

        try
        {
          final URL urlRS = resolver.resolveURL( context, lnkRS.href );
          final IObservation source = ZmlFactory.parseXML( urlRS, lnkRS.href );

          final URL urlPG = resolver.resolveURL( context, lnkPG.href );
          final IObservation dest = ZmlFactory.parseXML( urlPG, lnkPG.href );

          // let's hope that it works
          dest.setValues( source.getValues( null ) );
        }
        catch( MalformedURLException e )
        {
          e.printStackTrace();
        }
        catch( SensorException e )
        {
          e.printStackTrace();
        }

        monitor.worked( 1 );
      }
    }
    finally
    {
      monitor.done();
    }
  }

  private IFolder getSelectedCalcCase( )
  {
    if( m_checklist == null )
      return null;

    final CheckboxTableViewer checklist = m_checklist;
    final Control checkControl = checklist.getControl();
    if( checkControl == null || checkControl.isDisposed() )
      return null;

    final CheckListGetter getter = new CheckListGetter( m_checklist );
    checkControl.getDisplay().syncExec( getter );
    return getter.getSelected();
  }

  private void createMapPanel( final Composite parent ) throws Exception,
      CoreException
  {
    final Composite mapPanel = new Composite( parent, SWT.NONE );
    mapPanel.setLayout( new GridLayout() );

    final Control mapControl = initMap( mapPanel, MapPanel.WIDGET_TOGGLE_SELECT );
    mapControl.setLayoutData( new GridData( GridData.FILL_BOTH ) );
  }

  /**
   * @see org.kalypso.ui.calcwizard.modelpages.IModelWizardPage#performFinish()
   */
  public boolean performFinish( )
  {
    return true;
  }

  private class GraficToolStarter implements SelectionListener
  {
    // TODO: use getObservations instead
    // and call it with template and those obs'es

    /**
     * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
     */
    public void widgetSelected( SelectionEvent e )
    {
      final IKalypsoTheme theme = getMapModell().getActiveTheme();
      if( !(theme instanceof IKalypsoFeatureTheme) )
        return;

      m_obsdiagviewType.getObservation().clear();

      FileOutputStream fos = null;

      try
      {
        final IKalypsoFeatureTheme kft = (IKalypsoFeatureTheme) theme;

        final List selectedFeatures = GetSelectionVisitor.getSelectedFeatures(
            kft.getWorkspace(), kft.getFeatureType(), getSelectionID() );

        // TODO: wants tsProps instead of null
        if( selectedFeatures.size() > 0 )
          KalypsoWizardHelper.updateXMLDiagramTemplate( null, selectedFeatures,
              m_obsdiagviewType );

        // create tmp odt template
        final File file = File.createTempFile( "diag", ".odt" );
        file.deleteOnExit();

        fos = new FileOutputStream( file );
        ObservationTemplateHelper.saveDiagramTemplateXML( m_obsdiagviewType,
            fos );

        ObservationTemplateHelper.openGrafik4odt( file, getProject() );

        file.delete();
      }
      catch( Exception ex )
      {
        ex.printStackTrace();
      }
      finally
      {
        IOUtils.closeQuietly( fos );
      }
    }

    /**
     * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
     */
    public void widgetDefaultSelected( SelectionEvent e )
    {
      // empty
    }
  }

  /**
   * Überschrieben, da wir das gleiche für mehrere contexte = mehrere
   * Rechenfälle ausführen
   */
  public void refreshTimeseries( )
  {
    // erstmal leer, damit das Diagramm gelöscht wird
    refreshObservationsForContext( new TSLinkWithName[] {}, getContext() );

    final Object[] checkedCalcCases = getCheckedCalcCases();
    if( checkedCalcCases == null || checkedCalcCases.length == 0 )
      return;

    // todo: ist ein kleiner hack, der davon ausgeht, dass das modell sich nie
    // ändern wird
    // es werden einfach die links vom aktuellen Modell gegen alle
    // selektierten Rechenfälle aufgelöst
    final TSLinkWithName[] obs = getObservationsToShow();

    for( int i = 0; i < checkedCalcCases.length; i++ )
    {
      try
      {
        final IFolder calcCase = (IFolder) checkedCalcCases[i];
        final URL context = ResourceUtilities.createURL( calcCase );
        refreshObservationsForContext( obs, context );
      }
      catch( final MalformedURLException e )
      {
        e.printStackTrace();
      }
    }
  }

  private Object[] getCheckedCalcCases( )
  {
    if( m_checklist == null )
      return new Object[] {};

    final CheckboxTableViewer checklist = m_checklist;
    final Control checkControl = checklist.getControl();
    if( checkControl == null || checkControl.isDisposed() )
      return new Object[] {};

    final CheckListGetter getter = new CheckListGetter( m_checklist );
    checkControl.getDisplay().syncExec( getter );
    return getter.getResults();
  }

  /**
   * @see org.kalypso.ui.calcwizard.modelpages.AbstractCalcWizardPage#getObservationsToShow()
   */
  protected TSLinkWithName[] getObservationsToShow( )
  {
    return getObservationsFromMap( false );
  }

  public void setCalcCaseFolder( final Collection folders )
  {
    m_calcCaseFolder.clear();
    m_calcCaseFolder.addAll( folders );

    final Viewer viewer = m_checklist;
    if( viewer != null )
    {
      final Control control = m_checklist.getControl();
      if( control != null && !control.isDisposed() )
      {
        control.getDisplay().asyncExec( new Runnable()
        {
          public void run( )
          {
            viewer.refresh();
          }
        } );
      }
    }
  }

  private class CheckListGetter implements Runnable
  {
    private Object[] m_results;

    private CheckboxTableViewer m_cl;

    private IFolder m_selected;

    public CheckListGetter( final CheckboxTableViewer ctv )
    {
      m_cl = ctv;
    }

    /**
     * @return Returns the selected.
     */
    public IFolder getSelected( )
    {
      return m_selected;
    }

    public Object[] getResults( )
    {
      return m_results;
    }

    public void run( )
    {
      m_results = m_cl.getCheckedElements();
      final IStructuredSelection selection = (IStructuredSelection) m_cl
          .getSelection();

      m_selected = (IFolder) (selection.isEmpty() ? null : selection
          .getFirstElement());
    }
  }

}