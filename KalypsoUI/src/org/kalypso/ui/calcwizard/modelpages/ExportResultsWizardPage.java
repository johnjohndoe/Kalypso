package org.kalypso.ui.calcwizard.modelpages;

import java.io.File;
import java.io.FileOutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.deegree.model.feature.event.ModellEventListener;
import org.deegree_impl.model.feature.visitors.GetSelectionVisitor;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.sensor.diagview.ObservationTemplateHelper;
import org.kalypso.template.obsdiagview.ObsdiagviewType;

/**
 * @author Belger
 */
public class ExportResultsWizardPage extends AbstractCalcWizardPage implements ModellEventListener
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

  public ExportResultsWizardPage()
  {
    super( "<ViewResultsWizardPage>" );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#dispose()
   */
  public void dispose()
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
      initDiagram( rightSash );

      final int mainWeight = Integer.parseInt( getArguments().getProperty( PROP_MAINSASH, "50" ) );
      final int rightWeight = Integer.parseInt( getArguments().getProperty( PROP_RIGHTSASH, "50" ) );

      sashForm.setWeights( new int[]
      {
          mainWeight,
          100 - mainWeight } );

      rightSash.setWeights( new int[]
      {
          rightWeight,
          100 - rightWeight } );

      rightSash.addControlListener( getControlAdapter() );
      sashForm.addControlListener( getControlAdapter() );

      setControl( sashForm );

      // Load Template for Grafix.exe
      final String diagFileName = getArguments().getProperty( PROP_DIAGTEMPLATE );
      final IFile diagFile = (IFile)getProject().findMember( diagFileName );
      try
      {
        m_obsdiagviewType = ObservationTemplateHelper.loadDiagramTemplateXML( diagFile
            .getContents() );
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
    
    m_checklist.addCheckStateListener( new ICheckStateListener() {
      public void checkStateChanged( CheckStateChangedEvent event )
      {
        refreshTimeseries();
      }} );

    final Button button = new Button( topPanel, SWT.PUSH );
    button.setText( "Zeitreihe(n) bearbeiten" );
    button.setToolTipText( "Öffnet die die selektierten Zeitreihen zur Bearbeitung" );
    button.addSelectionListener( new GraficToolStarter() );
    final GridData buttonGridData = new GridData();
    buttonGridData.verticalAlignment = GridData.VERTICAL_ALIGN_BEGINNING;
    button.setLayoutData( buttonGridData );

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

    final Button exportPrognoseTS = new Button( exportGroup, SWT.CHECK );
    exportPrognoseTS.setText( "Prognose Zeitreihen" );

    final Button exportMap = new Button( exportGroup, SWT.CHECK );
    exportMap.setText( "Kartenansicht" );

    final Button doItButton = new Button( topPanel, SWT.PUSH );
    doItButton.setText( "Bericht(e) ablegen" );
    doItButton
        .setToolTipText( "Legt für alle aktivierten Rechenfälle und alle selektierten Pegel die ausgewählten Berichte ab." );
  }

  private void createMapPanel( final Composite parent ) throws Exception, CoreException
  {
    final Composite mapPanel = new Composite( parent, SWT.NONE );
    mapPanel.setLayout( new GridLayout() );

    final Control mapControl = initMap( mapPanel, MapPanel.WIDGET_SINGLE_SELECT );
    mapControl.setLayoutData( new GridData( GridData.FILL_BOTH ) );
  }

  /**
   * @see org.kalypso.ui.calcwizard.modelpages.IModelWizardPage#performFinish()
   */
  public boolean performFinish()
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
      if( !( theme instanceof IKalypsoFeatureTheme ) )
        return;

      m_obsdiagviewType.getObservation().clear();

      FileOutputStream fos = null;

      try
      {
        final IKalypsoFeatureTheme kft = (IKalypsoFeatureTheme)theme;

        final List selectedFeatures = GetSelectionVisitor.getSelectedFeatures( kft.getWorkspace(),
            kft.getFeatureType(), getSelectionID() );

        // TODO: wants tsProps instead of null
        if( selectedFeatures.size() > 0 )
          KalypsoWizardHelper.updateXMLDiagramTemplate( null, selectedFeatures, m_obsdiagviewType );

        // create tmp odt template
        final File file = File.createTempFile( "diag", ".odt" );
        file.deleteOnExit();

        fos = new FileOutputStream( file );
        ObservationTemplateHelper.saveDiagramTemplateXML( m_obsdiagviewType, fos );

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
  public void refreshTimeseries()
  {
    // erstmal leer, damit das Diagramm gelöscht wird
    refreshObservationsForContext( new TSLinkWithName[] {}, getContext() );

    final Object[] checkedCalcCases = getCheckedCalcCases();
    if( checkedCalcCases == null || checkedCalcCases.length == 0 )
      return;

    // todo: ist ein kliner hack, der davon ausgeht, dass das modell sich nie
    // ändern wird
    // es werden einfach die links vom aktuellen Modell gegen alle
    // selektierten Rechenfälle aufgelöst
    final TSLinkWithName[] obs = getObservationsToShow();

    for( int i = 0; i < checkedCalcCases.length; i++ )
    {
      try
      {
        final IFolder calcCase = (IFolder)checkedCalcCases[i];
        final URL context = ResourceUtilities.createURL( calcCase );
        refreshObservationsForContext( obs, context );
      }
      catch( final MalformedURLException e )
      {
        e.printStackTrace();
      }
    }
  }

  private Object[] getCheckedCalcCases()
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
  protected TSLinkWithName[] getObservationsToShow()
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
          public void run()
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
    
    public CheckListGetter( final CheckboxTableViewer ctv )
    {
      m_cl = ctv;
    }
    
    public Object[] getResults()
    {
      return m_results;
    }

    public void run()
    {
      m_results = m_cl.getCheckedElements();
    }
  }

}