package org.kalypso.ui.calcwizard.modelpages;

import java.io.File;
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.deegree_impl.model.feature.visitors.GetSelectionVisitor;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckboxTableViewer;
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

//      rightSash.setWeights( new int[]
//      {
//          rightWeight,
//          100 - rightWeight } );

      rightSash.addControlListener( getControlAdapter() );
      sashForm.addControlListener( getControlAdapter() );

      setControl( sashForm );

      // Load Template for Grafix.exe
      final String diagFileName = getArguments().getProperty( PROP_DIAGTEMPLATE );
      final IFile diagFile = (IFile)getProject().findMember( diagFileName );
      try
      {
        m_obsdiagviewType = ObservationTemplateHelper.loadDiagramTemplateXML( diagFile.getContents() );
      }
      catch( Exception e )
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
    final Composite leftHalf = new Composite( parent, SWT.NONE );
    leftHalf.setLayout( new GridLayout( 2, false ) );

    m_checklist = CheckboxTableViewer.newCheckList( leftHalf, SWT.BORDER );
    m_checklist.setContentProvider( new ArrayContentProvider() );
    m_checklist.setLabelProvider( new WorkbenchLabelProvider() );
    m_checklist.setInput( m_calcCaseFolder );

    final Composite rightHalf = new Composite( parent, SWT.NONE );
    rightHalf.setLayout( new GridLayout(  ) );

    final Button button = new Button( rightHalf, SWT.PUSH );
    button.setText( "Zeitreihe bearbeiten" );
    button.setToolTipText( "Öffnet die die selektierten Zeitreihen zur Bearbeitung" );
    button.addSelectionListener( new GraficToolStarter() );
    button.setLayoutData( new GridData(  ) );

    final Group exportGroup = new Group( rightHalf, SWT.NONE );
    exportGroup.setText( "Berichtsablage" );
    exportGroup.setLayout( new GridLayout() );
    
    final Button exportQDiagramm = new Button( exportGroup, SWT.CHECK );
    exportQDiagramm.setText( "Durchflussgrafik" );

    final Button exportWRadio = new Button( exportGroup, SWT.CHECK );
    exportWRadio.setText( "Wasserstandsgrafik" );

    final Button exportTableRadio = new Button( exportGroup, SWT.CHECK );
    exportTableRadio.setText( "Tabelle" );

    final Button exportMap = new Button( exportGroup, SWT.CHECK );
    exportMap.setText( "Kartenansicht" );
    
    final Button doItButton = new Button( exportGroup, SWT.PUSH );
    doItButton.setText( "Bericht(e) ablegen" );
    doItButton.setToolTipText( "Legt für alle aktivierten Rechenfälle und alle selektierten Pegel die ausgewählten Berichte ab." );
  }

  private void createMapPanel( final Composite parent ) throws Exception, CoreException
  {
    final Composite mapPanel = new Composite( parent, SWT.NONE );
    mapPanel.setLayout( new GridLayout() );

    final Control mapControl = initMap( mapPanel, MapPanel.WIDGET_SELECT );
    mapControl.setLayoutData( new GridData( GridData.FILL_BOTH ) );
  }

  /**
   * @see org.kalypso.ui.calcwizard.modelpages.IModelWizardPage#performFinish()
   */
  public boolean performFinish()
  {
    return true;
  }

  /**
   * @see org.deegree.model.feature.event.ModellEventListener#onModellChange(org.deegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
  //
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
            kft.getFeatureType(), SELECTION_ID );

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
   * @see org.kalypso.ui.calcwizard.modelpages.AbstractCalcWizardPage#getObservationsToShow()
   */
  protected TSLinkWithName[] getObservationsToShow()
  {
    // TODO: implement it!

    return new TSLinkWithName[] {};
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
}