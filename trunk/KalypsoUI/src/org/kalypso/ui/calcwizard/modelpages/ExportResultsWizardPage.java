/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.calcwizard.modelpages;

import java.io.FileOutputStream;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Logger;

import javax.xml.bind.JAXBException;
import javax.xml.rpc.ServiceException;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.dialogs.ListSelectionDialog;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.eclipse.core.runtime.MultiStatus;
import org.kalypso.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.java.lang.reflect.ClassUtilityException;
import org.kalypso.metadoc.Document;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.featureview.modfier.StringModifier;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.util.FeatureLabelProvider;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagViewUtils;
import org.kalypso.ogc.sensor.diagview.grafik.GrafikLauncher;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.services.ocs.repository.ServiceRepositoryObservation;
import org.kalypso.services.proxy.IObservationService;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.calcwizard.Arguments;
import org.kalypso.ui.calcwizard.bericht.ExportWizardBerichtWizard;
import org.kalypso.ui.calcwizard.bericht.IBerichtExporter;
import org.kalypso.ui.metadoc.util.MultiDocumentServiceWrapper;
import org.kalypso.util.runtime.args.DateRangeArgument;
import org.kalypso.util.url.UrlResolver;
import org.kalypso.zml.obslink.TimeseriesLink;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * 
 * <p>
 * Unterst�tzte Argument:
 * </p>
 * <p>
 * exporterN: Namen von Klassen, welche
 * {@link org.kalypso.ui.calcwizard.bericht.IBerichtExporter}implementieren.
 * </p>
 * <p>
 * Aus diesen kann der Nutzer f�r den Brichtsexport ausw�hlen
 * </p>
 * 
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

  /** Pfad auf die Vorlage f�r das Diagramm (.odt Datei) */
  private final static String PROP_DIAGTEMPLATE = "diagTemplate";

  protected ObsdiagviewType m_obsdiagviewType;

  private List m_calcCaseFolder = new ArrayList();

  private CheckboxTableViewer m_checklist;

  private IBerichtExporter[] m_berichtExporters;

  private static final String PROP_RESULT_TS_NAME = "resultProperty";

  private static final String PROP_PROGNOSE_TS_NAME = "prognoseProperty";

  private static final String PROP_PEGEL_NAME = "pegelNameProperty";

  public ExportResultsWizardPage()
  {
    super( "<ViewResultsWizardPage>", SELECT_FROM_MAPVIEW );
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
      createDiagramPanel( rightSash );

      final int mainWeight = Integer.parseInt( getArguments().getProperty(
          PROP_MAINSASH, "50" ) );
      final int rightWeight = Integer.parseInt( getArguments().getProperty(
          PROP_RIGHTSASH, "50" ) );

      sashForm.setWeights( new int[] {
          mainWeight,
          100 - mainWeight } );

      rightSash.setWeights( new int[] {
          rightWeight,
          100 - rightWeight } );

      rightSash.addControlListener( getControlAdapter() );
      sashForm.addControlListener( getControlAdapter() );

      setControl( sashForm );

      postCreateControl();

      // Load Template for Grafix.exe
      final String diagFileName = getArguments()
          .getProperty( PROP_DIAGTEMPLATE );
      if( diagFileName != null )
      {
        final IFile diagFile = (IFile)getProject().findMember( diagFileName );
        try
        {
          m_obsdiagviewType = DiagViewUtils.loadDiagramTemplateXML( diagFile
              .getContents() );
        }
        catch( final Exception e )
        {
          e.printStackTrace();
        }
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

    final Composite buttonPanel = new Composite( panel, SWT.NONE );
    buttonPanel.setLayout( new GridLayout( 2, false ) );
    buttonPanel.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    final GridData ignoreData = new GridData( GridData.FILL_HORIZONTAL );
    ignoreData.horizontalAlignment = GridData.BEGINNING;
    createIgnoreButtonPanel( buttonPanel ).setLayoutData( ignoreData );

    final Button button = new Button( buttonPanel, SWT.PUSH );
    button.setText( "Zeitreihe(n) bearbeiten" );
    button
        .setToolTipText( "�ffnet die im Diagram dargestellten Zeitreihen zur Bearbeitung" );
    button.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      public void widgetSelected( SelectionEvent e )
      {
        startGrafik();
      }
    } );
  }

  private void createExportPanel( final Composite parent )
  {
    m_berichtExporters = createExporters();

    // noch einen ListViewer einf�gen!
    final Composite topPanel = new Composite( parent, SWT.NONE );
    topPanel.setLayout( new GridLayout( 2, false ) );

    m_checklist = CheckboxTableViewer.newCheckList( topPanel, SWT.BORDER );
    m_checklist.getControl().setLayoutData( new GridData( GridData.FILL_BOTH ) );
    m_checklist.setContentProvider( new ArrayContentProvider() );
    m_checklist.setLabelProvider( new WorkbenchLabelProvider() );
    m_checklist.setInput( m_calcCaseFolder );
    m_checklist.setSelection( new StructuredSelection( getCalcFolder() ), true );
    m_checklist.setChecked( getCalcFolder(), true );

    m_checklist.addCheckStateListener( new ICheckStateListener()
    {
      public void checkStateChanged( CheckStateChangedEvent event )
      {
        refreshDiagram();
      }
    } );

    final Composite buttonPanel = new Composite( topPanel, SWT.NONE );
    buttonPanel.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    buttonPanel.setLayout( new GridLayout() );

    final Button exportPrognoseTS = new Button( buttonPanel, SWT.PUSH );
    exportPrognoseTS.setText( "Export Prognosen" );
    exportPrognoseTS
        .setToolTipText( "Exportiert die Prognosen der selektierten Rechenvariante" );
    exportPrognoseTS.setLayoutData( new GridData() );
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

    final Button doItButton = new Button( buttonPanel, SWT.PUSH );
    doItButton.setLayoutData( new GridData() );
    doItButton.setText( "Bericht(e) ablegen" );
    doItButton
        .setToolTipText( "Legt f�r alle aktivierten Rechenf�lle Dokumente im Berichtswesen ab." );
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

    doItButton
        .setEnabled( !( m_berichtExporters == null || m_berichtExporters.length == 0 ) );
  }

  /**
   * Exports for all selected document types
   */
  protected void exportSelectedDocuments()
  {
    final IFolder selectedCalcCase = getSelectedCalcCase();

    final Shell shell = getContainer().getShell();
    if( selectedCalcCase == null )
    {
      MessageDialog.openWarning( shell, "Berichte exportieren",
          "Keine Rechenvariante selektiert" );
      return;
    }

    FeatureList features = getFeatures();
    List selectedFeatures = getSelectedFeatures();

    final String resultProperty = getArguments().getProperty(
        PROP_RESULT_TS_NAME );
    final URL context;
    try
    {
      context = ResourceUtilities.createURL( selectedCalcCase );
      features = FeatureFactory
          .createFeatureList(null,null, filterForValidTimeseriesLinks( features,
              resultProperty, context ) );
      selectedFeatures = filterForValidTimeseriesLinks( selectedFeatures,
          resultProperty, context );
    }
    catch( MalformedURLException e )
    {
      e.printStackTrace();
    }

    final String nameProperty = getArguments().getProperty( PROP_PEGEL_NAME );

    MultiDocumentServiceWrapper metadocService = null;
    try
    {
      metadocService = new MultiDocumentServiceWrapper();

      final ExportWizardBerichtWizard wizard = new ExportWizardBerichtWizard(
          features, selectedFeatures, nameProperty, metadocService
              .getDummyExportableDoc(), metadocService.getDummyDocument(),
          m_berichtExporters );
      final WizardDialog dialog = new WizardDialog( getContainer().getShell(),
          wizard );
      if( dialog.open() == Window.OK )
      {
        final Feature[] choosenFeatures = wizard.getChoosenFeatures();
        final IBerichtExporter[] choosenExporter = wizard.getChoosenExporter();

        final MultiDocumentServiceWrapper msw = metadocService;
        final RunnableContextHelper helper = new RunnableContextHelper(
            getContainer() )
        {
          public void run( final IProgressMonitor monitor )
              throws InvocationTargetException, InterruptedException
          {
            try
            {
              exportDocuments( monitor, msw, choosenFeatures, choosenExporter );
            }
            catch( final CoreException e )
            {
              throw new InvocationTargetException( e );
            }
          }
        };
        helper.runAndHandleOperation( shell, false, true, "Berichtsablage",
            "Fehler bei der Berichtsablage." );
      }
    }
    catch( final CoreException e )
    {
      e.printStackTrace();

      ErrorDialog.openError( shell, "Berichtsablage",
          "Berichtsablagedienst konnte nicht initialisiert werden.", e
              .getStatus() );
      return;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      metadocService.dispose();
    }
  }

  protected void exportDocuments( final IProgressMonitor monitor,
      final MultiDocumentServiceWrapper metadocService,
      final Feature[] features, final IBerichtExporter[] exporter )
      throws CoreException, InterruptedException
  {
    monitor.beginTask( "Berichtsablage", exporter.length * features.length );

    final Collection stati = new LinkedList();
    for( int i = 0; i < exporter.length; i++ )
    {
      final IBerichtExporter berichtExporter = exporter[i];
      final String extension = berichtExporter.getExtension();
      for( int j = 0; j < features.length; j++ )
      {
        monitor.setTaskName( "Berichtsablage - " + berichtExporter.toString() );

        final Document doc = metadocService.getCopyDocument( extension );
        OutputStream os = null;
        try
        {
          os = new FileOutputStream( doc.getFile() );
          final IStatus status = berichtExporter.export( features[j], os );
          if( !status.isOK() )
            stati.add( status );

          metadocService.commitDocument( doc );
        }
        catch( final CoreException ce )
        {
          stati.add( ce.getStatus() );
        }
        catch( final Exception e )
        {
          e.printStackTrace();

          doc.dispose();

          final String msg = e.getLocalizedMessage();
          final String message = msg == null ? "Unbekannter Fehler" : msg;

          stati.add( KalypsoGisPlugin.createErrorStatus( message, e ) );
        }
        finally
        {
          IOUtils.closeQuietly( os );
        }

        monitor.worked( 1 );
        if( monitor.isCanceled() )
          throw new InterruptedException();
      }
    }

    if( stati.isEmpty() )
      return;

    throw new CoreException( new org.eclipse.core.runtime.MultiStatus(
        KalypsoGisPlugin.getId(), 0, (IStatus[])stati
            .toArray( new IStatus[stati.size()] ),
        "Beim Export ein oder mehrerer Dokumente traten Fehler auf.", null ) );
  }

  private IBerichtExporter[] createExporters()
  {
    final Collection exporters = new ArrayList();

    final Arguments arguments = getArguments();
    for( final Iterator aIt = arguments.entrySet().iterator(); aIt.hasNext(); )
    {
      final Map.Entry entry = (Entry)aIt.next();
      final String key = (String)entry.getKey();
      if( key.startsWith( "exporter" ) )
      {
        try
        {
          // todo: maybe check if argument is ok?
          final Arguments args = (Arguments)entry.getValue();
          final String classname = args.getProperty( "class" );
          if( classname != null )
          {
            final IBerichtExporter exporter = (IBerichtExporter)ClassUtilities
                .newInstance( classname, IBerichtExporter.class, this
                    .getClass().getClassLoader() );
            exporter.init( getContext(), args );
            exporters.add( exporter );
          }
        }
        catch( final ClassUtilityException e )
        {
          e.printStackTrace();
        }
      }
    }

    return (IBerichtExporter[])exporters
        .toArray( new IBerichtExporter[exporters.size()] );
  }

  private List chooseSelectedFeatures( final IFolder calcCase )
  {
    // Timeserie-Links holen
    List features = getFeatures();
    List selectedFeatures = getSelectedFeatures();

    final String resultProperty = getArguments().getProperty(
        PROP_RESULT_TS_NAME );
    final URL context;
    try
    {
      context = ResourceUtilities.createURL( calcCase );
      features = filterForValidTimeseriesLinks( features, resultProperty,
          context );
      selectedFeatures = filterForValidTimeseriesLinks( selectedFeatures,
          resultProperty, context );
    }
    catch( MalformedURLException e )
    {
      return null;
    }

    // view it!
    final String nameProperty = getArguments().getProperty( PROP_PEGEL_NAME );
    final FeatureType featureType = ( (IKalypsoFeatureTheme)getMapModell()
        .getActiveTheme() ).getFeatureType();
    final FeatureTypeProperty ftp = featureType.getProperty( nameProperty );
    if( ftp == null )
    {
      System.out.println( "No FeatureType for Property: " + nameProperty );
      return null;
    }

    final ILabelProvider labelProvider = new FeatureLabelProvider(
        new StringModifier( ftp ) );
    final ListSelectionDialog dialog = new ListSelectionDialog( getContainer()
        .getShell(), features, new ArrayContentProvider(), labelProvider,
        "Die Daten folgender Pegel werden exportiert:" );
    dialog.setInitialElementSelections( selectedFeatures );
    dialog.setTitle( "Export Pegel: Rechenvariante " + calcCase.getName() );
    if( dialog.open() != Window.OK )
      return null;

    return Arrays.asList( dialog.getResult() );
  }

  /**
   * test for each feature of the given list, if the timeserieslink in the given
   * property is valid
   * 
   * @param featureList list of features
   * @param propertyNameTimeserieslink
   * @param context
   * @return List of features that have valid timeserieslinks
   */
  private List filterForValidTimeseriesLinks( List featureList,
      String propertyNameTimeserieslink, URL context )
  {
    // TODO filter auch im berichtsexport benutzen
    final List result = new ArrayList();

    final UrlResolver resolver = new UrlResolver();
    URL resultURL;
    for( Iterator iter = featureList.iterator(); iter.hasNext(); )
    {
      Feature fe = (Feature)iter.next();
      TimeseriesLink resultLink = (TimeseriesLink)fe
          .getProperty( propertyNameTimeserieslink );
      if( resultLink == null )
        continue;

      try
      {
        resultURL = resolver.resolveURL( context, ZmlURL
            .getIdentifierPart( resultLink.getHref() ) );
        // let's see if it throws an exception
        resultURL.openStream();
        // no exception means, result is existing
        result.add( fe );
      }
      catch( Exception e )
      {
        //   nothing, as exception is expected if result is not there
      }
    }
    return result;
  }

  /**
   * Allows user to export selected timeseries into repository. Handles UI
   * selection and delegates call to performPrognoseExport.
   */
  protected void exportPrognoseTimeseries()
  {
    final IFolder selectedCalcCase = getSelectedCalcCase();

    final Shell shell = getContainer().getShell();
    if( selectedCalcCase == null )
    {
      MessageDialog.openWarning( shell, "Prognose Zeitreihen exportieren",
          "Keine Rechenvariante selektiert" );
      return;
    }

    final List featureList = chooseSelectedFeatures( selectedCalcCase );
    if( featureList == null )
      return;

    final String resultTsName = getArguments()
        .getProperty( PROP_RESULT_TS_NAME );
    final String prognoseTsName = getArguments().getProperty(
        PROP_PROGNOSE_TS_NAME );

    final TSLinkWithName[] resultTss = getTimeseriesForProperty( "",
        featureList, resultTsName, null );
    final TSLinkWithName[] prognoseTss = getTimeseriesForProperty( "",
        featureList, prognoseTsName, null );

    final WorkspaceModifyOperation op = new WorkspaceModifyOperation()
    {
      protected void execute( final IProgressMonitor monitor )
          throws CoreException
      {
        try
        {
          final IStatus status = performPrognoseExport( resultTss, prognoseTss,
              selectedCalcCase, monitor );

          if( !status.isOK() )
            throw new CoreException( status );
        }
        catch( final ServiceException e )
        {
          throw new CoreException( KalypsoGisPlugin.createErrorStatus(
              "Zeitreihen konnten nicht exportiert werden", e ) );
        }
      }
    };

    try
    {
      getContainer().run( false, true, op );
    }
    catch( final InvocationTargetException e )
    {
      e.printStackTrace();

      IStatus status = null;
      if( e.getTargetException() instanceof CoreException )
        status = ( (CoreException)e.getTargetException() ).getStatus();
      ErrorDialog.openError( shell, "Export Prognose Zeitreihen",
          "Fehler beim Export der Zeitreihen", status );
    }
    catch( InterruptedException e )
    {
      e.printStackTrace();
    }
  }

  /**
   * Performs the timeseries export
   */
  protected IStatus performPrognoseExport( final TSLinkWithName[] resultTss,
      final TSLinkWithName[] prognoseTss, final IFolder calcCase,
      final IProgressMonitor monitor ) throws ServiceException
  {
    if( resultTss.length != prognoseTss.length )
      throw new IllegalArgumentException( "Timeseries links not same length" );

    final Logger LOG = Logger.getLogger( getClass().getName() );

    monitor.beginTask( "Prognosen zur�ck speichern", prognoseTss.length );

    final MultiStatus multiStatus = new MultiStatus( IStatus.WARNING,
        KalypsoGisPlugin.getId(), 0,
        "Fehler sind w�hrend der Export aufgetaucht." );

    try
    {
      final URL context = ResourceUtilities.createURL( calcCase );

      final UrlResolver resolver = new UrlResolver();

      final IObservationService srv = KalypsoGisPlugin.getDefault()
          .getObservationServiceProxy();

      for( int i = 0; i < prognoseTss.length; i++ )
      {
        if( monitor.isCanceled() )
          return Status.CANCEL_STATUS;

        final TSLinkWithName lnkRS = resultTss[i];
        final TSLinkWithName lnkPG = prognoseTss[i];

        try
        {
          final URL urlRS = resolver.resolveURL( context, lnkRS.href );
          final IObservation source = ZmlFactory.parseXML( urlRS, lnkRS.href );

          final String destRef = ZmlURL.insertDateRange( lnkPG.href,
              new DateRangeArgument( new Date(), new Date() ) );
          final URL urlPG = resolver.resolveURL( context, destRef );
          final IObservation dest = ZmlFactory.parseXML( urlPG, destRef );

          ITuppleModel values = null;
          try
          {
            // copy values from source into dest, expecting full compatibility
            values = ObservationUtilities.optimisticValuesCopy( source, dest,
                null, true );
          }
          catch( IllegalArgumentException e )
          {
            // not all axes could be associated between source and dest
            LOG.warning( "Observations are not compatible: " + e + "\n"
                + "Check if all axes of " + dest + " are defined in " + source );

            multiStatus.addMessage(
                "Zeitreihe konnte nicht auf dem Server hochgeladen werden "
                    + "weil sie nicht mit der Zielzeitreihe kompatibel ist. "
                    + "Pr�fen Sie u.a. die Achsenliste in: " + source, e );
          }

          // todo: maybe inform when nothing happened during copy
          if( values == null )
            LOG.info( "Nothing to copy for " + source.getName() );
          else
          {
            // save observation if it is a server side one
            if( ZmlURL.isServerSide( lnkPG.href ) )
            {
              ServiceRepositoryObservation.setValuesFor( values, lnkPG.href,
                  srv );
              LOG.info( "Observation saved on server: " + lnkPG.href );
            }
            else
              LOG.warning( "! Observation not server side: " + lnkPG.href );
          }
        }
        catch( SensorException e )
        {
          LOG.warning( e.getLocalizedMessage() );

          multiStatus.addMessage( "Fehler mit folgenden Zeitreihen: "
              + lnkRS.href + " und " + lnkPG.href, e );
        }

        monitor.worked( 1 );
      }

      return multiStatus;
    }
    catch( MalformedURLException e )
    {
      LOG.warning( e.getLocalizedMessage() );

      return KalypsoGisPlugin.createErrorStatus( "Fehler in der Konfiguration",
          e );
    }
    finally
    {
      monitor.done();
    }
  }

  private IFolder getSelectedCalcCase()
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

    final Control mapControl = initMap( mapPanel, MapPanel.WIDGET_SINGLE_SELECT );
    mapControl.setLayoutData( new GridData( GridData.FILL_BOTH ) );
  }

  protected void startGrafik()
  {
    final IFolder calcFolder = getCalcFolder();
    final IFolder grafikFolder = calcFolder.getFolder( "grafik" );

    final ObsdiagviewType xml;
    try
    {
      xml = DiagViewUtils.buildDiagramTemplateXML( m_diagView );
    }
    catch( JAXBException e2 )
    {
      e2.printStackTrace();
      return;
    }

    final RunnableContextHelper op = new RunnableContextHelper( getContainer() )
    {
      public void run( final IProgressMonitor monitor )
          throws InvocationTargetException
      {
        try
        {
          monitor.beginTask( "Grafik �ffnen", IProgressMonitor.UNKNOWN );

          if( !monitor.isCanceled() )
            GrafikLauncher
                .startGrafikODT( "grafik", xml, grafikFolder, monitor );
        }
        catch( final SensorException se )
        {
          se.printStackTrace();
          throw new InvocationTargetException( se );
        }
        finally
        {
          monitor.done();
        }
      }
    };

    op.runAndHandleOperation( getShell(), true, false, "Hochwasser Vorhersage",
        "Grafik �ffnen" );
  }

  /**
   * �berschrieben, da wir das gleiche f�r mehrere contexte = mehrere
   * Rechenf�lle ausf�hren
   */
  public void refreshDiagram()
  {
    // TODO: sollte nicht diese methode �berschreiben sondern besser die methode
    // getObservationsToShow!

    // erstmal leer, damit das Diagramm gel�scht wird
    refreshDiagramForContext( new TSLinkWithName[] {}, getContext() );

    final Object[] checkedCalcCases = getCheckedCalcCases();
    if( checkedCalcCases == null || checkedCalcCases.length == 0 )
      return;

    // TODO ist ein kleiner hack, der davon ausgeht, dass das modell sich nie
    // �ndern wird
    // es werden einfach die links vom aktuellen Modell gegen alle
    // selektierten Rechenf�lle aufgel�st
    final TSLinkWithName[] obs = getObservationsToShow( true );

    for( int i = 0; i < checkedCalcCases.length; i++ )
    {
      try
      {
        final IFolder calcCase = (IFolder)checkedCalcCases[i];
        final URL context = ResourceUtilities.createURL( calcCase );
        refreshDiagramForContext( obs, context );
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
   * @see org.kalypso.ui.calcwizard.modelpages.AbstractCalcWizardPage#getObservationsToShow(boolean)
   */
  protected TSLinkWithName[] getObservationsToShow( final boolean onlySelected )
  {
    return getObservations( onlySelected );
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

    private IFolder m_selected;

    public CheckListGetter( final CheckboxTableViewer ctv )
    {
      m_cl = ctv;
    }

    /**
     * @return Returns the selected.
     */
    public IFolder getSelected()
    {
      return m_selected;
    }

    public Object[] getResults()
    {
      return m_results;
    }

    public void run()
    {
      m_results = m_cl.getCheckedElements();
      final IStructuredSelection selection = (IStructuredSelection)m_cl
          .getSelection();

      m_selected = (IFolder)( selection.isEmpty() ? null : selection
          .getFirstElement() );
    }
  }
}