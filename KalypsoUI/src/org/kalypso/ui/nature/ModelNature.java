package org.kalypso.ui.nature;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.apache.tools.ant.filters.ReplaceTokens;
import org.apache.tools.ant.filters.ReplaceTokens.Token;
import org.deegree.model.feature.GMLWorkspace;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.eclipse.core.resources.FolderUtilities;
import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.model.xml.CalcCaseConfigType;
import org.kalypso.model.xml.Calcwizard;
import org.kalypso.model.xml.CalcwizardType;
import org.kalypso.model.xml.Modelspec;
import org.kalypso.model.xml.ModelspecType;
import org.kalypso.model.xml.ObjectFactory;
import org.kalypso.model.xml.CalcwizardType.PageType.ArgType;
import org.kalypso.model.xml.ModelspecType.InputType;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.services.proxy.CalcJobDataBean;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.calcwizard.CalcWizard;
import org.kalypso.ui.calcwizard.ICalcWizardPage;
import org.kalypso.util.transformation.TransformationHelper;
import org.xml.sax.InputSource;

/**
 * 
 * @author belger
 */
public class ModelNature implements IProjectNature, IResourceChangeListener
{
  private static final String MODELLTYP_FOLDER = ".model";

  private static final String MODELLTYP_CALCCASECONFIG_XML = MODELLTYP_FOLDER + "/"
      + "calcCaseConfig.xml";

  private static final String MODELLTYP_MODELSPEC_XML = MODELLTYP_FOLDER + "/" + "modelspec.xml";

  public static final String ID = "org.kalypso.ui.ModelNature";

  private static final String METADATA_FILE = ".metadata";

  public static final String CALCULATION_FILE = ".calculation";

  public static final String CALCULATION_TEMPLATE = ".calculation.template";

  public static final String CALCULATION_VIEW = MODELLTYP_FOLDER + "/.calculation.view";

  private static final String MODELLTYP_CALCWIZARD_XML = MODELLTYP_FOLDER + "/" + "calcWizard.xml";

  private final Properties m_metadata = new Properties();

  private IProject m_project;

  private static final String PROGNOSE_FOLDER = ".prognose";

  private Map m_calcJobMap = new HashMap();

  public static final String CONTROL_TEMPLATE_GML = MODELLTYP_FOLDER + "/" + CALCULATION_TEMPLATE;

  public static final String CONTROL_TEMPLATE_XSD = MODELLTYP_FOLDER + "/schema/control.xsd";

  /**
   * @see org.eclipse.core.resources.IProjectNature#configure()
   */
  public void configure()
  {
  // nix tun
  }

  private IFile getMetadataFile()
  {
    return m_project.getFile( new Path( METADATA_FILE ) );
  }

  /**
   * @see org.eclipse.core.resources.IProjectNature#deconfigure()
   */
  public void deconfigure() throws CoreException
  {
    // TODO: wird nie aufgerufen!
    try
    {
      final IFile file = getMetadataFile();

      final ByteArrayOutputStream bos = new ByteArrayOutputStream();
      m_metadata.store( bos, "Modell-Projekt Metadata Information" );
      bos.close();

      final ByteArrayInputStream bis = new ByteArrayInputStream( bos.toByteArray() );

      if( file.exists() )
        file.setContents( bis, false, true, new NullProgressMonitor() );
      else
        file.create( bis, false, new NullProgressMonitor() );

      bis.close();
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.eclipse.core.resources.IProjectNature#getProject()
   */
  public IProject getProject()
  {
    return m_project;
  }

  /**
   * @see org.eclipse.core.resources.IProjectNature#setProject(org.eclipse.core.resources.IProject)
   */
  public void setProject( final IProject project )
  {
    if( m_project != null )
      ResourcesPlugin.getWorkspace().removeResourceChangeListener( this );

    m_project = project;

    if( m_project != null )
    {
      ResourcesPlugin.getWorkspace().addResourceChangeListener( this );
      try
      {
        reloadMetadata();
      }
      catch( final CoreException e )
      {
        // TODO: als job absetzen?
        e.printStackTrace();
      }
    }

  }

  public void dispose()
  {
    ResourcesPlugin.getWorkspace().removeResourceChangeListener( this );
  }

  public static String checkCanCreateCalculationCase( final IPath path )
  {
    final IWorkspaceRoot resourceRoot = ResourcesPlugin.getWorkspace().getRoot();
    final IResource resource = resourceRoot.findMember( path );

    if( resource == null || resource == resourceRoot )
      return "Rechenfall muss innerhalb eines Projektordners angelegt werden";

    if( resource instanceof IFolder )
    {
      final IFolder folder = (IFolder)resource;
      if( isCalcCalseFolder( folder ) )
        return "Rechenfall darf nicht innerhalb eines anderen Rechenfalls angelegt werden";

      return checkCanCreateCalculationCase( folder.getParent().getFullPath() );
    }
    else if( resource instanceof IProject )
    {
      final IProject project = (IProject)resource;
      try
      {
        project.isNatureEnabled( ModelNature.ID );
        return null;
      }
      catch( CoreException e )
      {
        e.printStackTrace();

        return e.getMessage();
      }
    }

    return "???";
  }

  public static boolean isCalcCalseFolder( final IFolder folder )
  {
    final IResource calcFile = folder.findMember( CALCULATION_FILE );
    return ( calcFile != null && calcFile.exists() && calcFile instanceof IFile );
  }

  public static CalcCaseConfigType readCalcCaseConfig( final IFolder folder ) throws CoreException
  {
    final IProject project = folder.getProject();

    final IFile tranformerConfigFile = project.getFile( ModelNature.MODELLTYP_CALCCASECONFIG_XML );
    try
    {
      // Protokolle ersetzen
      final ReplaceTokens replaceReader = new ReplaceTokens( new InputStreamReader(
          tranformerConfigFile.getContents(), tranformerConfigFile.getCharset() ) );

      configureReplaceTokensForCalcCase( folder, replaceReader );

      return (CalcCaseConfigType)new ObjectFactory().createUnmarshaller()
          .unmarshal( new InputSource( replaceReader ) );
    }
    catch( final UnsupportedEncodingException e )
    {
      e.printStackTrace();

      throw new CoreException( new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0,
          "Fehler beim Lesen der Rechenfallkonfiguration: "
              + tranformerConfigFile.getProjectRelativePath().toString(), e ) );
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0,
          "Fehler beim Lesen der Rechenfallkonfiguration: "
              + tranformerConfigFile.getProjectRelativePath().toString(), e ) );
    }
  }

  /** Erzeugt einen neuen Rechenfall im angegebenen Ordner */
  public static void createCalculationCaseInFolder( final IFolder folder,
      final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Rechenfall erzeugen", 2000 );

    // Protokolle ersetzen
    try
    {
      final CalcCaseConfigType trans = readCalcCaseConfig( folder );

      monitor.worked( 1000 );

      // daten transformieren
      TransformationHelper.doTranformations( trans.getCreateTransformations(),
          new SubProgressMonitor( monitor, 1000 ) );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();

      throw e;
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new CoreException( new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0,
          "Fehler beim Erzeugen des Rechenfalls\n" + e.getLocalizedMessage(), e ) );
    }

    monitor.done();
  }

  /** Aktualisiert einen vorhandenen Rechenfall 
   * @throws CoreException*/
  public static void updateCalcCase( final IFolder folder, final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Rechenfall erzeugen", 2000 );

    // Protokolle ersetzen
    try
    {
      final CalcCaseConfigType trans = readCalcCaseConfig( folder );

      monitor.worked( 1000 );

      // daten transformieren
      TransformationHelper.doTranformations( trans.getUpdateTransformations(),
          new SubProgressMonitor( monitor, 1000 ) );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();

      throw e;
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new CoreException( new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0,
          "Fehler beim Aktualisieren des Rechenfalls\n" + e.getLocalizedMessage(), e ) );
    }

    monitor.done();
  }

  private static void configureReplaceTokensForCalcCase( final IFolder calcFolder,
      final ReplaceTokens replaceTokens )
  {
    replaceTokens.setBeginToken( ':' );
    replaceTokens.setEndToken( ':' );

    final Token timeToken = new ReplaceTokens.Token();
    timeToken.setKey( "SYSTEM_TIME" );
    timeToken.setValue( new SimpleDateFormat( "dd.MM.yyyy HH:mm" ).format( new Date( System
        .currentTimeMillis() ) ) );

    replaceTokens.addConfiguredToken( timeToken );

    final Token calcdirToken = new ReplaceTokens.Token();
    calcdirToken.setKey( "calcdir" );
    calcdirToken.setValue( calcFolder.getFullPath().toString() + "/" );

    replaceTokens.addConfiguredToken( calcdirToken );

    final Token projectToken = new ReplaceTokens.Token();
    projectToken.setKey( "project" );
    projectToken.setValue( calcFolder.getProject().getFullPath().toString() + "/" );

    replaceTokens.addConfiguredToken( projectToken );
  }

  public String getCalcType() throws CoreException
  {
    final Modelspec modelspec = getModelspec();

    return modelspec.getTypeID();
  }

  /**
   * @see org.eclipse.core.resources.IResourceChangeListener#resourceChanged(org.eclipse.core.resources.IResourceChangeEvent)
   */
  public void resourceChanged( final IResourceChangeEvent event )
  {
    final IResourceDelta delta = event.getDelta();
    final IResourceDelta metadataDelta = delta.findMember( getMetadataFile().getFullPath() );
    if( metadataDelta == null )
      return;

    switch( metadataDelta.getKind() )
    {
    case IResourceDelta.ADDED:
    case IResourceDelta.REMOVED:
    case IResourceDelta.CHANGED:
    {
      try
      {
        reloadMetadata();
      }
      catch( final CoreException e )
      {
        // TODO: error handling? -->> als job absetzen?
        e.printStackTrace();
      }
      break;
    }
    }
  }

  private void reloadMetadata() throws CoreException
  {
    try
    {
      m_metadata.clear();

      final IFile file = getMetadataFile();
      m_metadata.load( file.getContents() );
    }
    catch( final IOException e )
    {
      throw new CoreException( new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0,
          "Error loading Metadata", e ) );
    }
  }

  private CalcJobDataBean[] prepareCalcCaseInput( final IFolder folder, final IProgressMonitor monitor ) throws CoreException
  {
    // modelspec holen
    final Modelspec modelspec = getModelspec();

//    final Collection inputURLs = new ArrayList();
//    try
//    {
//      // anhand der modelspec die dateien rausfinden
//      final List inputList = modelspec.getInput();
//
//      monitor.beginTask( "Eingabedateien werden gelesen", inputList.size() );
//
//      // immer erst mal die .calculation Datei
//      final IFile calcFile = folder.getFile( CALCULATION_FILE );
//      //inputURLs.add( loadFileIntoString( calcFile ) );
//      inputURLs.add( calcFile.getRawLocation().toFile().toURL() );
//
//      for( final Iterator iter = inputList.iterator(); iter.hasNext(); )
//      {
//        final ModelspecType.InputType input = (ModelspecType.InputType)iter.next();
//        final String path = input.getPath();
//
//        final IFile file = input.isRelativeToCalcCase() ? folder.getFile( path ) : m_project
//            .getFile( path );
//
//        inputURLs.add( file.getRawLocation().toFile().toURL() );
//
//        monitor.worked( 1 );
//      }
//    }
//    catch( final IOException e )
//    {
//      e.printStackTrace();
//    }
//
//    monitor.done();
//
//    return (URL[])inputURLs.toArray( new URL[inputURLs.size()] );

    // TODO: setzen!
    final File targetDir = null;
    final File serverCalcDir = new File( targetDir, "input" );
    final File serverBaseDir = new File( targetDir, "base" );
    
    final IProject project = folder.getProject();
    
    final List inputList = modelspec.getInput();
    
    final List inputBeanList = new ArrayList();
    for( final Iterator iter = inputList.iterator(); iter.hasNext(); )
    {
      final ModelspecType.InputType input = (InputType)iter.next();
      final String inputPath = input.getPath();

      final IResource inputResource = ( input.isRelativeToCalcCase() ? folder.findMember( inputPath ) : project.findMember( inputPath ) );
      
      final File dir = input.isRelativeToCalcCase() ? serverCalcDir : serverBaseDir;
      final File inputfile = new File( dir, inputPath );

      // visit over resource
      final IResourceVisitor copyVisitor = new CopyVisitor( inputfile );
      inputResource.accept( copyVisitor );

      // move to visitor
      // problem: visitor sagen wies mit den relativen Pfaden geht
      //      inputfile.getParentFile().mkdirs();
//      
//      FileUtilities.makeFileFromStream( false, inputfile, getClass().getResourceAsStream(
//          inputresource ) );

      // TODO wieder einbinden
//      inputBeanList.add( new CalcJobDataBean( input.getId(), input.getDescription(), path ) );
    }

    final CalcJobDataBean[] input = (CalcJobDataBean[])inputBeanList.toArray( new CalcJobDataBean[inputBeanList.size()]  );
    return input;

  }

  private Modelspec getModelspec() throws CoreException
  {
    try
    {
      final IFile file = m_project.getFile( MODELLTYP_MODELSPEC_XML );

      final ObjectFactory faktory = new ObjectFactory();
      final Unmarshaller unmarshaller = faktory.createUnmarshaller();
      return (Modelspec)unmarshaller.unmarshal( file.getContents() );
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
      
      throw new CoreException( KalypsoGisPlugin.createErrorStatus( "Fehler beim Laden der Modell-Spezifikation", e ) );
    }
  }

  //  private void putCalcCaseOutputData( final IFolder folder, final URL[]
  // results,
  //      final IProgressMonitor monitor ) throws CoreException
  //  {
  //    monitor.beginTask( "Ergebnisdaten werden abgelegt", 2000 );
  //
  //    final Modelspec modelspec = getModelspec();
  //
  //    final List outputList = modelspec.getOutput();
  //    int count = 0;
  //    if( results == null || results.length < outputList.size() )
  //      throw new CoreException( new Status( IStatus.ERROR,
  // KalypsoGisPlugin.getId(), 0,
  //          "Ergebnisdaten passen nicht zur Modellspezifikation", null ) );
  //
  //    final IFolder resultsFolder = folder.getFolder( CALC_RESULT_FOLDER );
  //    if( resultsFolder.exists() )
  //      resultsFolder.delete( false, true, new SubProgressMonitor( monitor, 1000 )
  // );
  //
  //    resultsFolder.create( false, true, null );
  //
  //    for( final Iterator iter = outputList.iterator(); iter.hasNext(); )
  //    {
  //      try
  //      {
  //        final ModelspecType.OutputType output =
  // (ModelspecType.OutputType)iter.next();
  //        final String path = output.getPath();
  //
  //        final IFile file = resultsFolder.getFile( path );
  //        final PipedInputStream pis = new PipedInputStream();
  //        final PipedOutputStream pos = new PipedOutputStream( pis );
  //
  //        final int index = count;
  //        final Thread readThread = new Thread( "Ergebnis lesen: " + path )
  //        {
  //          public void run()
  //          {
  //            try
  //            {
  //              final URL url = results[index];
  //              // jobmonitor.beginTask( "URL lesen: " + url.toExternalForm(),
  //              // IProgressMonitor.UNKNOWN );
  //
  //              final InputStream urlStream = url.openStream();
  //              StreamUtilities.streamCopy( urlStream, pos );
  //
  //              pos.close();
  //            }
  //            catch( final IOException e )
  //            {
  //              e.printStackTrace();
  //              // return new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0,
  //              // "Fehler beim Zugriff auf Ergebnisdaten", e );
  //            }
  //            finally
  //            {
  //              if( pos != null )
  //                try
  //                {
  //                  pos.close();
  //                }
  //                catch( IOException e )
  //                {
  //                  e.printStackTrace();
  //                  // return new Status( IStatus.ERROR, KalypsoGisPlugin.getId(),
  //                  // 0,
  //                  // "Fehler beim Zugriff auf Ergebnisdaten", e );
  //                }
  //            }
  //
  //            // return Status.OK_STATUS;
  //          }
  //        };
  //        readThread.start();
  //
  //        file.create( pis, false, null );
  //      }
  //      catch( final Exception e )
  //      {
  //        e.printStackTrace();
  //      }
  //
  //      count++;
  //    }
  //
  //    monitor.worked( 1000 );
  //  }

  public static void runPrognose( final Shell shell, final String name )
  {
    try
    {
      final IProject project = (IProject)ResourcesPlugin.getWorkspace().getRoot().findMember( name );

      // ein noch nicht benutztes Unterverzeichnis im Prognoseverzeichnis finden
      final IFolder prognoseFolder = project.getFolder( PROGNOSE_FOLDER );
      final IFolder calcCaseFolder = FolderUtilities
          .createUnusedFolder( prognoseFolder, "prognose" );

      final Job createCalcCaseJob = new Job( "neuen Rechenfall anlegen" )
      {
        protected IStatus run( final IProgressMonitor monitor )
        {
          try
          {
            monitor.beginTask( "neuen Rechenfall erzeugen", 2000 );
            calcCaseFolder.create( false, true, new SubProgressMonitor( monitor, 1000 ) );
            createCalculationCaseInFolder( calcCaseFolder, new SubProgressMonitor( monitor, 1000 ) );
          }
          catch( final Exception e )
          {
            e.printStackTrace();

            return new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0,
                "Der Rechenfall für die Prognoserechnung konnte nicht erzeugt werden", e );
          }

          return Status.OK_STATUS;
        }
      };
      createCalcCaseJob.schedule();
      createCalcCaseJob.join();
      if( createCalcCaseJob.getResult().getSeverity() != IStatus.OK )
        return;

      final Wizard wizard = new CalcWizard( calcCaseFolder );
      wizard.setNeedsProgressMonitor( true );
      wizard.setWindowTitle( "Prognoserechnung für " + project.getName() );

      // wizard seiten hinzufügen!
      final IFile wizardConfigFile = (IFile)project.findMember( MODELLTYP_CALCWIZARD_XML );
      final InputSource inputSource = new InputSource( wizardConfigFile.getContents() );
      inputSource.setEncoding( wizardConfigFile.getCharset() );

      final Calcwizard calcwizardData = (Calcwizard)new ObjectFactory().createUnmarshaller()
          .unmarshal( inputSource );
      final List pages = calcwizardData.getPage();
      for( Iterator pIt = pages.iterator(); pIt.hasNext(); )
      {
        final CalcwizardType.PageType page = (CalcwizardType.PageType)pIt.next();

        final Properties props = new Properties();
        final List arglist = page.getArg();
        for( Iterator aIt = arglist.iterator(); aIt.hasNext(); )
        {
          final CalcwizardType.PageType.ArgType arg = (ArgType)aIt.next();
          props.setProperty( arg.getName(), arg.getValue() );
        }

        final String className = page.getClassName();
        final String pageTitle = page.getPageTitle();
        final String imageLocation = page.getImageLocation();
        final ImageDescriptor imageDesc = imageLocation == null ? null : ImageProvider
            .id( imageLocation );

        final ICalcWizardPage wizardPage = (ICalcWizardPage)ClassUtilities.newInstance( className,
            ICalcWizardPage.class, ModelNature.class.getClassLoader(), null, null );
        wizardPage.init( project, pageTitle, imageDesc, props, calcCaseFolder );
        wizard.addPage( wizardPage );
      }

      final WizardDialog wizardDialog = new WizardDialog( shell, wizard );
      wizardDialog.create();
      wizardDialog.getShell().setBounds( shell.getBounds() );
      wizardDialog.open();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  public String startCalculation( final IFolder calcFolder, final IProgressMonitor monitor )
      throws InvocationTargetException
  {
    //    try
    //    {
    if( m_calcJobMap.containsValue( calcFolder ) )
      throw new InvocationTargetException( null,
          "Dieser Rechenfall wird zur Zeit bereits gerechnet" );

    monitor.beginTask( "Modellrechnung starten", 2000 );

    // die Dateien suchen und erzeugen
    //      final URL[] input = getCalcCaseInputData( calcFolder, new
    // SubProgressMonitor( monitor, 1000 ) );

    // start job
    // TODO
    //      final CalcJobService calcService =
    // KalypsoGisPlugin.getDefault().getCalcService();
    //      final String jobID = calcService.createJob( getCalcType(),
    // calcFolder.getName(), input );
    //
    //      monitor.worked( 1000 );
    //
    //      m_calcJobMap.put( jobID, calcFolder );
    //
    //      return jobID;
    return "";
    //    }
    //    catch( final CalcJobServiceException e )
    //    {
    //      e.printStackTrace();
    //
    //      throw new InvocationTargetException( e, "Rechenlauf konnte nicht
    // gestartet werden" );
    //    }
  }

  public void stopCalculation( final String jobID )
  {
    jobID.getClass();
    //    final CalcJobService calcService =
    // KalypsoGisPlugin.getDefault().getCalcService();
    //
    //    try
    //    {
    //      calcService.cancelJob( jobID );
    //      calcService.removeJob( jobID );
    //    }
    //    catch( final CalcJobServiceException e )
    //    {
    //      e.printStackTrace();
    //    }
    //    finally
    //    {
    //      m_calcJobMap.remove( jobID );
    //    }
  }

  //  public synchronized CalcJobDescription checkCalculation( final String jobID
  // )
  //      throws CoreException
  //  {
  //    try
  //    {
  //      final CalcJobService calcService =
  // KalypsoGisPlugin.getDefault().getCalcService();
  //
  //      final CalcJobDescription description = calcService.getJobDescription( jobID
  // );
  //
  //      return description;
  //    }
  //    catch( final CalcJobServiceException cse )
  //    {
  //      m_calcJobMap.remove( jobID );
  //
  //      throw new CoreException( new Status( IStatus.ERROR,
  // KalypsoGisPlugin.getId(), 0,
  //          "Fehler beim Prüfen des Rechenstatus: ID=" + jobID, cse ) );
  //    }
  //  }

  public void retrieveCalculation( final String jobID ) //throws CoreException
  {
    jobID.getClass();
    //    try
    //    {
    //      final CalcJobService calcService =
    // KalypsoGisPlugin.getDefault().getCalcService();
    //      final CalcJobDescription description = calcService.getJobDescription(
    // jobID );
    //
    //      if( description.getState() != CalcJobStatus.FINISHED )
    //        throw new CoreException( new Status( IStatus.ERROR,
    // KalypsoGisPlugin.getId(), 0,
    //            "Berechnung nicht beendet: ID=" + jobID, null ) );
    //
    //      final IFolder folder = (IFolder)m_calcJobMap.get( jobID );
    //
    //      final URL[] results = calcService.retrieveResults( jobID );
    //
    //      putCalcCaseOutputData( folder, results, new NullProgressMonitor() );
    //
    //      calcService.removeJob( jobID );
    //    }
    //    catch( CalcJobServiceException e )
    //    {
    //      e.printStackTrace();
    //
    //      throw new CoreException( new Status( IStatus.ERROR,
    // KalypsoGisPlugin.getId(), 0,
    //          "Fehler beim Prüfen des Rechenstatus: ID=" + jobID, null ) );
    //    }
    //    finally
    //    {
    //      m_calcJobMap.remove( jobID );
    //    }
  }

  public GMLWorkspace getDefaultControl() throws CoreException
  {
    try
    {
      final URL gmlURL = new URL( "platform:/resource/" + getProject().getName() + "/"
          + CONTROL_TEMPLATE_GML );
      final URL schemaURL = new URL( "platform:/resource/" + getProject().getName() + "/"
          + CONTROL_TEMPLATE_XSD );

      // TODO: ReplaceTokens

      return GmlSerializer.createGMLWorkspace( gmlURL, schemaURL );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new CoreException( new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0,
          "Konnte Standard-Steuerparameter nicht laden:" + e.getLocalizedMessage(), e ) );
    }
  }

  public void runCalculation( final IFolder folder, final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Berechnung durchführen" , 1000 );

    if( !isCalcCalseFolder( folder ) )
      throw new CoreException( KalypsoGisPlugin.createErrorStatus( "Verzeichnis ist kein Rechenfall :" + folder.getName(), null ) );
    
    // Dateen zum Service schieben
    
    // Job starten
    
    // auf Job warten
    
    // Ergebnisse abholen
  }

}