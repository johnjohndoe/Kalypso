package org.kalypso.ui.nature;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.net.URL;
import java.rmi.RemoteException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.rpc.ServiceException;

import org.apache.commons.io.CopyUtils;
import org.apache.tools.ant.filters.ReplaceTokens;
import org.apache.tools.ant.filters.ReplaceTokens.Token;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.gml.schema.Mapper;
import org.eclipse.core.resources.IContainer;
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
import org.kalypso.eclipse.core.resources.FolderUtilities;
import org.kalypso.eclipse.util.SetContentHelper;
import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.java.net.IUrlResolver;
import org.kalypso.model.xml.CalcCaseConfigType;
import org.kalypso.model.xml.Modelspec;
import org.kalypso.model.xml.ModelspecType;
import org.kalypso.model.xml.ObjectFactory;
import org.kalypso.model.xml.TransformationList;
import org.kalypso.model.xml.ModelspecType.InputType;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.services.ProxyFactory;
import org.kalypso.services.calculation.common.ICalcServiceConstants;
import org.kalypso.services.proxy.CalcJobBean;
import org.kalypso.services.proxy.CalcJobDataBean;
import org.kalypso.services.proxy.ICalculationService;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.transformation.TransformationHelper;
import org.kalypso.util.transformation.TransformationResult;
import org.kalypso.util.url.UrlResolver;
import org.xml.sax.InputSource;

/**
 * 
 * @author belger
 */
public class ModelNature implements IProjectNature, IResourceChangeListener
{
  public static final String MODELLTYP_FOLDER = ".model";

  public static final String MODELLTYP_CALCCASECONFIG_XML = MODELLTYP_FOLDER
      + "/" + "calcCaseConfig.xml";

  public static final String MODELLTYP_MODELSPEC_XML = "modelspec.xml";

  public static final String ID = "org.kalypso.ui.ModelNature";

  private static final String METADATA_FILE = ".metadata";

  public static final String CONTROL_NAME = ".calculation";

  public static final String CONTROL_TEMPLATE_NAME = ".calculation.template";

  public static final String CONTROL_VIEW_PATH = MODELLTYP_FOLDER
      + "/.calculation.view";

  public static final String MODELLTYP_CALCWIZARD_XML = MODELLTYP_FOLDER + "/"
      + "calcWizard.xml";

  private final Properties m_metadata = new Properties();

  private IProject m_project;

  public static final String PROGNOSE_FOLDER = ".prognose";

  public static final String CONTROL_TEMPLATE_GML_PATH = MODELLTYP_FOLDER + "/"
      + CONTROL_TEMPLATE_NAME;

  private static final String META_PROP_VALID_HOURS = "VALID_FORECAST_HOURS";

  /** Standardddifferenz des Simulationsstarts vor dem Vorhersagezeitpunkt */
  private static final String META_PROP_DEFAULT_SIMHOURS = "DEFAULT_SIMHOURS";

  private static final int TRANS_TYPE_UPDTAE = 0;

  private static final int TRANS_TYPE_CREATE = 1;

  private static final int TRANS_TYPE_AFTERCALC = 2;

  /**
   * @see org.eclipse.core.resources.IProjectNature#configure()
   */
  public void configure( )
  {
    // nix tun
  }

  public final IFolder getPrognoseFolder( )
  {
    return m_project.getFolder( PROGNOSE_FOLDER );
  }

  private IFile getMetadataFile( )
  {
    return m_project.getFile( new Path( METADATA_FILE ) );
  }

  /**
   * @see org.eclipse.core.resources.IProjectNature#deconfigure()
   */
  public void deconfigure( ) throws CoreException
  {
    // todo: wird nie aufgerufen!
    try
    {
      final IFile file = getMetadataFile();

      final ByteArrayOutputStream bos = new ByteArrayOutputStream();
      m_metadata.store( bos, "Modell-Projekt Metadata Information" );
      bos.close();

      final ByteArrayInputStream bis = new ByteArrayInputStream( bos
          .toByteArray() );

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
  public IProject getProject( )
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
        // todo: als job absetzen?
        e.printStackTrace();
      }
    }

  }

  public void dispose( )
  {
    ResourcesPlugin.getWorkspace().removeResourceChangeListener( this );
  }

  public static String checkCanCreateCalculationCase( final IPath path )
  {
    final IWorkspaceRoot resourceRoot = ResourcesPlugin.getWorkspace()
        .getRoot();
    final IResource resource = resourceRoot.findMember( path );

    if( resource == null || resource == resourceRoot )
      return "Rechenfall muss innerhalb eines Projektordners angelegt werden";

    if( resource instanceof IFolder )
    {
      final IFolder folder = (IFolder) resource;
      if( isCalcCalseFolder( folder ) )
        return "Rechenfall darf nicht innerhalb eines anderen Rechenfalls angelegt werden";

      return checkCanCreateCalculationCase( folder.getParent().getFullPath() );
    }
    else if( resource instanceof IProject )
    {
      final IProject project = (IProject) resource;
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
    final IResource calcFile = folder.findMember( CONTROL_NAME );
    return (calcFile != null && calcFile.exists() && calcFile instanceof IFile);
  }

  public CalcCaseConfigType readCalcCaseConfig( final IFolder folder )
      throws CoreException
  {
    final IProject project = getProject();

    final IFile tranformerConfigFile = project
        .getFile( ModelNature.MODELLTYP_CALCCASECONFIG_XML );
    try
    {
      // Protokolle ersetzen
      final ReplaceTokens replaceReader = new ReplaceTokens(
          new InputStreamReader( tranformerConfigFile.getContents(),
              tranformerConfigFile.getCharset() ) );

      configureReplaceTokensForCalcCase( folder, replaceReader );

      return (CalcCaseConfigType) new ObjectFactory().createUnmarshaller()
          .unmarshal( new InputSource( replaceReader ) );
    }
    catch( final UnsupportedEncodingException e )
    {
      e.printStackTrace();

      throw new CoreException( new Status( IStatus.ERROR, KalypsoGisPlugin
          .getId(), 0, "Fehler beim Lesen der Rechenfallkonfiguration: "
          + tranformerConfigFile.getProjectRelativePath().toString(), e ) );
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoGisPlugin
          .getId(), 0, "Fehler beim Lesen der Rechenfallkonfiguration: "
          + tranformerConfigFile.getProjectRelativePath().toString(), e ) );
    }
  }

  /**
   * Erzeugt einen neuen Rechenfall im angegebenen Ordner
   *  
   * @param folder
   * @param monitor
   * @return status
   * @throws CoreException
   */
  public IStatus createCalculationCaseInFolder( final IFolder folder,
      final IProgressMonitor monitor ) throws CoreException
  {
    return doCalcTransformation( "Rechenfall erzeugen", TRANS_TYPE_CREATE, folder,
        monitor );
  }

  /**
   * Aktualisiert einen vorhandenen Rechenfall
   * 
   * @param folder
   * @param monitor
   * @return status
   * 
   * @throws CoreException
   */
  public IStatus updateCalcCase( final IFolder folder,
      final IProgressMonitor monitor ) throws CoreException
  {
    return doCalcTransformation( "Rechenfall aktualisieren", TRANS_TYPE_UPDTAE,
        folder, monitor );
  }

  /**
   * Führt eine Transformation auf einem Rechenfall durch
   * 
   * @param taskName
   * @param type
   * @param folder
   * @param monitor
   * @return
   * 
   * @throws CoreException
   */
  private IStatus doCalcTransformation( final String taskName, final int type,
      final IFolder folder, final IProgressMonitor monitor )
      throws CoreException
  {
    monitor.beginTask( taskName, 2000 );

    try
    {
      final CalcCaseConfigType trans = readCalcCaseConfig( folder );

      monitor.worked( 1000 );

      // Daten transformieren
      TransformationList transList = null;
      switch( type )
      {
        case TRANS_TYPE_UPDTAE:
          transList = trans.getUpdateTransformations();
          break;

        case TRANS_TYPE_CREATE:
          transList = trans.getCreateTransformations();
          break;

        case TRANS_TYPE_AFTERCALC:
          transList = trans.getAfterCalcTransformations();
          break;

        default:
          transList = null;
          break;
      }

      if( transList == null )
        return Status.OK_STATUS;

      final TransformationResult res = TransformationHelper.doTranformations( folder, transList, new SubProgressMonitor(
          monitor, 1000 ) );
      
      return res.toStatus();
    }
    catch( final CoreException e )
    {
      e.printStackTrace();

      throw e;
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new CoreException( new Status( IStatus.ERROR, KalypsoGisPlugin
          .getId(), 0, taskName + ": " + e.getLocalizedMessage(), e ) );
    }
    finally
    {
      monitor.done();
    }
  }

  /**
   * fügt eine Reihe von Tokens zum ReplaceToken hinzu. Unter anderem für
   * :project: etc Ausserdem werden die Start, Mittel und Endzeit der Simulation
   * aus dem Rechenfall Verzeichnis ausgelesen (.calculation) und als Token
   * hinzugefgügt.
   * 
   * @param calcFolder
   * @param replaceTokens
   * @throws CoreException
   */
  private void configureReplaceTokensForCalcCase( final IFolder calcFolder,
      final ReplaceTokens replaceTokens ) throws CoreException
  {
    replaceTokens.setBeginToken( ':' );
    replaceTokens.setEndToken( ':' );

    final Token timeToken = new ReplaceTokens.Token();
    timeToken.setKey( "SYSTEM_TIME" );
    timeToken.setValue( new SimpleDateFormat( "dd.MM.yyyy HH:mm" )
        .format( new Date( System.currentTimeMillis() ) ) );

    replaceTokens.addConfiguredToken( timeToken );

    final Token calcdirToken = new ReplaceTokens.Token();
    calcdirToken.setKey( "calcdir" );
    calcdirToken.setValue( calcFolder.getFullPath().toString() + "/" );

    replaceTokens.addConfiguredToken( calcdirToken );

    final Token projectToken = new ReplaceTokens.Token();
    projectToken.setKey( "project" );
    projectToken.setValue( calcFolder.getProject().getFullPath().toString()
        + "/" );

    replaceTokens.addConfiguredToken( projectToken );

    // jetzt werte aus der .calculation des aktuellen Rechenfalls lesen und
    // bestimmte Werte
    // zum Ersetzen auslesen

    final GMLWorkspace workspace = loadOrCreateControl( calcFolder );
    if( workspace != null )
    {
      final Feature rootFeature = workspace.getRootFeature();
      final Date startSim = (Date) rootFeature.getProperty( "startsimulation" );
      final String startSimString = Mapper.mapJavaValueToXml( startSim,
          "dateTime" );
      final Token startSimToken = new ReplaceTokens.Token();
      startSimToken.setKey( "startsim" );
      startSimToken.setValue( startSimString );
      replaceTokens.addConfiguredToken( startSimToken );

      final Date startForecast = (Date) rootFeature
          .getProperty( "startforecast" );
      final String startForecastString = Mapper.mapJavaValueToXml(
          startForecast, "dateTime" );
      final Token startForecastToken = new ReplaceTokens.Token();
      startForecastToken.setKey( "startforecast" );
      startForecastToken.setValue( startForecastString );
      replaceTokens.addConfiguredToken( startForecastToken );

      // TODO: ziemlicher hack für den Endzeitpunkt: er ist immer fix
      // 48 Stunden nach dem startzeitpunkt
      final Calendar cal = Calendar.getInstance();
      cal.setTime( startForecast );
      cal.add( Calendar.HOUR_OF_DAY, 48 );

      final Date endSim = cal.getTime();
      final String endSimString = Mapper.mapJavaValueToXml( endSim, "dateTime" );
      final Token endSimToken = new ReplaceTokens.Token();
      endSimToken.setKey( "endsim" );
      endSimToken.setValue( endSimString );
      replaceTokens.addConfiguredToken( endSimToken );
    }
  }

  public String getCalcType( ) throws CoreException
  {
    final Modelspec modelspec = getModelspec( MODELLTYP_MODELSPEC_XML );

    return modelspec.getTypeID();
  }

  /**
   * @see org.eclipse.core.resources.IResourceChangeListener#resourceChanged(org.eclipse.core.resources.IResourceChangeEvent)
   */
  public void resourceChanged( final IResourceChangeEvent event )
  {
    final IResourceDelta delta = event.getDelta();
    final IFile metadataFile = getMetadataFile();
    if( delta == null || metadataFile == null )
      return;

    final IResourceDelta metadataDelta = delta.findMember( metadataFile
        .getFullPath() );
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
          // todo: error handling? -->> als job absetzen?
          e.printStackTrace();
        }
        break;
      }
    }
  }

  private void reloadMetadata( ) throws CoreException
  {
    try
    {
      m_metadata.clear();

      final IFile file = getMetadataFile();
      m_metadata.load( file.getContents() );
    }
    catch( final IOException e )
    {
      throw new CoreException( new Status( IStatus.ERROR, KalypsoGisPlugin
          .getId(), 0, "Error loading Metadata", e ) );
    }
  }

  /**
   * schreibt alle input-Dateien aus der Modelspec in das targetdir
   * <p>
   * Dateien relativ zum Calc-Verzeichnis werden nach <targetdir>/input/calc
   * geschrieben
   * </p>
   * <p>
   * Dateien nicht relativ zum Calc-Verzeichnis werden nach
   * <targetdir>/input/base geschrieben
   * </p>
   * 
   * @param modelspec
   * @param folder
   * @param targetdir
   * @param monitor
   * @return beans
   * @throws CoreException
   */
  private CalcJobDataBean[] prepareCalcCaseInput( final Modelspec modelspec,
      final IFolder folder, final File targetdir, final IProgressMonitor monitor )
      throws CoreException
  {
    final File serverInputDir = new File( targetdir,
        ICalcServiceConstants.INPUT_DIR_NAME );
    final File serverCalcDir = new File( serverInputDir, "calc" );
    final File serverBaseDir = new File( serverInputDir, "base" );

    final IProject project = folder.getProject();

    final List inputList = modelspec.getInput();

    monitor.beginTask( "Kopieren der Daten zum Berechnungsdienst", inputList
        .size() );

    final List inputBeanList = new ArrayList();
    for( final Iterator iter = inputList.iterator(); iter.hasNext(); )
    {
      final ModelspecType.InputType input = (InputType) iter.next();
      final String inputPath = input.getPath();

      final IContainer baseresource = input.isRelativeToCalcCase() ? (IContainer) folder
          : (IContainer) project;
      final IResource inputResource = baseresource.findMember( inputPath );
      if( inputResource == null )
        throw new CoreException( KalypsoGisPlugin.createErrorStatus(
            "Konnte Input-Resource nicht finden: " + inputPath
                + "\nÜberprüfen Sie die Datei " + MODELLTYP_FOLDER + "/"
                + MODELLTYP_MODELSPEC_XML, null ) );

      final File basedir = input.isRelativeToCalcCase() ? serverCalcDir
          : serverBaseDir;
      final String reldir = (input.isRelativeToCalcCase() ? "calc" : "base")
          + "/" + inputPath;

      final IResourceVisitor copyVisitor = new CopyResourceToFileVisitor(
          baseresource, basedir );
      inputResource.accept( copyVisitor );

      final CalcJobDataBean calcJobDataBean = new CalcJobDataBean( input
          .getId(), input.getDescription(), reldir );
      inputBeanList.add( calcJobDataBean );

      monitor.worked( 1 );
    }

    monitor.done();

    final CalcJobDataBean[] input = (CalcJobDataBean[]) inputBeanList
        .toArray( new CalcJobDataBean[inputBeanList.size()] );
    return input;

  }

  private Modelspec getModelspec( String modelSpec ) throws CoreException
  {
    try
    {
      final IFile file = m_project.getFile( MODELLTYP_FOLDER + "/" + modelSpec );

      final ObjectFactory faktory = new ObjectFactory();
      final Unmarshaller unmarshaller = faktory.createUnmarshaller();
      return (Modelspec) unmarshaller.unmarshal( file.getContents() );
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();

      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Fehler beim Laden der Modell-Spezifikation", e ) );
    }
  }

  private void retrieveOutput( final File serveroutputdir,
      final IFolder targetfolder, final CalcJobDataBean[] results,
      final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Berechnungsergebniss abrufen", results.length );

    System.out.println( "Results: " + results.length );

    for( int i = 0; i < results.length; i++ )
    {
      final CalcJobDataBean bean = results[i];

      final String beanPath = bean.getPath();
      final File serverfile = new File( serveroutputdir, beanPath );

      final IFile targetfile = targetfolder.getFile( beanPath );
      FolderUtilities.mkdirs( targetfile.getParent() );

      if( targetfile.exists() ) // loeschen, auch wenn er gerade geladen ist
      {
        try
        {
          targetfile.delete( true, false, new NullProgressMonitor() );
        }
        catch( Exception e )
        {
          // TODO hier tritt manchmal eine exception auf, testen...
          System.out.println( "could not delete File: "
              + targetfile.getFullPath() );
          e.printStackTrace();
          continue;
        }
      }
      System.out.println( "Write: " + serverfile.getAbsolutePath() );
      final SetContentHelper thread = new SetContentHelper()
      {
        protected void write( final Writer w ) throws Throwable
        {
          InputStreamReader reader = null;
          try
          {
            final FileInputStream fis = new FileInputStream( serverfile );
            reader = new InputStreamReader( fis, getCharset() );
            CopyUtils.copy( reader, w );
          }
          finally
          {
            if( reader != null )
              reader.close();
          }
        }
      };
      thread.setFileContents( targetfile, false, false,
          new NullProgressMonitor() );
      System.out.println( "Wrote: " + serverfile.getAbsolutePath() );

      monitor.worked( 1 );

      if( monitor.isCanceled() )
        throw new CoreException( new Status( IStatus.CANCEL, KalypsoGisPlugin
            .getId(), 0, "Vorgang vom Benutzer abgebrochen", null ) );
    }

    monitor.done();
  }

  public GMLWorkspace loadDefaultControl( ) throws CoreException
  {
    return loadOrCreateControl( null );
  }

  public GMLWorkspace loadOrCreateControl( final IFolder folder )
      throws CoreException
  {
    try
    {
      // gibts das file schon, dann laden
      String gmlPath = getProject().getName() + "/" + CONTROL_TEMPLATE_GML_PATH;

      if( folder != null )
      {
        final IFile controlFile = folder.getFile( CONTROL_NAME );
        if( controlFile.exists() )
          gmlPath = controlFile.getFullPath().toString();
      }

      final URL gmlURL = new URL( "platform:/resource/" + gmlPath );

      final IUrlResolver urlResolver = configureTokensForcontrol();

      return GmlSerializer.createGMLWorkspace( gmlURL, urlResolver );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new CoreException( new Status( IStatus.ERROR, KalypsoGisPlugin
          .getId(), 0, "Konnte Standard-Steuerparameter nicht laden:"
          + e.getLocalizedMessage(), e ) );
    }
  }

  public IUrlResolver configureTokensForcontrol( ) throws CoreException
  {
    final IUrlResolver urlResolver = new UrlResolver();

    final String user = System.getProperty( "user.name",
        "<Benutzer konnte nicht ermittelt werden>" );
    urlResolver.addReplaceToken( "user", user );

    final Date now = new Date();

    urlResolver.addReplaceToken( "time", Mapper.mapJavaValueToXml( now,
        "dateTime" ) );

    // auf x stunden vorher runden! hängt von der Modellspec ab
    final Calendar cal = Calendar.getInstance();
    cal.setTime( now );
    // erstmal auf die letzte Stunde runden
    cal.set( Calendar.MINUTE, 0 );
    cal.set( Calendar.SECOND, 0 );
    cal.set( Calendar.MILLISECOND, 0 );

    // jetzt solange ganze stunden abziehen, bis der Wert ins
    // Zeitvalidierungsschema passt
    int count = 0;
    while( !validateTime( cal ) )
    {
      cal.add( Calendar.HOUR_OF_DAY, -1 );

      // nach 24h spätestens abbrechen!
      count++;
      if( count == 24 )
        throw new CoreException( KalypsoGisPlugin.createErrorStatus(
            "Zeit konnte nicht vailidiert werden: " + cal, null ) );
    }

    final Date forecastTime = cal.getTime();
    urlResolver.addReplaceToken( "startforecast", Mapper.mapJavaValueToXml(
        forecastTime, "dateTime" ) );

    // standardzeit abziehen
    final int simDiff = new Integer( m_metadata.getProperty(
        META_PROP_DEFAULT_SIMHOURS, "120" ) ).intValue();
    cal.add( Calendar.HOUR_OF_DAY, -simDiff );
    final Date simTime = cal.getTime();
    urlResolver.addReplaceToken( "startsim", Mapper.mapJavaValueToXml( simTime,
        "dateTime" ) );

    return urlResolver;
  }

  /**
   * stellt fest, ob es sich um einen gültigen Zeitpunkt für den Start der
   * Prognose handelt
   * 
   * @param cal
   * @return true when time is valid
   */
  private boolean validateTime( final Calendar cal )
  {
    // todo: wäre schöner, wenn das besser parametrisiert werden könnte
    // z.B. ein Groovy-Skript aus der Modelspec o.ä.
    final String validHours = m_metadata
        .getProperty(
            META_PROP_VALID_HOURS,
            "VALID_FORECAST_HOURS=0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23" );

    final int hour = cal.get( Calendar.HOUR_OF_DAY );

    return (" " + validHours + " ").indexOf( " " + hour + " " ) != -1;
  }

  public IStatus runCalculation( final IFolder folder,
      final IProgressMonitor monitor ) throws CoreException
  {
    return runCalculation( folder, monitor, MODELLTYP_MODELSPEC_XML, true );
  }

  public IStatus runCalculation( final IFolder folder,
      final IProgressMonitor monitor, final String modelSpec,
      boolean clearResults ) throws CoreException
  {
    if( modelSpec == null )
      return runCalculation( folder, monitor );
    
    monitor.beginTask( "Modellrechnung wird durchgeführt", 5000 );

    final CoreException cancelException = new CoreException( new Status(
        IStatus.CANCEL, KalypsoGisPlugin.getId(), 0,
        "Berechnung wurde vom Benutzer abgebrochen", null ) );

    if( !isCalcCalseFolder( folder ) )
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Verzeichnis ist kein Rechenfall :" + folder.getName(), null ) );

    final ProxyFactory serviceProxyFactory = KalypsoGisPlugin.getDefault()
        .getServiceProxyFactory();

    final Modelspec modelspec = getModelspec( modelSpec );

    final SubProgressMonitor subMonitor1 = new SubProgressMonitor( monitor,
        1000 );
    subMonitor1.beginTask( "Rechendienst wird initialisiert", 1000 );

    final CalcJobBean job;
    final ICalculationService calcService;
    try
    {
      calcService = (ICalculationService) serviceProxyFactory.getProxy(
          "Kalypso_CalculationService", ClassUtilities
              .getOnlyClassName( ICalculationService.class ) );

      job = calcService.prepareJob( modelspec.getTypeID(), "" );

      if( monitor.isCanceled() )
      {
        calcService.disposeJob( job.getId() );
        throw cancelException;
      }
    }
    catch( final RemoteException e )
    {
      e.printStackTrace();
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Fehler beim Aufruf des Rechendienstes", e ) );
    }
    catch( final ServiceException e )
    {
      e.printStackTrace();
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Rechendienst konnte nicht initialisiert werden", e ) );
    }

    subMonitor1.done();

    try
    {
      final String jobID = job.getId();

      final File targetdir = new File( job.getBasedir() );
      if( !targetdir.exists() || !targetdir.isDirectory() )
        throw new CoreException( KalypsoGisPlugin.createErrorStatus(
            "Ungültiges temporäres Verzeichnis auf Server", null ) );

      // Daten zum Service schieben
      final CalcJobDataBean[] inputBeans = prepareCalcCaseInput( modelspec,
          folder, targetdir, new SubProgressMonitor( monitor, 1000 ) );
      calcService.startJob( jobID, inputBeans );

      if( monitor.isCanceled() )
        throw cancelException;

      final SubProgressMonitor calcMonitor = new SubProgressMonitor( monitor,
          1000 );
      calcMonitor.beginTask( "Berechnung wird durchgeführt", 100 );
      int oldProgess = 0;
      while( true )
      {
        final CalcJobBean bean = calcService.getJob( jobID );

        boolean bStop = false;
        switch( bean.getState() )
        {
          case ICalcServiceConstants.FINISHED:
          case ICalcServiceConstants.ERROR:
          case ICalcServiceConstants.CANCELED:
            bStop = true;
            break;

          default:
            break;
        }

        if( bStop )
          break;

        try
        {
          Thread.sleep( 1000 );
        }
        catch( final InterruptedException e1 )
        {
          e1.printStackTrace();

          throw new CoreException( KalypsoGisPlugin.createErrorStatus(
              "Kritischer Fehler", e1 ) );
        }

        int progress = bean.getProgress();
        calcMonitor.worked( progress - oldProgess );
        oldProgess = progress;

        // ab hier bei cancel nicht mehr zurückkehren, sondern
        // erstmal den Job-Canceln und warten bis er zurückkehrt
        if( monitor.isCanceled() )
          calcService.cancelJob( jobID );
      }

      calcMonitor.done();

      final CalcJobBean jobBean = calcService.getJob( jobID );

      // Abhängig von den Ergebnissen was machen
      switch( jobBean.getState() )
      {
        case ICalcServiceConstants.FINISHED:
          // Ergebniss abholen
          final CalcJobDataBean[] results = jobBean.getResults();
          final IFolder outputfolder = folder.getFolder( "Ergebnisse" );
          if( clearResults && outputfolder.exists() )
            outputfolder.delete( false, false, new NullProgressMonitor() );

          final File serveroutputdir = new File( jobBean.getBasedir(),
              ICalcServiceConstants.OUTPUT_DIR_NAME );
          retrieveOutput( serveroutputdir, outputfolder, results,
              new SubProgressMonitor( monitor, 1000 ) );

          return doCalcTransformation( "Rechenfall aktualisieren",
              TRANS_TYPE_AFTERCALC, folder, new SubProgressMonitor( monitor,
                  1000 ) );

        case ICalcServiceConstants.CANCELED:
          throw cancelException;

        case ICalcServiceConstants.ERROR:
          throw new CoreException( KalypsoGisPlugin.createErrorStatus(
              "Rechenvorgang fehlerhaft:\n" + jobBean.getMessage(), null ) );

        default:
          // darf eigentlich nie vorkommen
          throw new CoreException( KalypsoGisPlugin.createErrorStatus(
              "Rechenvorgang fehlerhaft:\n" + jobBean.getMessage(), null ) );
      }
    }
    catch( final RemoteException e )
    {
      e.printStackTrace();
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Fehler beim Aufruf des Rechendienstes", e ) );
    }
    catch( final CoreException e )
    {
      throw e;
    }
    finally
    {
      //            try
      //            {
      //              calcService.disposeJob( job.getId() );
      //            }
      //            catch( final RemoteException e1 )
      //            {
      //              e1.printStackTrace();
      //      
      //              throw new CoreException( KalypsoGisPlugin.createErrorStatus(
      //                  "Kritischer Fehler bei Löschen des Rechen-Jobs", e1 ) );
      //            }
    }
  }

}