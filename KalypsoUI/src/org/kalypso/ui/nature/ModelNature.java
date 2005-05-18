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
package org.kalypso.ui.nature;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Properties;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.rpc.ServiceException;

import org.apache.tools.ant.filters.ReplaceTokens;
import org.apache.tools.ant.filters.ReplaceTokens.Token;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.eclipse.core.runtime.LogStatusWrapper;
import org.kalypso.java.net.IUrlResolver;
import org.kalypso.model.xml.CalcCaseConfigType;
import org.kalypso.model.xml.ModeldataType;
import org.kalypso.model.xml.ObjectFactory;
import org.kalypso.model.xml.TransformationList;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.services.proxy.ICalculationService;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.nature.calcjob.CalcJobHandler;
import org.kalypso.util.transformation.TransformationHelper;
import org.kalypso.util.url.UrlResolver;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.schema.Mapper;
import org.kalypsodeegree_impl.model.feature.visitors.FindPropertyByNameVisitor;
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
      return "Rechenvariante muss innerhalb eines Projektordners angelegt werden";

    if( resource instanceof IFolder )
    {
      final IFolder folder = (IFolder) resource;
      if( isCalcCalseFolder( folder ) )
        return "Rechenvariante darf nicht innerhalb einer anderen Rechenvariante angelegt werden";

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
          .getId(), 0, "Fehler beim Lesen der Konfiguration: "
          + tranformerConfigFile.getProjectRelativePath().toString(), e ) );
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoGisPlugin
          .getId(), 0, "Fehler beim Lesen der Konfiguration: "
          + tranformerConfigFile.getProjectRelativePath().toString(), e ) );
    }
  }

  /**
   * Erzeugt eine neue Rechenvariante im angegebenen Ordner
   *  
   * @param folder
   * @param monitor
   * @return status
   * @throws CoreException
   */
  public IStatus createCalculationCaseInFolder( final IFolder folder,
      final IProgressMonitor monitor ) throws CoreException
  {
    return doCalcTransformation( "Rechenvariante erzeugen", TRANS_TYPE_CREATE, folder,
        monitor );
  }

  /**
   * Aktualisiert eine vorhandene Rechenvariante
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
    return doCalcTransformation( "Rechenvariante aktualisieren", TRANS_TYPE_UPDTAE,
        folder, monitor );
  }

  /**
   * Führt eine Transformation auf einer Rechenvariante durch
   * 
   * @param taskName
   * @param type
   * @param folder
   * @param monitor
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

      final LogStatusWrapper res = TransformationHelper.doTranformations( folder, transList, new SubProgressMonitor(
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
   * aus der Rechenvariante ausgelesen (.calculation) und als Token
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

      final FindPropertyByNameVisitor startsimFinder = new FindPropertyByNameVisitor( "startsimulation" );
      workspace.accept( startsimFinder, rootFeature, FeatureVisitor.DEPTH_INFINITE );
      
      final Object startSim = startsimFinder.getResult();
      if( startSim instanceof Date )
      {
        final String startSimString = Mapper.mapJavaValueToXml( startSim );
        final Token startSimToken = new ReplaceTokens.Token();
        startSimToken.setKey( "startsim" );
        startSimToken.setValue( startSimString );
        replaceTokens.addConfiguredToken( startSimToken );
      }
      
      final FindPropertyByNameVisitor startforecastFinder = new FindPropertyByNameVisitor( "startforecast" );
      workspace.accept( startforecastFinder, rootFeature, FeatureVisitor.DEPTH_INFINITE );
      final Object startForecast = startforecastFinder.getResult();
      if( startForecast instanceof Date )
      {
        final String startForecastString = Mapper.mapJavaValueToXml(
            startForecast );
        final Token startForecastToken = new ReplaceTokens.Token();
        startForecastToken.setKey( "startforecast" );
        startForecastToken.setValue( startForecastString );
        replaceTokens.addConfiguredToken( startForecastToken );

        // TODO: ziemlicher hack für den Endzeitpunkt: er ist immer fix
        // 48 Stunden nach dem startzeitpunkt
        final Calendar cal = Calendar.getInstance();
        cal.setTime( (Date)startForecast );
        cal.add( Calendar.HOUR_OF_DAY, 48 );

        final Date endSim = cal.getTime();
        final String endSimString = Mapper.mapJavaValueToXml( endSim );
        final Token endSimToken = new ReplaceTokens.Token();
        endSimToken.setKey( "endsim" );
        endSimToken.setValue( endSimString );
        replaceTokens.addConfiguredToken( endSimToken );
      }
    }
  }

  public String getCalcType( ) throws CoreException
  {
    final ModeldataType modelspec = getModelspec( MODELLTYP_MODELSPEC_XML );

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

  private ModeldataType getModelspec( String modelSpec ) throws CoreException
  {
    try
    {
      final IFile file = m_project.getFile( MODELLTYP_FOLDER + "/" + modelSpec );

      final ObjectFactory faktory = new ObjectFactory();
      final Unmarshaller unmarshaller = faktory.createUnmarshaller();
      return (ModeldataType)unmarshaller.unmarshal( file.getContents() );
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();

      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Fehler beim Laden der Modell-Spezifikation", e ) );
    }
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

    urlResolver.addReplaceToken( "time", Mapper.mapJavaValueToXml( now ) );

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
        forecastTime ) );

    // standardzeit abziehen
    final int simDiff = new Integer( m_metadata.getProperty(
        META_PROP_DEFAULT_SIMHOURS, "120" ) ).intValue();
    cal.add( Calendar.HOUR_OF_DAY, -simDiff );
    final Date simTime = cal.getTime();
    urlResolver.addReplaceToken( "startsim", Mapper.mapJavaValueToXml( simTime ) );

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
    return runCalculation( folder, monitor, MODELLTYP_MODELSPEC_XML );
  }

  public IStatus runCalculation( final IFolder calcCaseFolder,
      final IProgressMonitor monitor, final String modelSpec ) throws CoreException
  {
    if( modelSpec == null )
      return runCalculation( calcCaseFolder, monitor );
    
    monitor.beginTask( "Modellrechnung wird durchgeführt", 5000 );

    if( !isCalcCalseFolder( calcCaseFolder ) )
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Verzeichnis ist keine Rechenvariante:" + calcCaseFolder.getName(), null ) );


    try
    {
      final ICalculationService calcService = KalypsoGisPlugin.getDefault()
        .getCalculationServiceProxy();

      final ModeldataType modelspec = getModelspec( modelSpec );

      final CalcJobHandler cjHandler = new CalcJobHandler( modelspec, calcService );
      final IStatus runStatus = cjHandler.runJob( calcCaseFolder, new SubProgressMonitor( monitor, 5000 ) );
      if( runStatus.matches( IStatus.ERROR | IStatus.CANCEL ) )
        return runStatus;
      
      final IStatus transStatus = doCalcTransformation( "Rechenvariante aktualisieren",
          ModelNature.TRANS_TYPE_AFTERCALC, calcCaseFolder, new SubProgressMonitor( monitor,
              1000 ) );
      
      return new MultiStatus( KalypsoGisPlugin.getId(), 0, new IStatus[] { runStatus, transStatus }, "Berechnung abgeschlossen.", null );
//      if( !transStatus.isOK() )
//        return transStatus;
//      
//      if( transStatus.isOK() && finishText != null )
//        return new Status( IStatus.WARNING, KalypsoGisPlugin.getId(), 0, finishText, null );
//
//      return transStatus;
    }
    catch( final ServiceException e )
    {
      e.printStackTrace();
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Rechendienst konnte nicht initialisiert werden", e ) );
    }
  }
}