package org.kalypso.model.flood.ui.map.operations;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.model.flood.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoCascadingTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.template.types.StyledLayerType.Property;
import org.kalypso.template.types.StyledLayerType.Style;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * @author Gernot Belger
 */
public final class AddEventOperation implements ICoreRunnableWithProgress
{
  public static final String PROPERTY_EVENT_ID = "eventID"; //$NON-NLS-1$

  private final IFloodModel m_model;

  private final IFolder m_eventsFolder;

  private final IKalypsoCascadingTheme m_wspThemes;

  private final String m_eventName;

  private final SzenarioDataProvider m_provider;

  private final URL m_sldContent;

  public AddEventOperation( final String eventName, final IFloodModel model, final IFolder eventsFolder, final IKalypsoCascadingTheme wspThemes, final SzenarioDataProvider provider, final URL sldContent )
  {
    m_eventName = eventName;
    m_model = model;
    m_eventsFolder = eventsFolder;
    m_wspThemes = wspThemes;
    m_provider = provider;
    m_sldContent = sldContent;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
  {
    try
    {
      monitor.beginTask( Messages.getString( "org.kalypso.model.flood.ui.map.operations.AddEventOperation.0" ), 7 ); //$NON-NLS-1$

      final String dialogValue = FileUtilities.validateName( m_eventName, "_" ); //$NON-NLS-1$

      /* Create a unique name */
      final IFeatureWrapperCollection<IRunoffEvent> events = m_model.getEvents();
      final Set<String> names = new HashSet<String>();
      for( final IRunoffEvent runoffEvent : events )
        names.add( runoffEvent.getName() );

      int count = 0;
      String newFolderName = dialogValue;
      while( names.contains( newFolderName ) )
        newFolderName = dialogValue + count++;

      final IPath dataPath = Path.fromPortableString( newFolderName );

      ProgressUtilities.worked( monitor, 1 );

      /* Add new feature */
      final CommandableWorkspace workspace = m_provider.getCommandableWorkSpace( IFloodModel.class );
      final Feature parentFeature = events.getFeature();
      final IRelationType parentRelation = events.getWrappedList().getParentFeatureTypeProperty();
      final IFeatureType featureType = parentRelation.getTargetFeatureType();
      final Feature newEventFeature = workspace.createFeature( parentFeature, parentRelation, featureType, 1 );

      final IRunoffEvent newEvent = (IRunoffEvent) newEventFeature.getAdapter( IRunoffEvent.class );
      newEvent.setName( m_eventName );
      newEvent.setDataPath( dataPath );

      /* Create new folder and fill with defaults */
      final IFolder newEventFolder = m_eventsFolder.getFolder( dataPath );
      newEventFolder.create( false, true, new SubProgressMonitor( monitor, 2 ) );

      // search/replace in order to configure filter

      /* Add event-themes to map */

      // - check if theme is already there
      // - add theme to map
      checkSLDFile( newEvent, newEventFolder, m_sldContent );
      addEventThemes( m_wspThemes, newEvent );

      final AddFeatureCommand command = new AddFeatureCommand( workspace, parentFeature, parentRelation, -1, newEventFeature, null, true );
      workspace.postCommand( command );

      ProgressUtilities.worked( monitor, 1 );

      /*
       * Save model and map, as undo is not possible here and the user should not be able to discard the changes
       */
      m_provider.saveModel( IFloodModel.class, new SubProgressMonitor( monitor, 1 ) );
      // TODO: save map. Necessary?

      return Status.OK_STATUS;
    }
    catch( final IOException e )
    {
      throw new InvocationTargetException( e );
    }
    catch( final Exception e )
    {
      throw new InvocationTargetException( e );
    }
    finally
    {
      monitor.done();
    }
  }

  /**
   * Checks, if an sld file for this event already exists, creates it from the template if not
   */
  public static void checkSLDFile( final IRunoffEvent event, final IFolder eventFolder, final URL sldTemplate ) throws Exception
  {
    // TODO: this is quite crude, but the only way right now. We need to get the strings from an ITranslator defined in
    // the sld similar to the .gmt approach.

    // Configure sld
    final IFile sldFile = eventFolder.getFile( "wsp.sld" ); //$NON-NLS-1$
    final IFile propertiesFile = eventFolder.getFile( "wsp.properties" ); //$NON-NLS-1$
    final IFile propertiesDeFile = eventFolder.getFile( "wsp_de.properties" ); //$NON-NLS-1$
    if( !sldFile.exists() )
    {
      final String eventID = event.getFeature().getId();

      final URL propertiesLocation = new URL( sldTemplate, "wsp.properties" ); //$NON-NLS-1$
      final URL propertiesDeLocation = new URL( sldTemplate, "wsp_de.properties" ); //$NON-NLS-1$

      createFromUrl( sldFile, sldTemplate, eventID );
      createFromUrl( propertiesFile, propertiesLocation, eventID );
      createFromUrl( propertiesDeFile, propertiesDeLocation, eventID );
    }
  }

  private static void createFromUrl( final IFile sldFile, final URL resource, final String eventReplace ) throws IOException, CoreException
  {
    final String content = FileUtilities.toString( resource, "UTF-8" ).replaceAll( "%eventFeatureId%", eventReplace ); //$NON-NLS-1$ //$NON-NLS-2$
    final InputStream source = IOUtils.toInputStream( content, "UTF-8" ); //$NON-NLS-1$
    sldFile.create( source, false, new NullProgressMonitor() );
  }

  public static void addEventThemes( final IKalypsoCascadingTheme wspThemes, final IRunoffEvent event ) throws Exception
  {
    final String eventID = event.getFeature().getId();

    {// Polygone
      final StyledLayerType polygoneLayer = new StyledLayerType();

      polygoneLayer.setName( Messages.getString( "org.kalypso.model.flood.ui.map.operations.AddEventOperation.6", event.getName() ) ); //$NON-NLS-1$
      polygoneLayer.setFeaturePath( "polygonMember" ); //$NON-NLS-1$
      polygoneLayer.setLinktype( "gml" ); //$NON-NLS-1$
      polygoneLayer.setType( "simple" ); //$NON-NLS-1$
      polygoneLayer.setVisible( true );
      polygoneLayer.setActuate( "onRequest" ); //$NON-NLS-1$
      polygoneLayer.setHref( "../models/flood.gml" ); //$NON-NLS-1$
      polygoneLayer.setVisible( true );

      final Property layerPropertyDeletable = new Property();
      layerPropertyDeletable.setName( IKalypsoTheme.PROPERTY_DELETEABLE );
      layerPropertyDeletable.setValue( "false" ); //$NON-NLS-1$

      final Property layerPropertyEventId = new Property();
      layerPropertyEventId.setName( PROPERTY_EVENT_ID );
      layerPropertyEventId.setValue( eventID );

      final List<Property> layerPropertyList = polygoneLayer.getProperty();
      layerPropertyList.add( layerPropertyDeletable );

      final List<Style> styleList = polygoneLayer.getStyle();
      final Style extrsStyle = new Style();
      extrsStyle.setLinktype( "sld" ); //$NON-NLS-1$
      extrsStyle.setStyle( "extrapolationPolygonUserStyle" ); //$NON-NLS-1$
      extrsStyle.setActuate( "onRequest" ); //$NON-NLS-1$
      extrsStyle.setHref( styleLocationForEventWsp( event ) );
      extrsStyle.setType( "simple" ); //$NON-NLS-1$
      styleList.add( extrsStyle );

      final Style clipStyle = new Style();
      clipStyle.setLinktype( "sld" ); //$NON-NLS-1$
      clipStyle.setStyle( "clipPolygonUserStyle" ); //$NON-NLS-1$
      clipStyle.setActuate( "onRequest" ); //$NON-NLS-1$
      clipStyle.setHref( styleLocationForEventWsp( event ) );
      clipStyle.setType( "simple" ); //$NON-NLS-1$
      styleList.add( clipStyle );

      final Style volumeStyle = new Style();
      volumeStyle.setLinktype( "sld" ); //$NON-NLS-1$
      volumeStyle.setStyle( "volumePolygonUserStyle" ); //$NON-NLS-1$
      volumeStyle.setActuate( "onRequest" ); //$NON-NLS-1$
      volumeStyle.setHref( styleLocationForEventWsp( event ) );
      volumeStyle.setType( "simple" ); //$NON-NLS-1$
      styleList.add( volumeStyle );

      wspThemes.addLayer( polygoneLayer );
    }

    { // Wasserspiegel
      final StyledLayerType wspLayer = new StyledLayerType();

      wspLayer.setName( Messages.getString( "org.kalypso.model.flood.ui.map.operations.AddEventOperation.26", event.getName() ) ); //$NON-NLS-1$
      wspLayer.setFeaturePath( "#fid#" + eventID + "/tinMember" ); //$NON-NLS-1$ //$NON-NLS-2$
      wspLayer.setLinktype( "gml" ); //$NON-NLS-1$
      wspLayer.setType( "simple" ); //$NON-NLS-1$
      wspLayer.setVisible( true );
      wspLayer.setActuate( "onRequest" ); //$NON-NLS-1$
      wspLayer.setHref( "../models/flood.gml" ); //$NON-NLS-1$
      wspLayer.setVisible( true );
      final Property layerPropertyDeletable = new Property();
      layerPropertyDeletable.setName( IKalypsoTheme.PROPERTY_DELETEABLE );
      layerPropertyDeletable.setValue( "false" ); //$NON-NLS-1$

      final Property layerPropertyThemeInfoId = new Property();
      layerPropertyThemeInfoId.setName( IKalypsoTheme.PROPERTY_THEME_INFO_ID );

      final String infoFormat = String.format( Messages.getString("org.kalypso.model.flood.ui.map.operations.AddEventOperation.2"), event.getName() ); //$NON-NLS-1$
      final String infoValue = "org.kalypso.ogc.gml.map.themeinfo.TriangulatedSurfaceThemeInfo?format=" + infoFormat; //$NON-NLS-1$
      layerPropertyThemeInfoId.setValue( infoValue );

      final Property layerPropertyEventId = new Property();
      layerPropertyEventId.setName( PROPERTY_EVENT_ID );
      layerPropertyEventId.setValue( eventID );

      final List<Property> layerPropertyList = wspLayer.getProperty();
      layerPropertyList.add( layerPropertyDeletable );
      layerPropertyList.add( layerPropertyThemeInfoId );
      layerPropertyList.add( layerPropertyEventId );

      final List<Style> styleList = wspLayer.getStyle();
      final Style style = new Style();
      style.setLinktype( "sld" ); //$NON-NLS-1$
      style.setStyle( "wspUserStyle" ); //$NON-NLS-1$
      style.setActuate( "onRequest" ); //$NON-NLS-1$
      style.setHref( styleLocationForEventWsp( event ) );
      style.setType( "simple" ); //$NON-NLS-1$
      styleList.add( style );

      wspThemes.addLayer( wspLayer );
    }
  }

  public static String styleLocationForEventWsp( final IRunoffEvent event )
  {
    return "../events/" + event.getDataPath().toPortableString() + "/wsp.sld"; //$NON-NLS-1$ //$NON-NLS-2$
  }

}