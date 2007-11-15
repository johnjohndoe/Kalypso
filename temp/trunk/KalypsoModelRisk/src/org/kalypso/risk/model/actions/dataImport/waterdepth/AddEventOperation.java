package org.kalypso.risk.model.actions.dataImport.waterdepth;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.ogc.gml.AbstractCascadingLayerTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.template.types.StyledLayerType.Property;
import org.kalypso.template.types.StyledLayerType.Style;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * @author Gernot Belger
 */
public final class AddEventOperation implements ICoreRunnableWithProgress
{
  private final IRasterDataModel m_model;

  private final AbstractCascadingLayerTheme m_wspThemes;

  private final String m_eventName;

  private final SzenarioDataProvider m_provider;

  public AddEventOperation( final String eventName, final IRasterDataModel model, final AbstractCascadingLayerTheme wspThemes, final SzenarioDataProvider provider )
  {
    m_eventName = eventName;
    m_model = model;
    m_wspThemes = wspThemes;
    m_provider = provider;
  }

  public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
  {
    try
    {
      monitor.beginTask( "Ereignis hinzuf�gen", 7 );

      /* Create a unique name */
      final IFeatureWrapperCollection<IAnnualCoverageCollection> waterlevelCoverageCollection = m_model.getWaterlevelCoverageCollection();
      final Set<String> names = new HashSet<String>();
      for( final IAnnualCoverageCollection runoffEvent : waterlevelCoverageCollection )
        names.add( runoffEvent.getName() );

      ProgressUtilities.worked( monitor, 1 );

      /* Add new feature */
      final IAnnualCoverageCollection newCoverageCollection = waterlevelCoverageCollection.addNew( IAnnualCoverageCollection.QNAME );
      newCoverageCollection.setName( m_eventName );

      ProgressUtilities.worked( monitor, 1 );

      /* Add event-themes to map */

      // - check if theme is already there
      // - add theme to map
      addEventThemes( m_wspThemes, newCoverageCollection );

      // final AddFeatureCommand command = new AddFeatureCommand( workspace, parentFeature, parentRelation, -1,
      // newEventFeature, null, true );
      // workspace.postCommand( command );

      ProgressUtilities.worked( monitor, 1 );

      /*
       * Save model and map, as undo is not possible here and the user should not be able to 'verwerfen' the changes
       */
      // workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace,
      // waterdepthCoverageCollection.getWrappedFeature(), new Feature[] { annualCoverageCollection.getWrappedFeature()
      // }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
      m_provider.postCommand( IRasterDataModel.class, new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$
      m_provider.saveModel( IRasterDataModel.class, new SubProgressMonitor( monitor, 1 ) );
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

  protected void addEventThemes( final AbstractCascadingLayerTheme parentKalypsoTheme, final IAnnualCoverageCollection annualCoverageCollection ) throws Exception
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final IFolder scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
    final String layerName = annualCoverageCollection.getName();
    final StyledLayerType layer = new StyledLayerType();
    layer.setName( layerName );
    layer.setFeaturePath( "#fid#" + annualCoverageCollection.getGmlID() + "/coverageMember" ); //$NON-NLS-1$
    layer.setLinktype( "gml" ); //$NON-NLS-1$
    layer.setType( "simple" ); //$NON-NLS-1$
    layer.setVisible( true );
    layer.setActuate( "onRequest" ); //$NON-NLS-1$
    layer.setHref( "project:/" + scenarioFolder.getProjectRelativePath() + "/models/RasterDataModel.gml" ); //$NON-NLS-1$
    layer.setVisible( true );
    final Property layerPropertyDeletable = new Property();
    layerPropertyDeletable.setName( IKalypsoTheme.PROPERTY_DELETEABLE );
    layerPropertyDeletable.setValue( "false" );
    final Property layerPropertyThemeInfoId = new Property();
    layerPropertyThemeInfoId.setName( IKalypsoTheme.PROPERTY_THEME_INFO_ID );
    layerPropertyThemeInfoId.setValue( "org.kalypso.gml.ui.map.CoverageThemeInfo?format=Wassertiefe %.2f m" ); //$NON-NLS-1$
    final List<Property> layerPropertyList = layer.getProperty();
    layerPropertyList.add( layerPropertyDeletable );
    layerPropertyList.add( layerPropertyThemeInfoId );
    final List<Style> styleList = layer.getStyle();
    final Style style = new Style();
    style.setLinktype( "sld" ); //$NON-NLS-1$
    style.setStyle( "Kalypso style" ); //$NON-NLS-1$
    style.setActuate( "onRequest" ); //$NON-NLS-1$
    style.setHref( "../styles/WaterlevelCoverage.sld" ); //$NON-NLS-1$
    style.setType( "simple" ); //$NON-NLS-1$
    styleList.add( style );

    parentKalypsoTheme.addLayer( layer );
  }

}