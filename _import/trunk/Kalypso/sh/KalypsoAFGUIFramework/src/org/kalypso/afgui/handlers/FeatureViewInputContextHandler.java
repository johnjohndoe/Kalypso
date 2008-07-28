/**
 *
 */
package org.kalypso.afgui.handlers;

import java.net.URL;
import java.util.Properties;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.template.featureview.Featuretemplate;
import org.kalypso.template.featureview.Featuretemplate.Layer;
import org.kalypso.ui.editor.featureeditor.FeatureTemplateView;

import de.renew.workflow.connector.cases.CaseHandlingSourceProvider;

/**
 * Loads a template file in the current feature view. Requires that the current context contains the feature view. Use a
 * {@link ViewContextHandler} for this purpose.<br>
 * Supported parameters:
 * <ul>
 * <li>input (optional): (scenario relative) path to a .gft file; if set this file will be shown in the feature view</li>
 * <li>gml (optional): (scenario relative) path to a .gml file; if set, the root feature of this file will be shown in
 * the feature view</li>
 * <li>viewTitle (optional): If set, the title of the view will be set to this value</li>
 * </ul>
 * One of 'input' or 'gml' must be set.
 * 
 * @author Stefan Kurzbach
 */
public class FeatureViewInputContextHandler extends AbstractHandler
{
  private static final String PARAM_GML = "gml";

  private static final String PARAM_VIEW_TITLE = "viewTitle";

  private final String m_featureViewInput;

  private final String m_gmlPath;

  private final String m_viewTitle;

  /**
   * Creates a new {@link FeatureViewInputContextHandler} that loads the given input file
   */
  public FeatureViewInputContextHandler( final Properties properties )
  {
    m_featureViewInput = properties.getProperty( KalypsoContextHandlerFactory.PARAM_INPUT, null );
    m_gmlPath = properties.getProperty( PARAM_GML, null );
    m_viewTitle = properties.getProperty( PARAM_VIEW_TITLE, null );

    Assert.isTrue( m_featureViewInput != null || m_gmlPath != null, "Parameter 'input' not set for featureViewInputContext" );
  }

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @SuppressWarnings("unchecked")
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();

    final IFolder szenarioFolder = (IFolder) context.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
    // TODO: that is strange and probably bug-prone. Why not just use scenario-relative paths for the .gft file?
    final IFolder folder = m_featureViewInput == null ? null : SzenarioDataProvider.findModelContext( szenarioFolder, m_featureViewInput );
    // TODO: directly throw exceptions if something is missing
    final IFile file;
    if( folder != null && m_featureViewInput != null )
      file = folder.getFile( m_featureViewInput );
    else
      file = null;

    final IWorkbenchWindow window = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    final IWorkbenchPage page = window == null ? null : window.getActivePage();
    final IViewPart view = page == null ? null : page.findView( FeatureTemplateView.ID );

    if( !(view instanceof FeatureTemplateView) )
      throw new ExecutionException( "FeatureTemplateView not found. Must be open in order to execute this context." );

    final String gmlPath = m_gmlPath;
    if( file == null && gmlPath == null )
      throw new ExecutionException( "Unable to find .gft file: " + m_featureViewInput );

    final FeatureTemplateView featureView = (FeatureTemplateView) view;
    final String viewTitle = m_viewTitle;

    final UIJob job = new UIJob( "FeatureView wird geöffnet" )
    {
      @Override
      public IStatus runInUIThread( IProgressMonitor monitor )
      {
        try
        {
          final Featuretemplate template;
          final URL urlContext;
          // either the file is set OR we have a gmlPath (was checked above)
          if( file == null )
          {
            // if we have a gmlPath we create a pseudo template here
            template = GisTemplateHelper.OF_FEATUREVIEW.createFeaturetemplate();
            template.setName( "Feature View" );
            Layer layer = GisTemplateHelper.OF_FEATUREVIEW.createFeaturetemplateLayer();
            layer.setHref( gmlPath );
            layer.setLinktype( "gml" );
            layer.setFeaturePath( "#fid#root" ); // always use root feature; maybe get from parameter some day
            template.setLayer( layer );
            urlContext = ResourceUtilities.createURL( szenarioFolder );
          }
          else
          {
            template = GisTemplateHelper.loadGisFeatureTemplate( file );
            urlContext = ResourceUtilities.createURL( file );
          }

          if( viewTitle != null )
            template.setName( viewTitle );

          featureView.setTemplate( template, urlContext, null, null, null );

          return Status.OK_STATUS;
        }
        catch( Throwable e )
        {
          return StatusUtilities.statusFromThrowable( e );
        }
      }
    };
    job.schedule();

    return Status.OK_STATUS;
  }
}
