/**
 *
 */
package org.kalypso.afgui.handlers;

import java.net.URL;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
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
import org.kalypso.ui.editor.featureeditor.FeatureTemplateView;

import de.renew.workflow.connector.cases.CaseHandlingSourceProvider;

/**
 * Loads a template file in the current feature view. Requires that the current context contains the feature view. Use a
 * {@link ViewContextHandler} for this purpose.
 * 
 * @author Stefan Kurzbach
 */
public class FeatureViewInputContextHandler extends AbstractHandler implements IExecutableExtension
{
  public static final String FEATUREVIEW_INPUT = "org.kalypso.kalypso1d2d.pjt.contexts.featureViewInput"; //$NON-NLS-1$

  private String m_featureViewInput;

  /**
   * Creates a new {@link FeatureViewInputContextHandler} that loads the given input file
   */
  public FeatureViewInputContextHandler( final String input )
  {
    m_featureViewInput = input;
  }

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @SuppressWarnings("unchecked")
  @Override
  public Object execute( final ExecutionEvent event )
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();

    final IFolder szenarioFolder = (IFolder) context.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
    // TODO: that is strange and probably bug-prone. Why not just use scenario-relative paths for the .gft file?
    final IFolder folder = SzenarioDataProvider.findModelContext( szenarioFolder, m_featureViewInput );
    // TODO: directly throw exceptions if something is missing
    final IFile file;
    if( folder != null && m_featureViewInput != null )
      file = folder.getFile( m_featureViewInput );
    else
      file = null;

    final IWorkbenchWindow window = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    final IWorkbenchPage page = window == null ? null : window.getActivePage();
    final IViewPart view = page == null ? null : page.findView( FeatureTemplateView.ID );

    if( file != null && file.exists() && view != null && view instanceof FeatureTemplateView )
    {
      final FeatureTemplateView mapView = (FeatureTemplateView) view;

      final UIJob job = new UIJob( "Loading " + file.getName() )
      {
        @Override
        public IStatus runInUIThread( IProgressMonitor monitor )
        {
          try
          {
            Featuretemplate template = GisTemplateHelper.loadGisFeatureTemplate( file );

            URL urlContext = ResourceUtilities.createURL( file );

            mapView.setTemplate( template, urlContext, null, null, null );

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
    else
    {
      return Status.CANCEL_STATUS;
    }
  }

  /**
   * @see org.eclipse.core.runtime.IExecutableExtension#setInitializationData(org.eclipse.core.runtime.IConfigurationElement,
   *      java.lang.String, java.lang.Object)
   */
  public void setInitializationData( final IConfigurationElement config, final String propertyName, final Object data )
  {
    if( data instanceof Map )
    {
      final Map parameterMap = (Map) data;
      m_featureViewInput = (String) parameterMap.get( FEATUREVIEW_INPUT );
    }
  }
}
