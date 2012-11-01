/**
 *
 */
package org.kalypso.kalypso1d2d.pjt.actions;

import java.net.URL;
import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.template.featureview.Featuretemplate;
import org.kalypso.ui.editor.featureeditor.FeatureTemplateView;

/**
 * Opens the feature view on a given template
 *
 * @author Stefan Kurzbach
 */
public class OpenFeatureViewCommandHandler extends AbstractHandler implements IExecutableExtension
{
  private static final String PARAM_RESOURCE = "org.kalypso.kalypso1d2d.pjt.OpenFeatureViewCommand.resource"; //$NON-NLS-1$

  private String m_resource;

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();

    try
    {
      if( m_resource == null )
        throw new ExecutionException( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.OpenFeatureViewCommandHandler.4" ) ); //$NON-NLS-1$

      final IWorkbenchWindow activeWorkbenchWindow = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
      if( activeWorkbenchWindow == null )
        throw new ExecutionException( Messages.getString("org.kalypso.kalypso1d2d.pjt.actions.OpenFeatureViewCommandHandler.0") ); //$NON-NLS-1$

      final IFolder szenarioFolder = ScenarioHelper.getScenarioFolder();

      final IContainer folder = ScenarioHelper.findModelContext( szenarioFolder, m_resource );
      if( folder == null )
        throw new ExecutionException( Messages.getString("org.kalypso.kalypso1d2d.pjt.actions.OpenFeatureViewCommandHandler.1", m_resource )); //$NON-NLS-1$

      final IFile file = folder.getFile( Path.fromPortableString( m_resource ) );
      if( !file.exists() )
        throw new ExecutionException( Messages.getString("org.kalypso.kalypso1d2d.pjt.actions.OpenFeatureViewCommandHandler.2" , file.getFullPath() )); //$NON-NLS-1$

      final IWorkbenchPage workbenchPage = activeWorkbenchWindow.getActivePage();
      final FeatureTemplateView featureView = (FeatureTemplateView)workbenchPage.showView( FeatureTemplateView.ID );

      final UIJob job = new UIJob( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.OpenFeatureViewCommandHandler.3" ) + file.getName() ) //$NON-NLS-1$
      {
        @Override
        public IStatus runInUIThread( final IProgressMonitor monitor )
        {
          try
          {
            final Featuretemplate template = GisTemplateHelper.loadGisFeatureTemplate( file );

            final URL urlContext = ResourceUtilities.createURL( file );

            featureView.setTemplate( template, urlContext, null, null, null );

            return Status.OK_STATUS;
          }
          catch( final Throwable e )
          {
            return StatusUtilities.statusFromThrowable( e );
          }
        }
      };
      job.schedule();
    }
    catch( final CoreException e )
    {
      throw new ExecutionException(Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.OpenFeatureViewCommandHandler.5"  , m_resource), e ); //$NON-NLS-1$
    }

    return Status.OK_STATUS;
  }

  /**
   * @see org.eclipse.core.runtime.IExecutableExtension#setInitializationData(org.eclipse.core.runtime.IConfigurationElement,
   *      java.lang.String, java.lang.Object)
   */
  @Override
  public void setInitializationData( final IConfigurationElement config, final String propertyName, final Object data )
  {
    if( data instanceof Map )
    {
      final Map< ? , ? > parameterMap = (Map< ? , ? >) data;
      m_resource = (String) parameterMap.get( PARAM_RESOURCE );
    }
  }
}
