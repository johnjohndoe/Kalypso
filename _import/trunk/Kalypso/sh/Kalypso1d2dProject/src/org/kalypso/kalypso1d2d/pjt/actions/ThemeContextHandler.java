/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.actions;

import java.util.Map;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModellListener;
import org.kalypso.ogc.gml.mapmodel.MapModellAdapter;
import org.kalypso.ui.views.map.MapView;

/**
 * Activates a given theme in the current map view.
 * 
 * @author Stefan Kurzbach
 */
public class ThemeContextHandler extends AbstractHandler implements IExecutableExtension
{
  public static final String CONTEXT_THEME_FEATURE_TYPE = "org.kalypso.kalypso1d2d.pjt.contexts.theme";   //$NON-NLS-1$

  public static final String NO_THEME = "NO_THEME";   //$NON-NLS-1$

  protected final IMapModellListener m_modellListener = new MapModellAdapter()
  {
    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeAdded(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme)
     */
    @Override
    public void themeAdded( IMapModell source, IKalypsoTheme theme )
    {
      maybeActivateTheme( theme );
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#contextChanged(org.kalypso.ogc.gml.mapmodel.IMapModell)
     */
    @Override
    public void themeContextChanged( final IMapModell source, final IKalypsoTheme theme )
    {
      maybeActivateTheme( theme );
    }
  };

  private String m_featureType;

  protected IMapModell m_mapModell;

  public ThemeContextHandler( final String featureType )
  {
    m_featureType = featureType;
  }

  /**
   * @see org.eclipse.core.commands.AbstractHandler#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_mapModell != null )
      m_mapModell.removeMapModelListener( m_modellListener );

    super.dispose();
  }

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @SuppressWarnings("unchecked")   //$NON-NLS-1$
  @Override
  public Object execute( final ExecutionEvent event )
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();

    final IWorkbenchWindow window = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    final IWorkbenchPage activePage = window == null ? null : window.getActivePage();
    final IViewPart view = activePage == null ? null : activePage.findView( MapView.ID );

    if( m_featureType != null && view != null && view instanceof MapView )
    {
      final MapView mapView = (MapView) view;
      final MapPanel mapPanel = (MapPanel) mapView.getAdapter( MapPanel.class );
      final String layerContext = m_featureType;
      final Job job = new Job( org.kalypso.kalypso1d2d.pjt.actions.Messages.getString("ThemeContextHandler.3") )  //$NON-NLS-1$
      {
        @Override
        protected IStatus run( final IProgressMonitor monitor )
        {
          m_mapModell = mapPanel.getMapModell();
          if( m_mapModell != null )
            m_mapModell.addMapModelListener( m_modellListener );
          if( layerContext != null )
          {
            final IKalypsoTheme activeTheme = m_mapModell == null ? null : m_mapModell.getActiveTheme();
            if( activeTheme != null && layerContext.equals( NO_THEME ) )
            {
              m_mapModell.activateTheme( null );
            }
            else if( !layerContext.equals( activeTheme != null ? activeTheme.getContext() : null ) )
            {
              final IKalypsoTheme[] allThemes = m_mapModell.getAllThemes();
              for( final IKalypsoTheme theme : allThemes )
              {
                if( theme.isLoaded() )
                  maybeActivateTheme( theme );
              }
            }
          }

          return Status.OK_STATUS;
        }
      };
      job.setRule( mapView.getSchedulingRule().getActivateLayerSchedulingRule() );
      job.setUser( true );
      job.schedule();
    }

    return Status.OK_STATUS;
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
      m_featureType = (String) parameterMap.get( CONTEXT_THEME_FEATURE_TYPE );
    }
  }

  void maybeActivateTheme( final IKalypsoTheme themeToActivate )
  {
    try
    {
      final String themeContext = themeToActivate.getContext();
      if( m_featureType != null && m_featureType.equals( themeContext ) )
      {
        m_mapModell.activateTheme( themeToActivate );
      }
    }
    catch( final Throwable t )
    {
      t.printStackTrace();
    }
  }
}
