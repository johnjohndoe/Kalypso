package org.kalypso.ogc.sensor.template;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.factory.FactoryException;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * AbstractViewTemplate
 * 
 * @author schlienger
 */
public abstract class AbstractViewTemplate extends AbstractTemplateEventProvider implements
    IThemeEventListener
{
  private final List m_themes = new ArrayList();

  /**
   * If set, the Method addObservation ignores all axes with the given type Must
   * be one of TimeserieConstants.TYPE_...
   */
  private String m_ignoreType;

  public String getIgnoreType()
  {
    return m_ignoreType;
  }

  public void setIgnoreType( final String ignoreType )
  {
    synchronized( m_themes )
    {
      m_ignoreType = ignoreType;

      for( final Iterator it = getThemes().iterator(); it.hasNext(); )
        ( (AbstractObservationTheme)it.next() ).setIgnoreType( m_ignoreType );
    }
  }

  public void waitUntilLoaded( final int sleeptime, final int maxloops )
      throws InterruptedException, IllegalStateException
  {
    synchronized( m_themes )
    {
      for( int i = 0; i < maxloops; i++ )
      {
        if( !isLoading() )
          return;
        Thread.sleep( sleeptime );
      }

      throw new IllegalStateException( "Template not loaded: time out" );
    }
  }

  /**
   * @return true when at least one theme is still loading
   */
  public boolean isLoading()
  {
    synchronized( m_themes )
    {
      for( Iterator it = getThemes().iterator(); it.hasNext(); )
      {
        AbstractObservationTheme element = (AbstractObservationTheme)it.next();

        if( element.isLoading() )
          return true;
      }

      return false;
    }
  }

  /**
   * Adds a theme.
   * 
   * @param theme
   */
  public void addTheme( final AbstractObservationTheme theme )
  {
    synchronized( m_themes )
    {
      theme.addListener( this );
      theme.setIgnoreType( m_ignoreType );
      m_themes.add( theme );

      fireTemplateChanged( new TemplateEvent( theme, TemplateEvent.TYPE_ADD ) );
    }
  }

  /**
   * Removes a theme.
   * 
   * @param theme
   */
  public void removeTheme( final AbstractObservationTheme theme )
  {
    synchronized( m_themes )
    {
      if( m_themes.remove( theme ) )
      {
        theme.removeListener( this );
        fireTemplateChanged( new TemplateEvent( theme, TemplateEvent.TYPE_REMOVE ) );
        theme.dispose();
      }
    }
  }

  /**
   * Removes all the themes and fires event
   */
  public void removeAllThemes()
  {
    synchronized( m_themes )
    {
      if( m_themes.size() > 0 )
      {
        final Iterator it = m_themes.iterator();
        while( it.hasNext() )
          ( (AbstractObservationTheme)it.next() ).dispose();

        m_themes.clear();

        fireTemplateChanged( new TemplateEvent( this, null, TemplateEvent.TYPE_REMOVE_ALL ) );
      }
    }
  }

  public void dispose()
  {
    removeAllThemes();
  }

  public Collection getThemes()
  {
    return m_themes;
  }

  /**
   * Sets the observation for this template. Can be overriden. Default impl
   * removes all themes and calls addObservation( obs, args ).
   * 
   * @param obs
   * @param args
   */
  public void setObservation( final IObservation obs, final IVariableArguments args )
  {
    removeAllThemes();

    addObservation( obs, args );
  }

  /**
   * Adds an observation to this template
   * 
   * @param obs
   * @param args
   */
  public abstract void addObservation( final IObservation obs, final IVariableArguments args );

  /**
   * Convenienve method for adding an observation to this template.
   * 
   * @param themeName
   *          used as part of the col name if not null
   * @param context
   * @param href
   * @param linktype
   * @param ignoreExceptions
   * @param args
   * @return theme just being added
   */
  public abstract AbstractObservationTheme addObservation( final String themeName,
      final URL context, final String href, final String linktype, final boolean ignoreExceptions,
      final IVariableArguments args );

  /**
   * Saves the given obs using the pool.
   * 
   * @param obs
   * @param monitor
   * @throws FactoryException
   * @throws LoaderException
   */
  public void saveObservation( final IObservation obs, final IProgressMonitor monitor )
      throws LoaderException, FactoryException
  {
    KalypsoGisPlugin.getDefault().getPool().saveObject( obs, monitor );
  }

  /**
   * @see org.kalypso.ogc.sensor.template.IThemeEventListener#onThemeChanged(org.kalypso.ogc.sensor.template.AbstractObservationTheme)
   */
  public void onThemeChanged( final AbstractObservationTheme theme )
  {
    fireTemplateChanged( new TemplateEvent( theme, TemplateEvent.TYPE_REFRESH ) );
  }
}