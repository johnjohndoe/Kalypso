package org.kalypso.ogc.sensor.tableview.impl;

import java.net.URL;
import java.util.Iterator;
import java.util.List;
import java.util.TreeMap;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.tableview.ITableViewTheme;
import org.kalypso.ogc.sensor.tableview.rules.RulesFactory;
import org.kalypso.template.obstableview.ObstableviewType;
import org.kalypso.template.obstableview.TypeObservation;
import org.kalypso.template.obstableview.TypeRenderingRule;
import org.kalypso.template.obstableview.ObstableviewType.RulesType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.factory.FactoryException;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * A DefaultTableViewTemplate based on a XML template file. Performs the load
 * operation.
 * 
 * @author schlienger
 */
public class LinkedTableViewTemplate extends ObservationTableViewTemplate implements
    IPoolListener
{
  private final ResourcePool m_pool;

  private final TreeMap m_key2themes;

  /**
   * Constructor
   */
  public LinkedTableViewTemplate( )
  {
    super();
    
    m_pool = KalypsoGisPlugin.getDefault().getPool();
    m_key2themes = new TreeMap( m_pool.getKeyComparator() );
  }

  /**
   * Sets the base template and loads the columns.
   * 
   * @param obsTableView
   * @param context
   */
  public void setBaseTemplate( final ObstableviewType obsTableView,
      final URL context )
  {
    final RulesType trules = obsTableView.getRules();
    if( trules != null )
    {
      for( final Iterator it = trules.getRenderingrule().iterator(); it
          .hasNext(); )
        getRules().addRule( RulesFactory.createRenderingRule( (TypeRenderingRule) it
            .next() ) );
    }

    final List list = obsTableView.getObservation();
    for( final Iterator it = list.iterator(); it.hasNext(); )
    {
      final TypeObservation tobs = (TypeObservation) it.next();

      final LinkedTableViewTheme theme = new LinkedTableViewTheme( this, tobs );

      final PoolableObjectType key = new PoolableObjectType(
          tobs.getLinktype(), tobs.getHref(), context );

      startLoading( key, theme );
    }
  }

  /**
   * Adds a list of columns as an observation theme
   * 
   * @param key
   * @param theme
   */
  public void startLoading( final PoolableObjectType key, final ITableViewTheme theme )
  {
    // first record key
    m_key2themes.put( key, theme );

    // finally launch request on pool
    m_pool.addPoolListener( this, key );
  }
  
  /**
   * Convenienve method for adding an observation to this template.
   *
   * TODO: use themeName as name for the column
   * 
   * @param themeName used as part of the col name if not null
   * @param context
   * @param href
   * @param linktype
   * @param ignoreExceptions
   * @param args
   */
  public void addObservation( final String themeName, final URL context, final String href, final String linktype, final boolean ignoreExceptions, final IVariableArguments args )
  {
    // create key according to observation link
    final PoolableObjectType key = new PoolableObjectType( linktype, href, context, ignoreExceptions );

    // fake theme because it won't be added directly to this template
    DefaultTableViewTheme fakeTheme = new DefaultTableViewTheme( themeName );
    fakeTheme.setArguments( args );
    
    // use load mechanism
    startLoading( key, fakeTheme );
  }

  /**
   * Saves the given obs using the pool.
   * 
   * @param obs
   * @param monitor
   * @throws FactoryException
   * @throws LoaderException
   */
  public void saveObservation( final IObservation obs, final IProgressMonitor monitor ) throws LoaderException, FactoryException
  {
    m_pool.saveObject( obs, monitor );
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.impl.DefaultTableViewTemplate#removeAllThemes()
   */
  public void removeAllThemes( )
  {
    m_key2themes.clear();
    
    super.removeAllThemes();
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.impl.DefaultTableViewTemplate#removeTheme(org.kalypso.ogc.sensor.tableview.ITableViewTheme)
   */
  public void removeTheme( ITableViewTheme theme )
  {
    if( m_key2themes.containsValue( theme ) )
    {
      final Iterator it = m_key2themes.keySet().iterator();
      while( it.hasNext() )
      {
        Object key = it.next();
        
        if( m_key2themes.get( key ) == theme )
        {
          m_key2themes.remove( key );
          break;
        }
      }
    }
    
    super.removeTheme( theme );
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.impl.DefaultTableViewTemplate#dispose()
   */
  public void dispose( )
  {
    m_key2themes.clear();
    m_pool.removePoolListener( this );

    super.dispose();
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType,
   *      java.lang.Object, org.eclipse.core.runtime.IStatus)
   */
  public void objectLoaded( final IPoolableObjectType key,
      final Object newValue, final IStatus status )
  {
    if( status.isOK() && newValue != null )
    {
      final DefaultTableViewTheme theme = (DefaultTableViewTheme) m_key2themes.get( key );

      if( theme != null )
      {
        final IObservation obs = (IObservation) newValue;
        
        theme.setObservation( obs );
  
        // was it a fake theme?
        if( theme.getColumns().size() == 0 )
          addObservation( obs, true, theme.getArguments() );
        else
          addTheme( theme );
      }
    }
    else
      m_key2themes.remove( key );
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType,
   *      java.lang.Object)
   */
  public void objectInvalid( IPoolableObjectType key, Object oldValue )
  {
    final ITableViewTheme theme = (ITableViewTheme) m_key2themes.get( key );

    if( theme == null )
      return;
    
    removeTheme( theme );
  }
}