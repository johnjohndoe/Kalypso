package org.kalypso.ogc.sensor.tableview.impl;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.TreeMap;

import javax.xml.bind.JAXBException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.rules.RenderingRule;
import org.kalypso.template.obstableview.ObjectFactory;
import org.kalypso.template.obstableview.ObstableviewType;
import org.kalypso.template.obstableview.TypeColumn;
import org.kalypso.template.obstableview.TypeObservation;
import org.kalypso.template.obstableview.TypeRenderingRule;
import org.kalypso.template.obstableview.ObstableviewType.RulesType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.factory.FactoryException;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;

/**
 * A DefaultTableViewTemplate based on a XML template file. Performs the load
 * operation.
 * 
 * @author schlienger
 */
public class LinkedTableViewTemplate extends DefaultTableViewTemplate implements
    IPoolListener
{
  private final static ObjectFactory m_obsTableFactory = new ObjectFactory();

  private final ResourcePool m_pool;

  private final TreeMap m_key2cols;

  private final Hashtable m_col2key;

  /**
   * Constructor
   * 
   * @param obsTableView
   * @param context
   */
  public LinkedTableViewTemplate( final ObstableviewType obsTableView,
      final URL context )
  {
    super();

    m_pool = KalypsoGisPlugin.getDefault().getPool();
    m_key2cols = new TreeMap( m_pool.getKeyComparator() );
    m_col2key = new Hashtable();

    final RulesType trules = obsTableView.getRules();
    if( trules != null )
    {
      for( final Iterator it = trules.getRenderingrule().iterator(); it
          .hasNext(); )
        addRule( RenderingRule.createRenderingRule( (TypeRenderingRule) it
            .next() ) );
    }

    final List list = obsTableView.getObservation();
    for( final Iterator it = list.iterator(); it.hasNext(); )
    {
      final TypeObservation tobs = (TypeObservation) it.next();

      final List cols = new ArrayList();
      final List tcols = tobs.getColumn();
      for( Iterator itCols = tcols.iterator(); itCols.hasNext(); )
      {
        final TypeColumn tcol = (TypeColumn) itCols.next();

        final DefaultTableViewColumn col = new DefaultTableViewColumn( tcol
            .getAxis(), tcol.isEditable(), tcol.getWidth(), tcol.getAxis(), null );

        cols.add( col );
      }

      final PoolableObjectType key = new PoolableObjectType(
          tobs.getLinktype(), tobs.getHref(), context );

      addObservationTheme( key, cols );
    }
  }

  /**
   * Adds a list of columns as an observation theme
   * 
   * @param key
   * @param cols
   */
  public void addObservationTheme( final PoolableObjectType key, final List cols )
  {
    // first record key
    m_key2cols.put( key, cols );

    // store ref col to key, used in removeColumn( ... )
    final Iterator it = cols.iterator();
    while( it.hasNext() )
      m_col2key.put( it.next(), key );

    // finally launch request on pool
    m_pool.addPoolListener( this, key );
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
   * @see org.kalypso.ogc.sensor.tableview.impl.DefaultTableViewTemplate#removeAllColumns()
   */
  public void removeAllColumns( )
  {
    super.removeAllColumns();

    m_key2cols.clear();
    m_col2key.clear();
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.impl.DefaultTableViewTemplate#removeColumn(org.kalypso.ogc.sensor.tableview.ITableViewColumn)
   */
  public void removeColumn( final ITableViewColumn column )
  {
    super.removeColumn( column );

    final Object key = m_col2key.get( column );
    final List list = (List) m_key2cols.get( key );
    list.remove( column );
    if( list.isEmpty() )
      m_key2cols.remove( key );

    m_col2key.remove( column );
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.impl.DefaultTableViewTemplate#dispose()
   */
  public void dispose( )
  {
    super.dispose();

    m_key2cols.clear();
    m_col2key.clear();

    m_pool.removePoolListener( this );
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType,
   *      java.lang.Object, org.eclipse.core.runtime.IStatus)
   */
  public void objectLoaded( final IPoolableObjectType key,
      final Object newValue, final IStatus status )
  {
    if( status.isOK() )
    {
      final List cols = (List) m_key2cols.get( key );

      for( final Iterator itCols = cols.iterator(); itCols.hasNext(); )
      {
        final DefaultTableViewColumn col = (DefaultTableViewColumn) itCols
            .next();
        col.setObservation( (IObservation) newValue );
      }
      
      for( final Iterator itCols = cols.iterator(); itCols.hasNext(); )
      {
        addColumn( (ITableViewColumn) itCols.next() );
      }
    }
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType,
   *      java.lang.Object)
   */
  public void objectInvalid( IPoolableObjectType key, Object oldValue )
  {
    // TODO Auto-generated method stub
  }

  /**
   * @param file
   * @return table view template
   * @throws CoreException
   * @throws JAXBException
   * @throws IOException
   */
  public static LinkedTableViewTemplate loadTableViewTemplate( final IFile file )
      throws CoreException, JAXBException, IOException
  {
    final InputStream ins = file.getContents();
    final ObstableviewType baseTemplate = (ObstableviewType) m_obsTableFactory
        .createUnmarshaller().unmarshal( ins );
    ins.close();

    return new LinkedTableViewTemplate( baseTemplate, ResourceUtilities
        .createURL( file ) );
  }
}