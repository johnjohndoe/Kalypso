package org.kalypso.ogc.sensor.tableview.impl;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Iterator;
import java.util.List;
import java.util.TreeMap;

import javax.xml.bind.JAXBException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.tableview.rules.RenderingRule;
import org.kalypso.template.obstableview.ObjectFactory;
import org.kalypso.template.obstableview.ObstableviewType;
import org.kalypso.template.obstableview.TypeColumn;
import org.kalypso.template.obstableview.TypeObservation;
import org.kalypso.template.obstableview.TypeRenderingRule;
import org.kalypso.template.obstableview.ObstableviewType.RulesType;
import org.kalypso.ui.KalypsoGisPlugin;
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

  private final TreeMap m_keys;

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

    m_pool = KalypsoGisPlugin.getDefault().getPool( );
    m_keys = new TreeMap( m_pool.getKeyComparator() );

    final List list = obsTableView.getObservation();
    for( final Iterator it = list.iterator(); it.hasNext(); )
    {
      final TypeObservation tobs = (TypeObservation) it.next();

      final PoolableObjectType key = new PoolableObjectType( tobs.getLinktype(), tobs
          .getHref(), context );
      m_keys.put( key, tobs );
      m_pool.addPoolListener( this, key );
    }

    final RulesType trules = obsTableView.getRules();
    if( trules != null )
    {
      for( final Iterator it = trules.getRenderingrule().iterator(); it
          .hasNext(); )
        addRule( RenderingRule.createRenderingRule( (TypeRenderingRule) it
            .next() ) );
    }
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

  /**
   * @see org.kalypso.ogc.sensor.tableview.impl.DefaultTableViewTemplate#dispose()
   */
  public void dispose( )
  {
    super.dispose();

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
      final TypeObservation tobs = (TypeObservation) m_keys.get( key );

      final List cols = tobs.getColumn();
      for( Iterator itCols = cols.iterator(); itCols.hasNext(); )
      {
        final TypeColumn col = (TypeColumn) itCols.next();

        addColumn( new DefaultTableViewColumn( col.getAxis(), col.isEditable(),
            col.getWidth(), (IObservation) newValue ) );
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
}