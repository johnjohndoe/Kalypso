package org.kalypso.repository.virtual;

import java.io.File;

import javax.xml.bind.JAXBException;

import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.repository.AbstractRepository;
import org.kalypso.repository.IRepositoryFactory;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;
import org.kalypso.zml.repository.virtual.ObjectFactory;
import org.kalypso.zml.repository.virtual.VirtualRepositoryType;
import org.xml.sax.InputSource;

/**
 * VirtualRepository
 * 
 * @author schlienger
 */
public class VirtualRepository extends AbstractRepository
{
  private static final ObjectFactory OF = new ObjectFactory();
  
  /**
   * @param factory
   */
  public VirtualRepository( final IRepositoryFactory factory, final InputSource specSource ) throws SensorException
  {
    super( factory );
    
    try
    {
      final VirtualRepositoryType vrt = (VirtualRepositoryType) OF.createUnmarshaller().unmarshal( specSource );
      
      vrt.getLevel();
    }
    catch( JAXBException e )
    {
      e.printStackTrace();
      throw new SensorException( e );
    }
  }

  /**
   * @param factory
   * @param location
   * @param readOnly
   */
  public VirtualRepository( IRepositoryFactory factory, String location,
      boolean readOnly )
  {
    super( factory, location, readOnly );
    // TODO Auto-generated constructor stub
  }

  /**
   * @see org.kalypso.repository.IRepository#findItem(java.lang.String)
   */
  public IRepositoryItem findItem( String id ) throws RepositoryException
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.repository.IRepository#reload()
   */
  public void reload( ) throws RepositoryException
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getIdentifier()
   */
  public String getIdentifier( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren( ) throws RepositoryException
  {
    // TODO Auto-generated method stub
    return false;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren( ) throws RepositoryException
  {
    // TODO Auto-generated method stub
    return null;
  }

}
