package org.kalypso.ogc.sensor.file;

import java.io.File;
import java.io.FileFilter;
import java.util.Date;
import java.util.List;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITarget;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.Metadata;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.util.repository.file.FileItem;
import org.kalypso.zml.ObjectFactory;
import org.kalypso.zml.Observation;

/**
 * An Observation from a local file.
 * 
 * @author schlienger
 */
public class ObservationFileItem extends FileItem implements IObservation
{
  private final static ObjectFactory m_zmlObjectFactory = new ObjectFactory();
  
  private Observation m_obsFile = null;
  private Metadata m_metadata = null;

  public ObservationFileItem( String fileName, FileFilter filter )
  {
    super( fileName, filter );
  }

  public ObservationFileItem( File file, FileFilter filter )
  {
    super( file, filter );
  }

  /**
   * Helper that loads the file
   */
  private Observation getObservation() throws JAXBException
  {
    if( m_obsFile == null )
    {
      Unmarshaller u = m_zmlObjectFactory.createUnmarshaller();
  
      m_obsFile = (Observation)u.unmarshal( getFile() );
    }
    
    return m_obsFile;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getTarget()
   */
  public ITarget getTarget()
  {
    return null;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getMetadata()
   */
  public Metadata getMetadata()
  {
    if( m_metadata == null )
    {
      m_metadata = new Metadata();

      m_metadata.put( Metadata.MD_NAME, getName() );
      m_metadata.put( Metadata.MD_DESCRIPTION, "in " + getParent() );
    }

    return m_metadata;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getAxisList()
   */
  public List getAxisList()
  {
//    Observation obs = getObservation();
//    
//    for( Iterator iter = obs.getAxis().iterator(); iter.hasNext(); )
//    {
//      AxisType axis = (AxisType)iter.next();
//      
//    }
//    
      return null;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getValues(java.util.Date,
   *      java.util.Date)
   */
  public ITuppleModel getValues( Date from, Date to ) throws SensorException
  {
    return null;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#setValues(org.kalypso.ogc.sensor.ITuppleModel)
   */
  public void setValues( ITuppleModel values ) throws SensorException
  {}
}