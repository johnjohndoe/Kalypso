package org.kalypso.ogc.sensor.zml;

import java.io.File;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.kalypso.ogc.sensor.DefaultTarget;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITarget;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.Metadata;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.util.repository.file.FileItem;
import org.kalypso.util.repository.file.FileRepository;
import org.kalypso.zml.AxisType;
import org.kalypso.zml.MetadataType;
import org.kalypso.zml.ObjectFactory;
import org.kalypso.zml.Observation;
import org.kalypso.zml.TargetPropertyType;

/**
 * An Observation from a local zml-File.
 * 
 * @author schlienger
 */
public class ZmlObservationItem extends FileItem implements IObservation
{
  private final static ObjectFactory m_zmlObjectFactory = new ObjectFactory();

  private Observation m_obsFile = null;

  private Metadata m_metadata = null;

  private List m_axisList = null;

  private DefaultTarget m_target = null;

  private ZmlTuppleModel m_model = null;

  public ZmlObservationItem( final FileRepository rep, final File file )
  {
    super( rep, file );
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
    if( m_target == null )
    {
      try
      {
        TargetPropertyType tp = getObservation().getTargetProperty();
        m_target = new DefaultTarget( tp.getSource(), tp.getType(), tp.getObjectid() );
      }
      catch( JAXBException e )
      {
        e.printStackTrace();
        return null;
      }
    }
    
    return m_target;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getMetadata()
   */
  public Metadata getMetadata()
  {
    if( m_metadata == null )
    {
      m_metadata = new Metadata();

      try
      {
        Observation obs = getObservation();
        
        m_metadata.put( Metadata.MD_NAME, obs.getName() );
        m_metadata.put( "Filename", getName() );
        m_metadata.put( "Folder", getParent() );

        List mdList = obs.getMetadataList().getMetadata();

        for( Iterator it = mdList.iterator(); it.hasNext(); )
        {
          MetadataType md = (MetadataType)it.next();

          m_metadata.put( md.getName(), md.getValue() );
        }
      }
      catch( JAXBException e )
      {
        e.printStackTrace();
      }
    }

    return m_metadata;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getAxisList()
   */
  public List getAxisList()
  {
    if( m_axisList == null )
    {
      try
      {
        List tmpList = getObservation().getAxis();
        
        m_axisList = new Vector( tmpList.size() );
        
        int axisPosition = 0;
        for( Iterator it = tmpList.iterator(); it.hasNext(); )
        {
          AxisType tmpAxis = (AxisType)it.next();

          m_axisList.add( new ZmlAxis( tmpAxis, axisPosition ) );

          axisPosition++;
        }
      }
      catch( JAXBException e )
      {
        e.printStackTrace();
        return null;
      }
    }

    return m_axisList;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getValues(java.util.Date,
   *      java.util.Date)
   */
  public ITuppleModel getValues( Date from, Date to ) throws SensorException
  {
    if( m_model == null )
      m_model = new ZmlTuppleModel( getAxisList() );
    
    return m_model;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#setValues(org.kalypso.ogc.sensor.ITuppleModel)
   */
  public void setValues( ITuppleModel values ) throws SensorException
  {
    // TODO
  }
}
