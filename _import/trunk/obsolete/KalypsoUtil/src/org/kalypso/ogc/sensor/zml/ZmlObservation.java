package org.kalypso.ogc.sensor.zml;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.Unmarshaller;

import org.kalypso.ogc.sensor.DefaultTarget;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITarget;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.Metadata;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.values.ZmlTuppleModel;
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.zml.AxisType;
import org.kalypso.zml.MetadataType;
import org.kalypso.zml.ObjectFactory;
import org.kalypso.zml.Observation;
import org.kalypso.zml.TargetPropertyType;

/**
 * A class that represents a zml based IObservation. The format is zml which is
 * defined in the observation.xsd schema file.
 * 
 * @author schlienger
 */
public class ZmlObservation implements IObservation
{
  private Observation m_obsFile = null;

  private Metadata m_metadata = null;

  private ZmlAxis[] m_axisList = null;

  private DefaultTarget m_target = null;

  private ZmlTuppleModel m_model = null;

  private final static ObjectFactory m_zmlObjectFactory = new ObjectFactory();

  private final String m_sourceName;

  private final InputStream m_inputStream;

  public ZmlObservation( final File file ) throws FileNotFoundException
  {
    this( file.getName(), new FileInputStream( file ) );
  }

  public ZmlObservation( final String sourceName, final InputStream inputStream )
  {
    m_sourceName = sourceName;
    m_inputStream = inputStream;
  }

  /**
   * Returns the name of the source of this ZmlObservation.
   */
  public String getSourceName()
  {
    return m_sourceName;
  }

  /**
   * Helper that loads the file
   */
  private Observation getObservation()
  {
    if( m_obsFile == null )
    {
      try
      {
        Unmarshaller u = m_zmlObjectFactory.createUnmarshaller();

        m_obsFile = (Observation)u.unmarshal( m_inputStream );
        
        m_inputStream.close();
      }
      catch( Exception e )
      {
        // TODO: how to handle this correctly?
        e.printStackTrace();
      }
    }

    return m_obsFile;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getName()
   */
  public String getName()
  {
    return getObservation().getName();
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getTarget()
   */
  public ITarget getTarget()
  {
    if( m_target == null )
    {
      TargetPropertyType tp = getObservation().getTargetProperty();
      m_target = new DefaultTarget( tp.getSource(), tp.getType(), tp.getObjectid() );
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

      Observation obs = getObservation();

      m_metadata.put( Metadata.MD_NAME, obs.getName() );

      List mdList = obs.getMetadataList().getMetadata();

      for( Iterator it = mdList.iterator(); it.hasNext(); )
      {
        MetadataType md = (MetadataType)it.next();

        m_metadata.put( md.getName(), md.getValue() );
      }
    }

    return m_metadata;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getAxisList()
   */
  public IAxis[] getAxisList()
  {
    if( m_axisList == null )
    {
      List tmpList = getObservation().getAxis();

      m_axisList = new ZmlAxis[tmpList.size()];

      for( int i = 0; i < m_axisList.length; i++ )
      {
        AxisType tmpAxis = (AxisType)tmpList.get( i );

        try
        {
          m_axisList[i] = new ZmlAxis( tmpAxis, i );
        }
        catch( SensorException e )
        {
          // TODO: besseres Handling?
          throw new RuntimeException( e );
        }
      }
    }

    return m_axisList;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getValues(org.kalypso.util.runtime.IVariableArguments)
   */
  public ITuppleModel getValues( IVariableArguments args ) throws SensorException
  {
    if( m_model == null )
    {
      getAxisList();
      m_model = new ZmlTuppleModel( m_axisList );
    }

    return m_model;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#setValues(org.kalypso.ogc.sensor.ITuppleModel)
   */
  public void setValues( ITuppleModel values )
  {
  // TODO
  }
}