package org.kalypso.ogc.sensor.zml;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.zml.values.ZmlTuppleModel;
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.util.xml.xlink.IXlink;
import org.kalypso.util.xml.xlink.JAXBXLink;
import org.kalypso.zml.AxisType;
import org.kalypso.zml.MetadataType;
import org.kalypso.zml.ObjectFactory;
import org.kalypso.zml.Observation;
import org.xml.sax.InputSource;

/**
 * A class that represents a zml based IObservation. The format is zml which is
 * defined in the observation.xsd schema file.
 * 
 * @author schlienger
 */
public class ZmlObservation implements IObservation
{
  protected final static ObjectFactory m_zmlObjectFactory = new ObjectFactory();

  private final URL m_url;
  
  private final String m_identifier;

  private final SimpleObservation m_observation;

  private final ZmlAxis[] m_axes;

  private ZmlTuppleModel m_model = null;

  /**
   * Constructor using a <code>File</code> object.
   * 
   * @throws MalformedURLException
   * @throws JAXBException
   * @throws SensorException
   * @throws IOException
   */
  public ZmlObservation( final File file ) throws MalformedURLException, JAXBException,
      IOException, SensorException
  {
    this( file.toURL(), file.getAbsolutePath() );
  }

  /**
   * Constructor using URL.
   * 
   * @throws JAXBException
   * @throws IOException
   * @throws SensorException
   */
  public ZmlObservation( final URL url, final String identifier ) throws JAXBException, IOException, SensorException
  {
    m_identifier = identifier;
    m_url = url;

    Unmarshaller u = m_zmlObjectFactory.createUnmarshaller();

    // unmarshal and close stream
    final InputStream inputStream = m_url.openStream();
    final Observation obs;
    
    try
    {
      obs = (Observation)u.unmarshal( new InputSource( inputStream ) );
    }
    catch( JAXBException e )
    {
      throw new SensorException( "Error while unmarshalling: " + url.toExternalForm(), e );
    }
    
    inputStream.close();

    // metadata
    final MetadataList metadata = new MetadataList();
    metadata.put( MetadataList.MD_NAME, obs.getName() );

    if( obs.getMetadataList() != null )
    {
      final List mdList = obs.getMetadataList().getMetadata();

      for( final Iterator it = mdList.iterator(); it.hasNext(); )
      {
        final MetadataType md = (MetadataType)it.next();

        metadata.put( md.getName(), md.getValue() );
      }
    }

    // axes
    final List tmpList = obs.getAxis();
    final List axisList = new ArrayList( tmpList.size() );

    for( int i = 0; i < tmpList.size(); i++ )
    {
      final AxisType tmpAxis = (AxisType)tmpList.get( i );

      axisList.add( new ZmlAxis( tmpAxis, i ) );
    }

    m_axes = (ZmlAxis[])axisList.toArray( new ZmlAxis[axisList.size()] );

    m_observation = new SimpleObservation( obs.getName(), obs.isEditable(), new JAXBXLink( obs
        .getTarget() ), metadata, (IAxis[])axisList.toArray( new IAxis[0] ) );
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getValues(org.kalypso.util.runtime.IVariableArguments)
   */
  public synchronized ITuppleModel getValues( final IVariableArguments args )
      throws SensorException
  {
    if( m_model == null )
      m_model = new ZmlTuppleModel( m_url, m_axes );

    return m_model;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#setValues(org.kalypso.ogc.sensor.ITuppleModel)
   */
  public void setValues( final ITuppleModel values )
  {
  // TODO
  }

  public synchronized IAxis[] getAxisList()
  {
    return m_observation.getAxisList();
  }

  public MetadataList getMetadataList()
  {
    return m_observation.getMetadataList();
  }

  public String getName()
  {
    return m_observation.getName();
  }

  public IXlink getTarget()
  {
    return m_observation.getTarget();
  }

  public boolean isEditable()
  {
    return m_observation.isEditable();
  }

  public String toString()
  {
    return m_observation.toString();
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getIdentifier()
   */
  public String getIdentifier()
  {
    return m_identifier;
  }
}