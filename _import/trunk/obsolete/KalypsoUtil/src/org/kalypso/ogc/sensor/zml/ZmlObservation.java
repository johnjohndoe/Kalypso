package org.kalypso.ogc.sensor.zml;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.JAXBException;
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
import org.xml.sax.InputSource;

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

  private ArrayList m_axisList = null;

  private DefaultTarget m_target = null;

  private ZmlTuppleModel m_model = null;

  protected final static ObjectFactory m_zmlObjectFactory = new ObjectFactory();

  private final String m_sourceName;

  private final URL m_url;

  /**
   * Constructor using a <code>File</code> object.
   */
  public ZmlObservation( final File file ) throws MalformedURLException
  {
    this( file.getName(), file.toURL() );
  }

  public ZmlObservation( final String sourceName, final URL url )
  {
    m_sourceName = sourceName;
    m_url = url;
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

        m_obsFile = (Observation)u.unmarshal( new InputSource( m_url.openStream() ) );
      }
      catch( Exception e )
      {
        // TODO: how to handle this correctly?
        throw new RuntimeException( e );
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

      if( obs.getMetadataList() != null )
      {
        List mdList = obs.getMetadataList().getMetadata();

        for( Iterator it = mdList.iterator(); it.hasNext(); )
        {
          MetadataType md = (MetadataType)it.next();

          m_metadata.put( md.getName(), md.getValue() );
        }
      }
    }

    return m_metadata;
  }

  /**
   * TODO: explain why synchronized (jobs for display in table and diagram QV)
   * 
   * @see org.kalypso.ogc.sensor.IObservation#getAxisList()
   */
  public synchronized IAxis[] getAxisList()
  {
    if( m_axisList == null )
    {
      List tmpList = getObservation().getAxis();

      m_axisList = new ArrayList( tmpList.size() );

      for( int i = 0; i < tmpList.size(); i++ )
      {
        AxisType tmpAxis = (AxisType)tmpList.get( i );

        try
        {
          m_axisList.add( new ZmlAxis( tmpAxis, i ) );
        }
        catch( SensorException e )
        {
          // TODO: besseres Handling?
          throw new RuntimeException( e );
        }
      }
    }

    return (ZmlAxis[])m_axisList.toArray( new ZmlAxis[0] );
  }

  /**
   * Adds an axis to this observation. Convenience method that can be used by
   * subclasses.
   * 
   * @throws SensorException
   */
  protected void addAxis( final String name, final String unit, final String dataType,
      final String separator, final String values ) throws SensorException
  {
    try
    {
      AxisType at = m_zmlObjectFactory.createAxisType();
      at.setName( name );
      at.setUnit( unit );
      at.setDatatype( dataType );

      AxisType.ValueArrayType vat = m_zmlObjectFactory.createAxisTypeValueArrayType();
      vat.setSeparator( separator );
      vat.setValue( values );

      at.setValueArray( vat );

      m_axisList.add( new ZmlAxis( at, m_axisList.size() ) );
    }
    catch( JAXBException e )
    {
      throw new SensorException( e );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getValues(org.kalypso.util.runtime.IVariableArguments)
   */
  public ITuppleModel getValues( final IVariableArguments args ) throws SensorException
  {
    if( m_model == null )
      m_model = new ZmlTuppleModel( m_url, (ZmlAxis[])getAxisList() );

    return m_model;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#setValues(org.kalypso.ogc.sensor.ITuppleModel)
   */
  public void setValues( final ITuppleModel values )
  {
  // TODO
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#isEditable()
   */
  public boolean isEditable()
  {
    //getObservation().
    return true;
  }
}