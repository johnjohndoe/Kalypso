package org.kalypso.ogc.sensor.impl;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.util.xml.xlink.IXlink;

/**
 * @author schlienger
 */
public class SimpleObservation implements IObservation
{
  private final String m_name;

  private boolean m_editable;

  private final IXlink m_target;

  private final MetadataList m_metadata;

  private final IAxis[] m_axes;

  private ITuppleModel m_values = null;

  public SimpleObservation( final String name, final boolean editable, final IXlink target,
      final MetadataList metadata, final IAxis[] axes )
  {
    m_name = name;
    m_editable = editable;
    m_target = target;
    m_metadata = metadata;
    m_axes = axes;

    m_values = new SimpleTuppleModel( axes );
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getName()
   */
  public String getName()
  {
    return m_name;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#isEditable()
   */
  public boolean isEditable()
  {
    return m_editable;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getTarget()
   */
  public IXlink getTarget()
  {
    return m_target;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getMetadataList()
   */
  public MetadataList getMetadataList()
  {
    return m_metadata;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getAxisList()
   */
  public IAxis[] getAxisList()
  {
    return m_axes;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getValues(org.kalypso.util.runtime.IVariableArguments)
   */
  public ITuppleModel getValues( IVariableArguments args ) throws SensorException
  {
    if( m_values == null )
      throw new SensorException( "Keine Werte vorhanden." );

    return m_values;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#setValues(org.kalypso.ogc.sensor.ITuppleModel)
   */
  public void setValues( ITuppleModel values ) throws SensorException
  {
    if( values == null )
      throw new SensorException( "Null nicht erlaubt." );

    m_values = values;
  }
}