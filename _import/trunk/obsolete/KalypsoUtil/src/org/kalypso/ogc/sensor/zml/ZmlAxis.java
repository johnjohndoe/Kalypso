package org.kalypso.ogc.sensor.zml;

import org.kalypso.java.reflect.ClassUtilities;
import org.kalypso.ogc.sensor.DefaultAxis;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.values.IZmlValuesProvider;
import org.kalypso.ogc.sensor.zml.values.ZmlValueFactory;
import org.kalypso.zml.AxisType;

/**
 * Wrapper über der Zml-AxisType
 * 
 * @author schlienger
 */
public class ZmlAxis extends DefaultAxis
{
  private final AxisType m_axisType;
  private IZmlValuesProvider m_values = null;

  public ZmlAxis( final AxisType axisType, final int position ) throws ClassNotFoundException
  {
    super( axisType.getName(), axisType.getUnit(),
        ClassUtilities.typeToClass( axisType.getDatatype(), null ), false, position );

    m_axisType = axisType;
  }

  public AxisType getAxisType()
  {
    return m_axisType;
  }
  
  public void fetchValues() throws SensorException
  {
    m_values = ZmlValueFactory.createLoader( m_axisType, this ).load( );
  }
  
  public IZmlValuesProvider getValues()
  {
    return m_values;
  }
}
