package org.kalypso.ogc.sensor.zml;

import org.kalypso.java.reflect.ClassUtilities;
import org.kalypso.ogc.sensor.DefaultAxis;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.values.IZmlValuesLoader;
import org.kalypso.ogc.sensor.zml.values.IZmlValuesProvider;
import org.kalypso.ogc.sensor.zml.values.ZmlTuppleModel;
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
        getDataClass( axisType.getDatatype() ), false, position );

    m_axisType = axisType;
  }
  
  private static Class getDataClass( String dataType )
  {
    Class c = null;
    try
    {
      c = ClassUtilities.typeToClass( dataType, null );
    }
    catch( ClassNotFoundException e )
    {
      e.printStackTrace();
    }
    
    return c;
  }

  public AxisType getAxisType()
  {
    return m_axisType;
  }
  
  public void fetchValues(ZmlTuppleModel model) throws SensorException
  {
    IZmlValuesLoader loader = ZmlValueFactory.createLoader( m_axisType, this );
    loader.setModel( model );
    m_values = loader.load( );
  }
  
  public IZmlValuesProvider getValues()
  {
    return m_values;
  }
}
