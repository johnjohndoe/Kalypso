package org.kalypso.ogc.sensor.zml;

import java.text.ParseException;
import java.util.List;
import java.util.Vector;

import org.kalypso.ogc.sensor.DefaultAxis;
import org.kalypso.util.xml.SchemaUtils;
import org.kalypso.zml.AxisType;

/**
 * Wrapper über der Zml-AxisType
 * 
 * @author schlienger
 */
public class ZmlAxis extends DefaultAxis
{
  private final AxisType m_axisType;

  public ZmlAxis( final AxisType axisType, final int position )
  {
    super( axisType.getName(), axisType.getUnit(),
        SchemaUtils.typeToClass( axisType.getDatatype() ), false, position );

    m_axisType = axisType;
  }

  public AxisType getAxisType()
  {
    return m_axisType;
  }
  
  public List getValues( )
  {
    try
    {
      return ZmlFactory.createLoader( m_axisType ).load( this );
    }
    catch( ParseException e )
    {
      e.printStackTrace();
      return new Vector();
    }
  }
}
