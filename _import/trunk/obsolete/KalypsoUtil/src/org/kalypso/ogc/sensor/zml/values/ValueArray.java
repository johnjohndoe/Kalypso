package org.kalypso.ogc.sensor.zml.values;

import java.util.StringTokenizer;
import java.util.Vector;

import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.ZmlAxis;
import org.kalypso.util.parser.ParserException;
import org.kalypso.zml.AxisType;

/**
 * ValueArray where values are inline in the Zml-File.
 * 
 * @author schlienger 
 */
public class ValueArray implements IZmlValuesLoader, IZmlValuesProvider
{  
  private final AxisType.ValueArrayType m_va;
  private final ZmlAxis m_axis;
  
  private Vector m_values = null;

  public ValueArray( final AxisType.ValueArrayType va, final ZmlAxis axis )
  {
    m_va = va;
    m_axis = axis;
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValuesLoader#load()
   */
  public IZmlValuesProvider load( ) throws SensorException
  {
    StringTokenizer stok = new StringTokenizer( m_va.getValue(), m_va.getSeparator() );

    m_values = new Vector( stok.countTokens() );

    try
    {
      while( stok.hasMoreElements() )
        m_values.add( m_axis.getParser().parse( stok.nextToken() ) );

      return this;
    }
    catch( ParserException e )
    {
      throw new SensorException( e );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValuesLoader#setModel(org.kalypso.ogc.sensor.zml.values.ZmlTuppleModel)
   */
  public void setModel( ZmlTuppleModel model )
  {
    // nix
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValuesProvider#getCount()
   */
  public int getCount()
  {
    return m_values.size();
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValuesProvider#getElement(int)
   */
  public Object getElement( int index )
  {
    return m_values.get( index );
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValuesProvider#setElement(int, java.lang.Object)
   */
  public void setElement( int index, Object element )
  {
    m_values.set( index, element );
  }
}
