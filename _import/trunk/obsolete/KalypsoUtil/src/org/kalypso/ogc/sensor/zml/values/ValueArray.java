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
public class ValueArray implements IZmlValuesLoader, IZmlValues
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
  public IZmlValues load( ) throws SensorException
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
  public void setModel( final ZmlTuppleModel model )
  {
    // nix
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValues#getCount()
   */
  public int getCount()
  {
    return m_values.size();
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValues#getElement(int)
   */
  public Object getElement( final int index )
  {
    return m_values.get( index );
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValues#setElement(int, java.lang.Object)
   */
  public void setElement( final int index, final Object element )
  {
    m_values.set( index, element );
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValues#indexOf(java.lang.Object)
   */
  public int indexOf( final Object obj )
  {
    return m_values.indexOf( obj );
  }
}
